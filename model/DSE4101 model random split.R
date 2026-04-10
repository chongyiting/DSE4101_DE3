# ----------------------------
# A. Settings
# ----------------------------

rm(list=ls())
set.seed(4101)
packages <- c(
  "dplyr",
  "tidyr",
  "visdat",
  "tidyverse",
  "tidyselect",
  "smotefamily",
  "glmnet",
  "MALDIquant",
  "pROC",
  "ggplot2",
  "ranger",
  "randomForest",
  "xgboost",
  "caret"
)
installed <- rownames(installed.packages())

for (pkg in packages) {
  if (!(pkg %in% installed)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}
library(SHAPforxgboost)

# ----------------------------
# C. Methodology 
# ----------------------------



final_data <- read.csv("C:/Users/zhoum/Documents/dse4101 project data/predictor_disaster_data_INITIAL_COPY.csv")



#


predictors <- c(
  # Disaster
  "disaster_subtype",
  "total_damage_adjusted",
  "disaster_year",
  "start_month",
  "end_month",
  "duration",
  "region",
  
  #firm_data
  
  "assets",
  "assets_current",
  "cash_and_equivalents",
  "stockholders_equity",
  "revenue",
  "operating_income_loss",
  "Net.Income",
  "operating_cash_flow",
  "Number.of.employees",
  "Cash.flow...Operating.revenue",
  "Net.inventory",
  "EBITDA.margin",
  "Liquidity.ratio",
  "Net.debt",
  "Total.cash...short.term.investments",
  "pre_shock_price",
  "GDPCA",
  "inflation"
  
  
)

data=final_data


#can adjust later for different outcomes
#outcome= "rec_1d"

outcome ="rec_7d"

#outcome="rec_30d"

#outcome="rec_180d"




##---------------------------
## Main Running Function
##---------------------------

#For testing and converting to Python purposes, don't uncomment the function
#For actual final run, uncomment the function


model <- function(data, outcome, predictors) {

  # 1. Drop non-feature variables (e.g. id, name)
  model_df <- data %>%
    select(all_of(c(outcome, predictors))) %>%
    mutate(across(where(is.character), as.factor)) %>%
    drop_na() %>%
    mutate(y = factor(.data[[outcome]], levels = c(0, 1))) %>%
    select(-all_of(outcome))
  
  # 2. Split data into training and testing
  train_prop <- 0.8
  train_index <- createDataPartition(model_df$y, p = train_prop, list = FALSE)
  train_df <- model_df[train_index, , drop = FALSE]
  test_df  <- model_df[-train_index, , drop = FALSE]
  y_train <- train_df$y
  x_train <- model.matrix(~ . - 1, data = train_df %>% select(-y)) %>%
    as.data.frame()
  
  y_test <- test_df$y
  x_test <- model.matrix(~ . - 1, data = test_df %>% select(-y)) %>%
    as.data.frame()
  
  # 3. Apply SMOTE
  minority_n <- min(table(train_df$y))
  K_use <- max(1, min(5, minority_n - 1)) #There is a chance where minority class is too small to find K neighbors, so we take the min here
  
  smoted <- SMOTE(X = x_train, target = y_train, K = K_use)
  smoted_data <- as.data.frame(smoted$data)
  names(smoted_data)[ncol(smoted_data)] <- "y"
  #print(smoted_data)
  smoted_data$y <- factor(smoted_data$y, levels = levels(train_df$y))
  y_smoted <- factor(smoted_data$y, levels = levels(train_df$y))
  x_smoted <- smoted_data[, -ncol(smoted_data), drop = FALSE]
  
  # 4. Prepare RFE for Random Forest/XGBoost
  y_smoted_rfe <- factor(ifelse(y_smoted == "1", "yes", "no"), levels = c("no", "yes"))
  rfe_sizes <- seq_len(ncol(x_smoted))
  rfe_train_ctrl <- caret::trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = caret::twoClassSummary
  )
  
  rank_from_varimp <- function(object) {
    imp <- caret::varImp(object)$importance
    if (is.data.frame(imp)) {
      if ("yes" %in% colnames(imp)) {
        imp <- imp[, "yes", drop = FALSE]
      } else if ("Overall" %in% colnames(imp)) {
        imp <- imp[, "Overall", drop = FALSE]
      } else {
        imp <- imp[, 1, drop = FALSE]
      }
      
      out <- data.frame(
        var = rownames(imp),
        Overall = imp[, 1],
        row.names = NULL
      )
    } else {
      out <- data.frame(
        var = names(imp),
        Overall = as.numeric(imp),
        row.names = NULL
      )
    }
    out[order(out$Overall, decreasing = TRUE), , drop = FALSE]
  }
  
  
  ## ----------------
  ## II. Train Model 
  ## ----------------
  
  ## =========================
  ## 1. Multi-split LASSO (from Costa and Baker, 2021)
  ## =========================

  # Define some placeholder vars/matrices
  var.names <- colnames(x_smoted)
  VIM <- data.frame("Variables" = var.names, "VIM" = 0)
  p.final <- data.frame("Variables" = var.names)
  e.final <- data.frame("Variables" = var.names)
  s.final <- data.frame("Variables" = var.names)
  
  B <- 10 #Number of splits
  for (i in 1:B) {
    beta.lasso <- data.frame("Variables" = var.names, "Multiplier" = 0)
    raw <- data.frame("Variables" = var.names, "p.raw" = 1, "estimate.raw" = 0, "error.raw" = 0)
    
    # 1. randomly split the data into screening and cleaning sets
    screen.rows <- sample(seq_len(nrow(smoted_data)), floor(0.5 * nrow(smoted_data)))
    screen <- smoted_data[screen.rows, ]
    x.screen <- data.matrix(screen[, -ncol(screen)])
    y.screen <- screen[, ncol(screen)]
    
    # 2. Use LASSO on the screening set
    fit <- cv.glmnet(
      x.screen, y.screen,
      alpha = 1,
      type.measure = "deviance",
      nfolds = 10,
      family = "binomial"
    )
    
    min.lambda <- fit$lambda.min
    best.lambda <- fit$lambda.1se
    
  
    tmp_coeffs <- coef(fit, s = best.lambda)
    
    coef_matrix <- as.matrix(tmp_coeffs)
    
    coefs <- data.frame(
      name = rownames(coef_matrix)[coef_matrix[, 1] != 0],
      coefficient = coef_matrix[coef_matrix[, 1] != 0, 1]
    )
    
    coefs <- coefs[coefs$name != "(Intercept)", ]
    
   
    selected_vars <- rownames(coef_matrix)[coef_matrix[, 1] != 0]
    selected_vars <- setdiff(selected_vars, "(Intercept)")
    

    beta.lasso$Multiplier <- ifelse(beta.lasso$Variables %in% selected_vars, 1, 0)
    
  
    
    clean.all <- smoted_data[-screen.rows, ]
    
    # keep only valid matching columns
    selected_vars <- intersect(selected_vars, colnames(clean.all))
    
    clean <- clean.all[, selected_vars, drop = FALSE]

    if (ncol(clean) < 2) {
      clean <- clean.all
    }
    
    df.clean <- as.data.frame(clean)
    y.clean <- y_smoted[-screen.rows]
    
    df.clean$y <- y.clean
    
    # Fit a logistic regression model using Maximum Likelihood Estimator with the selected features
    mle.fit <- glm(y ~ ., data = df.clean, family = binomial)
    summary_coef <- summary(mle.fit)$coefficients
    
    # Obtain the raw p-values for the predictors selected with the MLE
    for (j in 1:nrow(raw)) {
      for (k in 1:ncol(df.clean)) {
        if (raw[j, 1] == colnames(df.clean)[[k]]) {
          for (l in 1:nrow(summary_coef)) {
            if (rownames(summary_coef)[l] == colnames(df.clean)[[k]]) {
              raw[j, 2] <- summary_coef[l, 4]
              raw[j, 3] <- summary_coef[l, 1]
              raw[j, 4] <- summary_coef[l, 2]
            }
          }
        }
      }
    }
    
    # Calculate the final p-value (For selecting the final set of predictors)
    p.corr <- c()
    for (j in 1:nrow(raw)) {
      p.corr <- c(p.corr, min(raw[j, 2] * (ncol(clean) - 1), 1))
    }
    
    p.final <- cbind(p.final, p.corr)
    e.final <- cbind(e.final, raw[, 3])
    s.final <- cbind(s.final, raw[, 4])
    
    # calculate the VIM
    for (j in 1:nrow(coefs)) {
      for (k in 1:nrow(VIM)) {
        if (coefs[j, 1] == VIM[k, 1]) {
          VIM[k, 2] <- VIM[k, 2] + 1 / B
        }
      }
    }
  }
  
  # Calculate the p-values, estimates, and standard errors
  df.summary <- data.frame("Variables" = var.names, "p" = 1)
  
  for (i in 1:nrow(p.final)) {
    p.sorted <- sort(t(p.final[i, -1]), decreasing = FALSE)
    deltas <- seq(0.05, 1, 0.01)
    quantiles <- quantile(p.sorted, probs = deltas)
    quantiles.df <- data.frame(
      "d" = as.numeric(deltas),
      "q" = as.numeric(as.matrix(quantiles)),
      "q.d" = as.numeric(as.matrix(quantiles)) / as.numeric(deltas)
    )
    
    min.delta <- quantiles.df[match(min(quantiles.df$q.d), quantiles.df$q.d), 1]
    q.min.delta <- quantiles.df[match(min(quantiles.df$q.d), quantiles.df$q.d), 2]
    
    summ.delta <- min(4 * q.min.delta / min.delta, 1)
    summ.index <- match.closest(summ.delta, p.sorted, tolerance = Inf, nomatch = 0) + 1
    p.summ <- p.final[[i, summ.index]]
    e.summ <- e.final[[i, summ.index]]
    s.summ <- s.final[[i, summ.index]]
    df.summary$p[i] <- p.summ
  }
  
  # Results from the multi-split algorithm
  results <- cbind(VIM, df.summary[, -1])
  names(results)[2] <- "VIM"
  names(results)[3] <- "pValue"
  results$Code <- 0
  results$Check <- 0
  
  for (i in 1:nrow(results)) {
    results$Check[i] <- 0
    if (results[i, 2] > 0.75 & results[i, 3] < 0.05) {
      results$Check[i] <- 1
    }
  }
  
  # final selected variables store
  multisplit.final.vars <- results$Variables[results$Check == 1]

  ## =========================
  ## 2. Random Forest + RFE
  ## =========================
  
  #For simple testing: uncomment the commented lines just to make stuff faster
  
  rfe_sizes  <- c( 10, 20, 30)

  
  rf_rfe_funcs <- caret::caretFuncs
  rf_rfe_funcs$fit <- function(x, y, first, last, ...) {
    x <- as.data.frame(x)
    p <- ncol(x)
    mtry_vals <- unique(pmax(1, pmin(p, c(floor(sqrt(p)), floor(p / 2), p))))
    rf_grid <- expand.grid(
      mtry = mtry_vals,
      splitrule = "gini",
      min.node.size = 1
    )
    # RFE using Random Forests
    caret::train(
      x = x,
      y = y,
      method = "ranger",
      metric = "ROC",
      num.trees = 100,
      trControl = rfe_train_ctrl,
      tuneGrid = rf_grid,
      importance = "impurity"
    )
  }
  rf_rfe_funcs$rank <- function(object, x, y) rank_from_varimp(object)
  
  rf_rfe_ctrl <- rfeControl(
    functions = rf_rfe_funcs,
    method = "cv",
    number = 3,
    verbose=TRUE
  )
  
  rf_rfe <- rfe(
    x = x_smoted,
    y = y_smoted_rfe,
    sizes = rfe_sizes,
    rfeControl = rf_rfe_ctrl
    
  )
  
  rf_selected_vars <- rf_rfe$optVariables
  if (is.null(rf_selected_vars) || length(rf_selected_vars) == 0) {
    rf_selected_vars <- colnames(x_smoted)
  }
  
  rf.fit <- randomForest(
    x = x_smoted[, rf_selected_vars, drop = FALSE],
    y = y_smoted,
    importance = TRUE
  )
  
  rf.prob <- predict(
    rf.fit,
    newdata = x_test[, rf_selected_vars, drop = FALSE],
    type = "prob"
  )
  
  

  
  
  ## =========================
  ## 3. XGBoost + RFE
  ## =========================
  
  
  # can change nrounds or cv or rfe_sizes to make faster training
  
  rfe_sizes  <- c( 10, 20, 30)
  
  
  xgb_rfe_funcs <- caret::caretFuncs
  xgb_rfe_funcs$fit <- function(x, y, first, last, ...) {
    x <- as.data.frame(x)
    
    xgb_grid <- expand.grid(
      nrounds = 100,
      max_depth = 3,
      eta = 0.05,
      gamma = 0,
      colsample_bytree = 0.8,
      min_child_weight = 1,
      subsample = 0.8
    )
    # RFE using xgboost
    caret::train(
      x = x,
      y = y,
      method = "xgbTree",
      metric = "ROC",
      trControl = rfe_train_ctrl,
      tuneGrid = xgb_grid
    )
  }
  xgb_rfe_funcs$rank <- function(object, x, y) rank_from_varimp(object)
  
  xgb_rfe_ctrl <- rfeControl(
    functions = xgb_rfe_funcs,
    method = "cv",
    number = 3,
    verbose=TRUE
  )
  
  xgb_rfe <- rfe(
    x = x_smoted,
    y = y_smoted_rfe,
    sizes = rfe_sizes,
    rfeControl = xgb_rfe_ctrl,
    
  )
  
  xgb_selected_vars <- xgb_rfe$optVariables
  if (is.null(xgb_selected_vars) || length(xgb_selected_vars) == 0) {
    xgb_selected_vars <- colnames(x_smoted)
  }
  
  dtrain <- xgb.DMatrix(
    data = as.matrix(x_smoted[, xgb_selected_vars, drop = FALSE]),
    label = as.numeric(as.character(y_smoted))
  )
  
  dtest <- xgb.DMatrix(
    data = as.matrix(x_test[, xgb_selected_vars, drop = FALSE]),
    label = as.numeric(as.character(y_test))
  )
  # Fit the models
  xgb.fit <- xgb.train(
    params = list(
      objective = "binary:logistic",
      eval_metric = "auc",
      eta = 0.05,
      max_depth = 6,
      subsample = 0.8,
      colsample_bytree = 0.8
    ),
    data = dtrain,
    nrounds = 200,
    verbose = 0
  )
 
 
  ## ----------------
  ## III. Testing Model 
  ## ----------------
  
  ## 1. Multi-split LASSO using Logistic Regression
  if (length(multisplit.final.vars) > 0) {
    mle_train <- smoted_data[, c(multisplit.final.vars, "y"), drop = FALSE]
    mle_test  <- x_test[, multisplit.final.vars, drop = FALSE]
    
    multisplit.mle <- glm(y ~ ., data = mle_train, family = binomial)
    pred_multisplit <- predict(multisplit.mle, newdata = mle_test, type = "response")
  } else {
    mle_train <- data.frame(y = smoted_data$y)
    multisplit.mle <- glm(y ~ 1, data = mle_train, family = binomial)
    pred_multisplit <- rep(
      as.numeric(predict(multisplit.mle, type = "response")[1]),
      nrow(x_test)
    )
  }
  
  ## 2. Random Forest with RFE
  pred_rf <- rf.prob[, which(colnames(rf.prob) == "1")]
  
  ## 3. XGBoost with RFE
  pred_xgb <- predict(xgb.fit, newdata = dtest)
  
  ## ----------------
  ## IV. ROC Curves 
  ## ----------------
  ## =========================
  ## 4. ROC evaluation
  ## =========================
  roc_multisplit <- roc(
    response = y_test,
    predictor = pred_multisplit,
    levels = c("0", "1"),
    direction = "<",
    quiet = TRUE
  )
  
  roc_rf <- roc(
    response = y_test,
    predictor = pred_rf,
    levels = c("0", "1"),
    direction = "<",
    quiet = TRUE
  )
  
  roc_xgb <- roc(
    response = y_test,
    predictor = pred_xgb,
    levels = c("0", "1"),
    direction = "<",
    quiet = TRUE
  )
  
  auc_table <- data.frame(
    Model = c("Multi-split LASSO + MLE", "Random Forest", "XGBoost"),
    AUC = c(
      as.numeric(auc(roc_multisplit)),
      as.numeric(auc(roc_rf)),
      as.numeric(auc(roc_xgb))
    )
  )
  
  roc_list <- list(
    roc_multisplit,
    roc_rf,
    roc_xgb
  )
  
  names(roc_list) <- c(
    paste0(
      "Multi-split LASSO + MLE (AUC = ",
      round(as.numeric(auc(roc_multisplit)), 3), ")"
    ),
    paste0(
      "Random Forest (AUC = ",
      round(as.numeric(auc(roc_rf)), 3), ")"
    ),
    paste0(
      "XGBoost (AUC = ",
      round(as.numeric(auc(roc_xgb)), 3), ")"
    )
  )
  
  roc_plot <- ggroc(
    roc_list,
    legacy.axes = TRUE,
    linewidth = 1
  ) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(
      title = ("ROC Curve Comparison"),
      x = "False Positive Rate",
      y = "True Positive Rate",
      color = "Model"
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_minimal()
  
  ## ----------------
  ## V. Feature Tables 
  ## ----------------
  if (length(multisplit.final.vars) > 0) {
    multisplit_feature_table <- data.frame(
      Model = "Multi-split LASSO + MLE",
      Feature = multisplit.final.vars,
      Score = results$VIM[match(multisplit.final.vars, results$Variables)],
      pValue = results$pValue[match(multisplit.final.vars, results$Variables)],
      stringsAsFactors = FALSE
    )
  } else {
    multisplit_feature_table <- data.frame(
      Model = character(0),
      Feature = character(0),
      Score = numeric(0),
      pValue = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  
#RF Importance
  
  rf_imp <- importance(rf.fit)
  
  rf_accuracy <- rf_imp[, "MeanDecreaseAccuracy"]
  rf_gini <- rf_imp[, "MeanDecreaseGini"]
  
  rf_rank_accuracy <- sort(rf_accuracy, decreasing = TRUE)
  rf_rank_gini <- sort(rf_gini, decreasing = TRUE)
  
  head(rf_rank_accuracy, 10)
  head(rf_rank_gini, 10)
  
  rf_feature_table <- data.frame(
    Model = "Random Forest",
    Feature = rownames(rf_imp),
    MeanDecreaseAccuracy = rf_accuracy,
    MeanDecreaseGini = rf_gini,
    stringsAsFactors = FALSE
  )
  
# can use meandecreasaccuracy too
  
  rf_feature_table <- rf_feature_table[order(-rf_feature_table$MeanDecreaseGini), ]
  
  head(rf_feature_table, 10)
  
  
  #XGB Importance
  

  
  xgb_imp <- xgb.importance(
    feature_names = xgb_selected_vars,
    model = xgb.fit
  )
  
  all_features <- data.frame(Feature = xgb_selected_vars, stringsAsFactors = FALSE)
  
  if (nrow(xgb_imp) > 0) {
    xgb_feature_table <- data.frame(
      Model = "XGBoost",
      Feature = xgb_imp$Feature,
      Score = xgb_imp$Gain,
      Cover = xgb_imp$Cover,
      Frequency = xgb_imp$Frequency,
      pValue = NA_real_,
      stringsAsFactors = FALSE
    )
    
    xgb_feature_table_full <- merge(
      all_features,
      xgb_feature_table,
      by = "Feature",
      all.x = TRUE
    )
    
    xgb_feature_table_full$Score[is.na(xgb_feature_table_full$Score)] <- 0
    xgb_feature_table_full$Cover[is.na(xgb_feature_table_full$Cover)] <- 0
    xgb_feature_table_full$Frequency[is.na(xgb_feature_table_full$Frequency)] <- 0
    xgb_feature_table_full$Model <- "XGBoost"
    
    xgb_feature_table <- xgb_feature_table_full[
      order(-xgb_feature_table_full$Score), 
    ]
    
    head(xgb_feature_table, 10)
    
  } else {
    xgb_feature_table <- data.frame(
      Model = character(0),
      Feature = character(0),
      Score = numeric(0),
      pValue = numeric(0),
      stringsAsFactors = FALSE
    )
    
    
  }
  
  X_train <- as.matrix(x_smoted[, xgb_selected_vars, drop = FALSE])
  y_train <- as.numeric(as.character(y_smoted))
  shap_values <- shap.values(xgb_model = xgb.fit, X_train = X_train)
  
  
  shap_importance <- shap_values$mean_shap_score
  
  shap_df <- data.frame(
    Feature = names(shap_importance),
    SHAP = shap_importance
  )
  
  
  shap_df <- data.frame(
    Model = "XGBoost_SHAP",
    Feature = names(shap_importance),
    SHAP = as.numeric(shap_importance),
    stringsAsFactors = FALSE
  )
  
  shap_df <- shap_df[order(shap_df$SHAP, decreasing = TRUE), ]
  head(shap_df, 10) 
  
  feature_table <- bind_rows(
    multisplit_feature_table,
    rf_feature_table,
    xgb_feature_table,
    shap_df
  )
  
  prediction_table <- data.frame(
    y_true = y_test,
    pred_multisplit = pred_multisplit,
    pred_rf = pred_rf,
    pred_xgb = pred_xgb
  )
  
  


  
  
  
  return(list(
    train_df = train_df,
    test_df = test_df,
    x_smoted = x_smoted,
    x_test = x_test,
    y_smoted = y_smoted,
    y_test = y_test,
    
    multisplit_results = results,
    multisplit.final.vars = multisplit.final.vars,
    multisplit.mle = multisplit.mle,
    
    rf_rfe = rf_rfe,
    rf_selected_vars = rf_selected_vars,
    rf.fit = rf.fit,
  
    
    xgb_rfe = xgb_rfe,
    xgb_selected_vars = xgb_selected_vars,
    xgb.fit = xgb.fit,
    
    
    
    
    prediction_table = prediction_table,
    auc_table = auc_table,
    roc_plot = roc_plot,
    feature_table = feature_table
  ))
}
outcome="rec_1d"
out <- model(data, outcome, predictors)
out$multisplit_results
out$multisplit.final.vars
out$rf_selected_vars
out$xgb_selected_vars
out$auc_table
out$feature_table
out$roc_plot
head(out$prediction_table)

feature_table <-out $feature_table
roc_plot < - out $roc_plot

desktop_path <- "~/Desktop"
write.csv(feature_table, file.path(desktop_path, "my_table_1.csv"), row.names = FALSE)




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
  "PRROC",
  "ggplot2",
  "ranger",
  "randomForest",
  "xgboost",
  "caret",
  "fastshap"
)
installed <- rownames(installed.packages())

for (pkg in packages) {
  if (!(pkg %in% installed)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}
library(SHAPforxgboost)
library(foreach)
registerDoSEQ()



# ----------------------------
# C. Methodology 
# ----------------------------


final_data <- read.csv("Desktop/predictor_disaster_data.csv")

na_summary <- data.frame(
  column = names(final_data),
  na_percent = colSums(is.na(final_data)) / nrow(final_data) * 100
)
na_summary

#could drop disaster subtypes or region

predictors <- c(
  # Disaster
  "region",
  "total_damage_adjusted",
  "disaster_year",
  "start_month",
  "end_month",
  "duration",
  
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


final_data <- final_data %>% 
  drop_na()

#can adjust later for different outcomes
outcome= "rec_1d"

#outcome ="rec_7d"
#outcome="rec_30d"
#outcome="rec_180d"


full_df <- data.frame(final_data)

# convert respective cols to date
full_df$start_date <- as.Date(full_df$start_date, format = "%Y/%m/%d")
full_df$end_date <- as.Date(full_df$end_date, format = "%Y/%m/%d")

# split full df to upstream & downstream df
upstream_cutoff <- 1.9548 # 1.954804292
upstream_df <- full_df[full_df$upstream >= upstream_cutoff, ]
downstream_df <- full_df[full_df$upstream < upstream_cutoff, ]

# helper function to prepare data
prepare_data <- function(data, outcome, predictors) {
  
  df <- data %>%
    select(all_of(c(outcome, predictors))) %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(y = factor(.data[[outcome]], levels = c(0, 1))) %>%
    select(-all_of(outcome))
  
  y <- df$y
  
  x <- model.matrix(~ . - 1, data = df %>% select(-y)) %>%
    as.data.frame()
  
  return(list(x = x, y = y))
}

# helper function to do train test split by splitting event 80/20 in chronological order
# observations from the earlier 80% events as train, later 20% test
train_test_split <- function(full_df) {
  full_df_sorted <- full_df %>%
    arrange(start_date, event_name)
  n <- n_distinct(full_df_sorted$event_name)
  n_train <- floor(0.8 * n)
  
  event_order <- full_df_sorted %>%
    distinct(event_name)
  train_events <- event_order$event_name[1:n_train]
  
  train_df <- full_df_sorted %>%
    filter(event_name %in% train_events)
  
  test_df <- full_df_sorted %>%
    filter(!(event_name %in% train_events))
  
  return(list(train_df = train_df, test_df = test_df))
}



##---------------------------
## Main Running Function
##---------------------------

model <- function(data, outcome, predictors) {
  #call train test split function
  res <- train_test_split(data)
  train_df <- res$train_df
  test_df <- res$test_df

  train_processed <- prepare_data(train_df,outcome , predictors)
  test_processed  <- prepare_data(test_df, outcome, predictors)
  
  x_train <- train_processed$x
  y_train <- train_processed$y
  
  x_test <- test_processed$x
  y_test <- test_processed$y
  
  #helper function for calculating prauc
  calc_pr_auc <- function(y_true, pred_prob) {
    y_num <- as.numeric(as.character(y_true))
    
    fg <- pred_prob[y_num == 1]
    bg <- pred_prob[y_num == 0]
    
    pr <- PRROC::pr.curve(
      scores.class0 = fg,
      scores.class1 = bg,
      curve = TRUE
    )
    
    return(pr$auc.integral)
  }
  
  
  # 3. Apply SMOTE
  minority_n <- min(table(y_train))
  K_use <- max(1, min(5, minority_n - 1)) 
  #There is a chance where minority class is too small to find K neighbors, so we take the min here
  
  smoted <- SMOTE(X = x_train, target = y_train, K = K_use)
  smoted_data <- as.data.frame(smoted$data)
  names(smoted_data)[ncol(smoted_data)] <- "y"
  #print(smoted_data)
  smoted_data$y <- factor(smoted_data$y, levels = levels(y_train))
  y_smoted <- factor(smoted_data$y, levels = levels(y_train))
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
    coefs <- data.frame(
      name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1],
      coefficient = tmp_coeffs@x
    )
    
    for (k in 1:nrow(beta.lasso)) {
      for (j in 1:nrow(coefs)) {
        if (coefs[j, 1] == beta.lasso[k, 1]) {
          beta.lasso[k, 2] <- 1
        }
      }
    }
    
    clean.all <- smoted_data[-screen.rows, ]
    clean <- clean.all
    # Get all coefficient with Beta > 0
    for (j in 1:nrow(beta.lasso)) {
      for (k in 1:ncol(clean.all)) {
        if (beta.lasso[j, 1] == colnames(clean.all)[[k]] & beta.lasso[j, 2] == 0.0) {
          index <- grep(paste0("^", colnames(clean.all)[[k]], "$"), colnames(clean))
          clean <- clean[, -index, drop = FALSE]
        }
      }
    }
    
    df.clean <- as.data.frame(clean)
    
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
  
  rfe_sizes  <- c(5, 10, 15, 20, 25, 30)

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
      num.trees = 500, #number of trees
      trControl = rfe_train_ctrl,
      tuneGrid = rf_grid,
      importance = "impurity"
    )
  }
  rf_rfe_funcs$rank <- function(object, x, y) rank_from_varimp(object)
  
  rf_rfe_ctrl <- rfeControl(
    functions = rf_rfe_funcs,
    method = "cv",
    number = 5,
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
  
  xgb_rfe_funcs <- caret::caretFuncs
  xgb_rfe_funcs$fit <- function(x, y, first, last, ...) {
    x <- as.data.frame(x)
    
    xgb_grid <- expand.grid(
      nrounds = 500, #nrounds for xgboost
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
    number = 5,
    verbose=TRUE
  )
  
  xgb_rfe <- rfe(
    x = x_smoted,
    y = y_smoted_rfe,
    sizes = rfe_sizes,
    rfeControl = xgb_rfe_ctrl
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
    nrounds = 500, #nroudns of xgboost
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
  
  #added PR-AUC
  pr_auc_multisplit <- calc_pr_auc(y_test, pred_multisplit)
  pr_auc_rf <- calc_pr_auc(y_test, pred_rf)
  pr_auc_xgb <- calc_pr_auc(y_test, pred_xgb)
  
  auc_table <- data.frame(
    Model = c("Multi-split LASSO + MLE", "Random Forest", "XGBoost"),
    ROC_AUC = c(
      as.numeric(auc(roc_multisplit)),
      as.numeric(auc(roc_rf)),
      as.numeric(auc(roc_xgb))
    ),
    PR_AUC = c(
      pr_auc_multisplit,
      pr_auc_rf,
      pr_auc_xgb
    )
  )
  
  roc_list <- list(
    roc_multisplit,
    roc_rf,
    roc_xgb
  )
  
  names(roc_list) <- c(
    paste0(
      "Multi-split LASSO + MLE (ROC-AUC = ",
      round(as.numeric(auc(roc_multisplit)), 3), ")"
    ),
    paste0(
      "Random Forest (ROC-AUC = ",
      round(as.numeric(auc(roc_rf)), 3), ")"
    ),
    paste0(
      "XGBoost (ROC-AUC = ",
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
      title = paste("ROC Curve Comparison -", outcome),
      x = "False Positive Rate",
      y = "True Positive Rate",
      color = "Model"
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_minimal()
  
  
  ## ----------------
  ## V. Feature Tables 
  ## ----------------
  
  ## 1. Multi-split LASSO feature table
  if (length(multisplit.final.vars) > 0) {
    multisplit_feature_table <- data.frame(
      Model = "Multi-split LASSO + MLE",
      Feature = multisplit.final.vars,
      VIM = results$VIM[match(multisplit.final.vars, results$Variables)], #VIM score
      pValue = results$pValue[match(multisplit.final.vars, results$Variables)],
      stringsAsFactors = FALSE
    ) %>%
      arrange(desc(VIM), pValue) %>%
      mutate(MultiSplit_Rank = row_number()) #arrange by rank desc, assign rank
  } else {
    multisplit_feature_table <- data.frame(
      Model = character(0),
      Feature = character(0),
      VIM = numeric(0),
      pValue = numeric(0),
      MultiSplit_Rank = integer(0), #added rank number
      stringsAsFactors = FALSE
    )
  }

  
  #2. RF Importance, SHAP - fastshap package used
  
  rf_pred_wrapper <- function(object, newdata) {
    predict(object, newdata = newdata, type = "prob")[, "1"]
  }
  
  rf_shap_values <- fastshap::explain(
    object = rf.fit,
    feature_names = rf_selected_vars,
    X = x_smoted[, rf_selected_vars, drop = FALSE],
    pred_wrapper = rf_pred_wrapper,
    nsim = 30, #100 is good for final round, 30 or 50 for testing
    adjust = TRUE
  )
  
  rf_shap_importance <- colMeans(abs(rf_shap_values))
  
  shap_rf_df <- data.frame(
    Model = "Random Forest",
    Feature = names(rf_shap_importance),
    RF_SHAP = as.numeric(rf_shap_importance),
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(RF_SHAP)) %>%
    mutate(RF_SHAP_Rank = row_number()) # arrange in the same way as compared to multisplit's vim score, but shap
  
  
  #3. XGB Importance - SHAP 

  X_train_shap <- as.matrix(x_smoted[, xgb_selected_vars, drop = FALSE])
  
  shap_xgb_values <- shap.values(xgb_model = xgb.fit, X_train = X_train_shap)
  
  shap_xgb_df <- data.frame(
    Model = "XGBoost",
    Feature = names(shap_xgb_values$mean_shap_score),
    XGB_SHAP = as.numeric(shap_xgb_values$mean_shap_score),
    stringsAsFactors = FALSE
    ) %>%
      arrange(desc(XGB_SHAP)) %>%
      mutate(XGB_SHAP_Rank = row_number())

  #head(shap_df, 10) 
  
  
  #4. Comparison table across 3 models
  all_features <- unique(c(
    multisplit_feature_table$Feature,
    shap_rf_df$Feature,
    shap_xgb_df$Feature
  ))
  
  feature_table <- data.frame(
    Feature = all_features,
    stringsAsFactors = FALSE
  ) %>%
    left_join(
      multisplit_feature_table %>%
        select(Feature, VIM, pValue, MultiSplit_Rank) %>%
        mutate(MultiSplit_Selected = 1),
      by = "Feature"
    ) %>%
    left_join(
      shap_rf_df %>%
        select(Feature, RF_SHAP, RF_SHAP_Rank),
      by = "Feature"
    ) %>%
    left_join(
      shap_xgb_df %>%
        select(Feature, XGB_SHAP, XGB_SHAP_Rank),
      by = "Feature"
    ) %>%
    mutate(
      MultiSplit_Selected = ifelse(is.na(MultiSplit_Selected), 0, MultiSplit_Selected)
    ) %>%
    arrange(XGB_SHAP_Rank, RF_SHAP_Rank, MultiSplit_Rank)
  
  # VI. Return results
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


    auc_table = auc_table,
    roc_plot = roc_plot,
    
    multisplit_feature_table = multisplit_feature_table,
    shap_rf_df = shap_rf_df,
    shap_xgb_df = shap_xgb_df,
    feature_table = feature_table
  ))
}

# VII. save results helper

save_model_results <- function(out, outcome, save_dir = "../results") {
  dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(save_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(save_dir, "plots"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(save_dir, "rds"), showWarnings = FALSE, recursive = TRUE)
  
  # save metrics
  write.csv(
    out$auc_table,
    file.path(save_dir, "tables", paste0("auc_pr_table_", outcome, ".csv")),
    row.names = FALSE
  )
  
  # save feature tables
  write.csv(
    out$multisplit_feature_table,
    file.path(save_dir, "tables", paste0("multisplit_features_", outcome, ".csv")),
    row.names = FALSE
  )
  
  write.csv(
    out$shap_rf_df,
    file.path(save_dir, "tables", paste0("rf_shap_features_", outcome, ".csv")),
    row.names = FALSE
  )
  
  write.csv(
    out$shap_xgb_df,
    file.path(save_dir, "tables", paste0("xgb_shap_features_", outcome, ".csv")),
    row.names = FALSE
  )
  
  write.csv(
    out$feature_table,
    file.path(save_dir, "tables", paste0("feature_comparison_", outcome, ".csv")),
    row.names = FALSE
  )
  
  # save ROC plot
  ggsave(
    filename = file.path(save_dir, "plots", paste0("roc_plot_", outcome, ".png")),
    plot = out$roc_plot,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  # save full object
  saveRDS(
    out,
    file.path(save_dir, "rds", paste0("model_output_", outcome, ".rds"))
  )
}


## VIII. Run once


#out <- model(full_df, outcome, predictors) 
#full_df is cleaner than final_data with the date type conversion, but not much diff

#out$multisplit_results
#out$multisplit.final.vars
#out$rf_selected_vars
#out$xgb_selected_vars
#out$auc_table
#out$multisplit_feature_table
#out$shap_rf_df
#out$shap_xgb_df
#out$feature_table
#out$roc_plot

#save_model_results(out, outcome)



# IX. Run all outcomes
outcomes <- c("rec_1d", "rec_7d", "rec_30d", "rec_180d")

dataset_list <- list(
  #full = full_df,  
  upstream = upstream_df,
  downstream = downstream_df
)

all_results <- list()

for (dataset_name in names(dataset_list)) {
  df_use <- dataset_list[[dataset_name]]
  
  all_results[[dataset_name]] <- lapply(outcomes, function(y) {
    out <- model(df_use, y, predictors)
    
    # save with dataset name included
    save_model_results(out, paste0(dataset_name, "_", y))
    
    list(
      dataset = dataset_name,
      outcome = y,
      auc_table = out$auc_table,
      multisplit_feature_table = out$multisplit_feature_table,
      shap_rf_df = out$shap_rf_df,
      shap_xgb_df = out$shap_xgb_df,
      feature_table = out$feature_table
    )
  })
  
  names(all_results[[dataset_name]]) <- outcomes
}

saveRDS(all_results, "../results/rds/all_results_by_dataset.rds")

all_results


# ----------------------------
# Simulation
# ----------------------------
simulate_recovery_data <- function(data,
                                   predictors,
                                   event_col = "event_name",
                                   n_events = NULL,
                                   sample_events_randomly = TRUE,
                                   baseline_days = 90,
                                   noise_sd = 8,
                                   max_recovery_days = 365,
                                   zero_share = 0.05,
                                   over_180_share = 0.08) {
  
  sim <- data
  # subset rows by number of events
  if (!is.null(n_events)) {
    events <- unique(sim[[event_col]])
    n_events <- min(n_events, length(events))
    
    chosen_events <- if (sample_events_randomly) {
      sample(events, n_events)
    } else {
      events[seq_len(n_events)]
    }
    
    sim <- sim[sim[[event_col]] %in% chosen_events, , drop = FALSE]
  }
  
  
  if ("start_date" %in% names(sim) && !"start_month" %in% names(sim)) {
    sim$start_month <- as.integer(format(as.Date(sim$start_date), "%m"))
  }
  
  if ("end_date" %in% names(sim) && !"end_month" %in% names(sim)) {
    sim$end_month <- as.integer(format(as.Date(sim$end_date), "%m"))
  }
  
  if ("start_date" %in% names(sim) && !"disaster_year" %in% names(sim)) {
    sim$disaster_year <- as.integer(format(as.Date(sim$start_date), "%Y"))
  }
  
  if (all(c("start_date", "end_date") %in% names(sim)) && !"duration" %in% names(sim)) {
    sim$duration <- as.numeric(as.Date(sim$end_date) - as.Date(sim$start_date))
  }
  
  if ("region" %in% names(sim)) {
    sim$region_num <- as.numeric(as.factor(sim$region))
  }
  
  # variables used in simulation
  use_vars <- predictors
  if ("region" %in% predictors) {
    use_vars <- setdiff(use_vars, "region")
    use_vars <- c(use_vars, "region_num")
  }
  
  num_vars <- use_vars[sapply(sim[, use_vars, drop = FALSE], is.numeric)]
  z_df <- as.data.frame(scale(sim[, num_vars, drop = FALSE]))
  z_df[is.na(z_df)] <- 0
  
  get_z <- function(x) {
    if (x %in% names(z_df)) z_df[[x]] else rep(0, nrow(sim))
  }
  
  # simulate recovery days
  recovery_days_raw <-
    baseline_days +
    10 * get_z("total_damage_adjusted") +
    10 * get_z("duration") +
    7  * get_z("Net.debt") -
    8  * get_z("Liquidity.ratio") -
    7  * get_z("cash_and_equivalents") -
    6  * get_z("operating_cash_flow") -
    5  * get_z("EBITDA.margin") -
    7  * get_z("Net.Income") -
    3  * get_z("pre_shock_price") +
    2  * get_z("inflation") -
    2  * get_z("GDPCA") +
    2  * get_z("region_num") +
    7  * get_z("total_damage_adjusted") * get_z("duration") -
    4  * get_z("Liquidity.ratio") * get_z("Net.debt") +
    rnorm(nrow(sim), 0, noise_sd)
  
  recovery_days <- round(recovery_days_raw)
  recovery_days <- pmax(0, pmin(max_recovery_days, recovery_days))
  
  n <- nrow(sim)
  n_zero <- max(1, round(zero_share * n))
  n_over_180 <- max(1, round(over_180_share * n))
  
  if ((n_zero + n_over_180) >= n) {
    stop("zero + over_180 is too many")
  }
  
  ord_low <- order(recovery_days)
  ord_high <- order(recovery_days, decreasing = TRUE)
  
  # force some immediate recovery
  zero_idx <- ord_low[seq_len(n_zero)]
  recovery_days[zero_idx] <- 0
  
  # force some >180-day recovery from the highest remaining values
  remaining_high <- ord_high[!(ord_high %in% zero_idx)]
  over_180_idx <- remaining_high[seq_len(n_over_180)]
  recovery_days[over_180_idx] <- sample(181:max_recovery_days, length(over_180_idx), replace = TRUE)
  
  # cap all other non-zero observations at 180
  other_idx <- setdiff(seq_len(n), c(zero_idx, over_180_idx))
  recovery_days[other_idx] <- pmin(recovery_days[other_idx], 180)
  
  sim$recovery_days <- recovery_days
  sim$rec_1d   <- as.integer(sim$recovery_days <= 1)
  sim$rec_7d   <- as.integer(sim$recovery_days <= 7)
  sim$rec_30d  <- as.integer(sim$recovery_days <= 30)
  sim$rec_180d <- as.integer(sim$recovery_days <= 180)
  
  if ("region_num" %in% names(sim)) {
    sim$region_num <- NULL
  }
  sim
}

sim_data <- simulate_recovery_data(data = final_data, predictors = predictors,n_events = 50)

#can adjust later for different outcomes
outcome= "rec_1d"

## Run simulation
outcome1 = "rec_1d"
out1 <- model(sim_data, outcome1, predictors) 
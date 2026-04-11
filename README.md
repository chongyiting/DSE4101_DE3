# DSE4101_DE3

## Project Overview
This repository contains the code, data, and results for our DSE4101 project on firm resilience after climate-related shock events. The project combines disaster-level, firm-level, and macroeconomic data to construct predictors of recovery, and then applies machine learning models to examine which factors are associated with post-shock recovery outcomes.

A key extension of the analysis is the separation of firms into upstream and downstream groups, allowing us to investigate whether a firm’s position in the supply chain is associated with differences in resilience and recovery patterns.

## Repository Structure

### `data preprocessing code/`
Scripts used to clean, merge, and prepare the raw datasets before modelling.

Main files include:
- `datacode.py` – filters and processes position / sector data and prepares upstream-downstream classification inputs
- `merge.py` – helper script for combining datasets
- `dse4101 initial fixing data.R` – initial data cleaning and fixing
- `dse4101 final merge.R` – final data merging pipeline
- `pull_final_sec_yearly_2013_2025.py` – script for pulling SEC-related yearly firm data
- `recovery_days_cleaning.ipynb` – notebook for preparing recovery-day outcomes
- `dse4101_eda.ipynb` – exploratory data analysis
- `dse4101_phase1_video.ipynb` – supporting notebook used in the earlier project phase

### `data/`
Processed and intermediate datasets used in the project.

Important files include:
- `predictor_disaster_data.csv` – main modelling dataset
- `storm_company_recovery_days.csv` / `.xlsx` – firm recovery outcomes after storm events
- `company_disaster_post2014.csv` – disaster-company matched data
- `final_merged_company_data.xlsx` – merged firm-level dataset
- `final_merged_company_data_with_upstream_downstream.xlsx` – merged dataset with supply-chain position classification
- `position_full.csv` and `position_usa_adb_only.csv` – supply-chain position data
- `firm_data_excl_employee.csv`, `firm_employee_data.csv` – firm characteristics
- `GDPCA.csv`, `inflation.csv` – macroeconomic controls

### `model/`
Scripts for model training and evaluation.

Main files include:
- `DSE4101 model.R` – main modelling pipeline
- `DSE4101 model random split.R` – alternative specification using a random split
- `DSE4101_Added Simulation.R` – additional simulation / robustness analysis

### `results/`
Saved outputs from the analysis, including:
- `plots/`
- `tables/`
- `rds/`
- `random split inspection plot/`

## Methodology Summary
The project constructs a predictor dataset combining:
- disaster characteristics,
- firm financial variables,
- pre-shock stock-related variables,
- and macroeconomic controls.

The main modelling workflow:
1. loads the processed predictor dataset,
2. prepares the outcome variable,
3. splits firms into upstream and downstream groups,
4. applies class balancing with SMOTE where needed,
5. trains machine learning models,
6. and evaluates predictive performance.

The modelling scripts include methods such as:
- LASSO-based variable selection,
- Random Forest,
- and XGBoost-related modelling support.

## Upstream–Downstream Classification
Firms are classified using supply-chain position data derived from upstreamness and downstreamness measures. This allows the analysis to be run separately for upstream and downstream companies so that differences by supply-chain position can be studied directly.

## How to Run

### 1. Preprocess the data
Run the scripts in `data preprocessing code/` to clean and merge the raw data into the final modelling dataset.

Suggested order:
1. initial data cleaning scripts
2. upstream / downstream classification scripts
3. final merge script
4. recovery-day outcome construction

### 2. Run the models
Move to the `model/` folder and run:
- `DSE4101 model.R` for the main specification
- `DSE4101 model random split.R` for the random-split robustness check
- `DSE4101_Added Simulation.R` for additional simulation analysis

### 3. Inspect outputs
Outputs will be stored in the `results/` folder, including plots, tables, and saved R objects.

## Required Packages

### R
The main R scripts use packages including:
- `dplyr`
- `tidyr`
- `tidyverse`
- `smotefamily`
- `glmnet`
- `pROC`
- `PRROC`
- `ggplot2`
- `ranger`
- `randomForest`
- `xgboost`
- `caret`
- `fastshap`
- `SHAPforxgboost`
- `foreach`

### Python
The preprocessing scripts use packages including:
- `pandas`
- `numpy`
- `openpyxl`

## Notes
- File paths in the scripts are currently written as relative paths, so the folder structure should be preserved when running the code.
- Some preprocessing scripts expect local input files to already be available in the working directory.
- Results may differ depending on the chosen outcome horizon and sample split.

## Authors
DSE4101 Group DE3

## Acknowledgements
This repository was developed as part of the DSE4101 course project.

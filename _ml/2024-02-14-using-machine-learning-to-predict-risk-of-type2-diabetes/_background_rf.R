pacman::p_load("tidyverse", #for tidy data science practice
               "tidymodels", "workflows",# for tidy machine learning
               "pacman", #package manager
               "devtools", #developer tools
               "Hmisc", "skimr", "broom", "modelr",#for EDA
               "jtools", "huxtable", "interactions", # for EDA
               "ggthemes", "ggstatsplot", "GGally",
               "scales", "gridExtra", "patchwork", "ggalt", "vip",
               "ggstance", "ggfortify", # for ggplot
               "DT", "plotly", #interactive Data Viz
               # Lets install some ML related packages that will help tidymodels::
               "usemodels", "poissonreg", "agua", "sparklyr", "dials",#load computational engines
               "doParallel", # for parallel processing (speedy computation)
               "ranger", "xgboost", "glmnet", "kknn", "earth", "klaR", "discrim", "naivebayes",#random forest
               "janitor", "lubridate", "haven")

data <- read_csv("diabetes_cleaned_data.csv")

set.seed(2024021403)
data_split_big <-
  data %>% 
  dplyr::sample_frac(size = 0.01, replace = FALSE) %>% #use 15% of data due to lack of computing power
  initial_split(strata = diabete4) # strata by diabete4
data_train_big <-
  data_split_big %>% 
  training()
data_test_big <-
  data_split_big %>% 
  testing()
data_fold_big <-
  data_train_big %>% 
  vfold_cv(v = 10, strata = diabete4)


base_rec <-
  recipes::recipe(formula = diabete4 ~.,
                  data = data_train_big) %>% 
  step_zv(all_predictors())

dummy_rec <-
  base_rec %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_nzv(all_predictors())

# rf
rf_spec <-
  rand_forest() %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

rf_spec_for_tuning <-
  rf_spec %>% 
  set_args(trees = tune(),
           mtry = tune(),
           min_n = tune())

set.seed(2024021403)

doParallel::registerDoParallel(cores = 6)
rf_tuning_set <-
  workflow_set(
    list(dummy_rec),
    list(rf_spec_for_tuning),
    cross = TRUE)
doParallel::registerDoParallel(cores = 6)
rf_tuning_results <-
  workflow_map(rf_tuning_set,
               fn = "tune_grid",
               resamples = data_fold_big,
               grid = 11,
               verbose = TRUE,
               control = control_grid(save_pred = TRUE))

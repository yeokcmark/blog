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
# set.seed(2024021401)
# data_split <-
#   data %>% 
#   #dplyr::sample_frac(size = 0.15, replace = FALSE) %>% #use 15% of data due to lack of computing power
#   initial_split(strata = diabete4) # strata by diabete4
# data_train <-
#   data_split %>% 
#   training()
# data_test <-
#   data_split %>% 
#   testing()
# data_fold <-
#   data_train %>% 
#   vfold_cv(v = 10, strata = diabete4)

set.seed(2024021403)
data_split_big <-
  data %>% 
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
  step_dummy(all_nominal_predictors())

normal_rec <-
  dummy_rec %>% 
  step_normalize(all_predictors())


log_rec <-
  base_rec %>% 
  step_log(all_numeric_predictors())

rf_spec <-
  rand_forest(trees = 1000L) %>% 
  set_engine("ranger",
             importance = "permutation") %>% 
  set_mode("classification")

rf_spec_for_tuning <-
  rf_spec %>% 
  set_args(mtry = tune(),
           min_n = tune())

# Classification Tree Model
ct_spec <- 
  decision_tree() %>%
  set_engine(engine = 'rpart') %>%
  set_mode('classification') 

ct_spec_for_tuning <-
  ct_spec %>% 
  set_args(tree_depth = tune(),
           min_n = tune(), 
           cost_complexity = tune())

# knn
knn_spec <-
  nearest_neighbor() %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

knn_spec_for_tuning <-
  knn_spec %>% 
  set_args(neighbors = tune(),
           weight_func = tune(),
           dist_power = tune())

# xgboost
xgb_spec <-
  boost_tree(trees = 1000L) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

xgb_spec_for_tuning <-
  xgb_spec %>% 
  set_args(tree_depth = tune(),
           min_n = tune(),
           loss_reduction = tune(),
           sample_size = tune(),
           mtry = tune(),
           learn_rate = tune())

# # naive bayes

naive_spec <-
  naive_Bayes() %>%
  set_engine("naivebayes",
             usepoisson = TRUE) %>%
  set_mode("classification")

naive_spec_for_tuning <-
  naive_spec %>% 
  set_args(smoothness = tune(),
           Laplace = tune())

# Logistic Regression Model
logistic_spec <- 
  logistic_reg() %>%
  set_engine(engine = 'glm') %>%
  set_mode('classification') 

# Lasso Logistic Regression Model

logistic_lasso_spec <-
  logistic_reg(mixture = 1, penalty = 1) %>% 
  set_engine(engine = 'glmnet') %>%
  set_mode('classification') 


logistic_lasso_spec_for_tuning <- 
  logistic_lasso_spec %>% 
  set_args(penalty = tune()) #we could let penalty = tune()
# 
# base_set <- #works
#   workflow_set (
#     list(base_rec, dummy_rec, log_rec), #preprocessor
#     list(rf_spec, ct_spec,
#          rf_spec_for_tuning, ct_spec_for_tuning), #model
#     cross = TRUE) #default is cross = TRUE
# 
# dummy_set <- #works
#   workflow_set (
#     list(dummy_rec),
#     list(knn_spec, xgb_spec, logistic_spec,
#          knn_spec_for_tuning, xgb_spec_for_tuning),
#     cross = TRUE)
# 
# normal_set <-
#   workflow_set(
#     list(normal_rec),
#     list(logistic_lasso_spec,
#          logistic_lasso_spec_for_tuning),
#     cross = TRUE)
# 
# naive_set <- #works
#   workflow_set(
#     list(base_rec, log_rec),
#     list(naive_spec,
#          naive_spec_for_tuning),
#     cross = TRUE)
# 
# model_set <-
#   bind_rows(base_set, dummy_set, normal_set, naive_set)
# 
# set.seed(2024021402)
# doParallel::registerDoParallel()
# model_results <-
#   workflow_map(model_set,
#                fn = "tune_grid",
#                resamples = data_fold,
#                grid = 7,
#                verbose = TRUE,
#                control = control_grid(save_pred = TRUE))
set.seed(2024021403)
log_set <-
  workflow_set(
    list(dummy_rec),
    list(logistic_spec),
    cross = TRUE)
doParallel::registerDoParallel(cores = 6)
log_results <-
  workflow_map(log_set,
               fn = "tune_grid",
               resamples = data_fold_big,
               grid = 11,
               verbose = TRUE,
               control = control_grid(save_pred = TRUE))

autoplot(log_results)
log_results %>% collect_metrics()

pacman::p_load("tidyverse", #for tidy data science practice
               "tidymodels", "workflows", "finetune", "themis", "embed",# for tidy machine learning
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
               "ranger", "xgboost", "glmnet", "kknn", "earth", "klaR", "discrim", "naivebayes", "baguette", "kernlab",#random forest
               "janitor", "lubridate")


base_inter_rec <-
  recipes::recipe(formula = is_canceled ~.,
                  data = data_train) %>% 
  step_zv(all_predictors()) %>% # remove zero variance
  # make new features with date
  step_date(arrival_date, features = c("dow", "month", "year"), role = "predictors") %>% 
  update_role(arrival_date, new_role = "date") %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_interact(terms = ~ all_numeric_predictors():all_numeric_predictors(),
                role = "predictor") %>% 
  step_dummy(all_nominal_predictors())

#base_inter_rec %>% prep() %>% juice() %>% skim()

base_inter_rf_wflow <- 
  workflow() %>% 
  add_recipe(base_inter_rec) %>% 
  add_model(rf_spec_for_tuning)

set.seed(2024022802)
doParallel::registerDoParallel(cl=3, cores = 6)
base_inter_rf_results <-
  tune_race_anova(base_inter_rf_wflow,
                  data_fold,
                  grid = c(4:10),
                  metrics = metric_set(roc_auc, f_meas, accuracy, mn_log_loss), 
                  control = control_race(verbose = TRUE, 
                                         verbose_elim = TRUE,
                                         allow_par = TRUE, 
                                         save_pred = TRUE,
                                         parallel_over = "everything")
  )

save(base_inter_rf_results, file = "base_inter_rf_results.Rda")
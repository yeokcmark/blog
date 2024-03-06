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

# xgboost
xgb_spec <-
  boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

xgb_spec_for_tuning <-
  xgb_spec %>% 
  set_args(trees = tune(),
           min_n = tune(),
           mtry = tune(),
           learn_rate = 0.01,
           tree_depth = tune(),
           loss_reduction = tune(),
           sample_size = tune())

set.seed(2024021403)
xgboost_race_tuning_wf <-
  workflows::workflow(dummy_rec, xgb_spec_for_tuning)

doParallel::registerDoParallel(cores = 2)
library(finetune)
xgboost_race_tuning_results <-
  tune_race_anova(
    xgboost_race_tuning_wf,
    data_fold,
    grid = 25,
    metrics = metric_set(mn_log_loss),
    control = control_race(verbose = TRUE,
                           verbose_elim = TRUE,
                           allow_par = TRUE,
                           save_workflow = TRUE,
                           parallel_over = "resamples"))
  


pacman::p_load("tidyverse", #for tidy data science practice
               "tidymodels", "workflows", "finetune",# for tidy machine learning
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
               "janitor", "lubridate", "haven")

nnet_spec_for_tuning <- 
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>% 
  set_engine("nnet", MaxNWts = 2600) %>% 
  set_mode("classification")

set.seed(2024021403)
nnet_race_tuning_wf <-
  workflows::workflow(normal_rec, nnet_spec_for_tuning)

doParallel::registerDoParallel(cores = 2)
nnet_race_tuning_results <-
  tune_race_anova(
    nnet_race_tuning_wf,
    data_fold,
    grid = 25,
    metrics = metric_set(roc_auc),
    control = control_race(verbose = TRUE,
                           verbose_elim = TRUE,
                           allow_par = TRUE,
                           save_workflow = TRUE,
                           parallel_over = "resamples"))
    
    
  


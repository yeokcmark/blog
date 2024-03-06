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
               "ranger", "xgboost", "glmnet", "kknn", "earth", "klaR", "discrim", "naivebayes",#random forest
               "janitor", "lubridate")

bayesian_model_results <-
  workflow_map(model_racing_set,
               fn = "tune_bayes",
               resamples = data_fold_full,
               grid = 11,
               metrics = metric_set(roc_auc),
               control = control_bayes(verbose = TRUE,
                                      verbose_iter = TRUE,
                                      allow_par = TRUE,
                                      save_workflow = TRUE,
                                      parallel_over = "everything",
                                      seed = 2025022001))
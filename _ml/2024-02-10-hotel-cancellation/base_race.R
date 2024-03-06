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



set.seed(2024020102)
doParallel::registerDoParallel(cl=3, cores = 6)

racing_results <-
  workflow_map(base_set,
               fn = "tune_race_anova",
               resamples = data_fold,
               grid = c(3:10),
               metrics = metric_set(roc_auc, f_meas, accuracy, mn_log_loss),
               control = control_race(verbose = TRUE,
                                      verbose_elim = TRUE,
                                      allow_par = TRUE,
                                      save_workflow = TRUE,
                                      parallel_over = "everything"))

save(racing_results, file = "racing_results.Rda")
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



# base recipe and rf workflow
base_rf_wflow2 <-
  workflow() %>% 
  add_model(rf_spec_for_tuning) %>% 
  add_recipe(base_rec)

# set up new search grid
rf_grid <-
  extract_parameter_set_dials(rf_spec_for_tuning) %>% 
  update(trees = trees(c(100L,1500L)),
         mtry = mtry(c(5L,100L)),
         min_n = min_n(c(5L,100L)
         )
  )

rf_metrics <-
  metric_set(roc_auc, f_meas, accuracy, mn_log_loss)

# tune_sim_anneal

set.seed(2024040301)
doParallel::registerDoParallel(cl=3, cores = 6)

base_rf_sim_anneal_result <-
  tune_sim_anneal(
    object = base_rf_wflow2,
    resamples = data_fold,
    iter = 25,
    metrics = rf_metrics,
    param_info = rf_grid,
    initial = 1,
    control = control_sim_anneal(verbose = TRUE,
                                 verbose_iter = TRUE,
                                 allow_par = TRUE,
                                 parallel_over = "everything")
  )

save(base_rf_sim_anneal_result, file = "base_rf_sim_anneal_result.Rda")
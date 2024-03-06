
rm(list = ls())
sessionInfo()

# Set packages and dependencies
pacman::p_load("tidyverse", #for tidy data science practice
               "tidymodels", "workflows", "finetune", "stacks",# for tidy machine learning
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

df <- read_csv("rental_price.csv")
data <-
  df %>% 
  mutate(across(c(district, region, ref_year, property), as.factor))

# split data
set.seed(2024020101)
data_split <-
  data %>% 
  initial_split(strata = region) # strata by region
data_train <-
  data_split %>% 
  training()
data_test <-
  data_split %>% 
  testing()
data_fold <-
  data_train %>% 
  vfold_cv(v = 10, strata = region)

base_rec <-
  recipes::recipe(formula = price_median ~ .,
                  data = data_train) %>% 
  update_role(unit_id, new_role = "unit_id") %>% 
  update_role(property, new_role = "name_id") %>% 
  step_rm(loc_x, loc_y) %>%  # remove geo locator, property name
  step_dummy(all_nominal_predictors())

# models

rf_spec <-
  rand_forest(trees = 1000L) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

rf_spec_for_tuning <-
  rf_spec %>% 
  set_args(mtry = tune(),
           min_n = tune())

knn_spec <-
  nearest_neighbor() %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

knn_spec_for_tuning <-
  knn_spec %>% 
  set_args(neighbors = tune(),
           weight_func = "optimal",
           dist_power = tune())

# xgboost
xgb_spec <-
  boost_tree(trees = 1000L) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

xgb_spec_for_tuning <-
  xgb_spec %>% 
  set_args(tree_depth = tune(),
           min_n = tune(),
           loss_reduction = tune(),                     
           sample_size = tune(),
           mtry = tune(),        
           learn_rate = tune())

# workflow

rf_wflow <-
  workflow() %>% 
  add_recipe(base_rec) %>% 
  add_model(rf_spec_for_tuning)

knn_wflow <-
  workflow() %>% 
  add_recipe(base_rec) %>% 
  add_model(knn_spec_for_tuning)

xgb_wflow <-
  workflow() %>% 
  add_recipe(base_rec) %>% 
  add_model(xgb_spec_for_tuning)
  
# resamples tune

rf_res <-
  tune_grid(
    object = rf_wflow,
    resamples = data_fold,
    grid = 11,
    verbose = TRUE,
    control = control_stack_grid()
  )

knn_res <-
  tune_grid(
    object = knn_wflow,
    resamples = data_fold,
    grid = 11,
    verbose = TRUE,
    control = control_stack_grid()
  )

xgb_res <-
  tune_grid(
    object = xgb_wflow,
    resamples = data_fold,
    grid = 11,
    verbose = TRUE,
    control = control_stack_grid()
  )

# stack

hdb_model_st <-
  stacks() %>% 
  add_candidates(rf_res) %>% 
  add_candidates(knn_res) %>% 
  add_candidates(xgb_res) %>% 
  blend_predictions() %>% 
  fit_members()
# generate predictions
hdb_pred <-
  data_test %>% 
  bind_cols(predict(hdb_model_st, .)) %>% 
  ggplot(aes(x = price_median,
             y = .pred)) +
  geom_point(alpha = 0.2)+
  geom_abline(lty = 2,
              color = "dodgerblue") +
  labs(x = "Actual Median Price $psf",
       y = "Predicted Median Price $psf")+
  theme_bw()

# calculate rmse

yardstick::rmse(hdb_pred,
               truth = price_median,
               estimate = .pred)

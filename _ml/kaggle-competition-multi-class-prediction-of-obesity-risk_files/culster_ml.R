## This dataset came from a kaggle competition where you are asked to try and predict
## obesity (NObeyesdad) from a list of features given.

rm(list = ls())
pacman::p_load(tidyverse, # tidy DS
               tidymodels, # tidy ML
               skimr, GGally, Hmisc, broom, modelr, ggstatsplot, # EDA 1
               scales, ggthemes, gridExtra, # ggplot2::
               DT, # for interactive data display
               janitor, themis, # recipes::
               factoextra, cluster, NbClust, clevr, fossil, dbscan,
               tidyclust,
               doParallel)


## Load functions written by me ----
# function to define model parameters----
my_function_define_model_parameters <-
  function(my_workflowset, my_data)
  {
    result_df <- list()
    
    for (i in 1:length(my_workflowset$wflow_id))
    {
      param_name <- paste0("param_", my_workflowset$wflow_id[i])
      param <- 
        my_workflowset %>% 
        extract_workflow(id = my_workflowset$wflow_id[i]) %>% 
        extract_parameter_set_dials() %>% 
        finalize(my_data)
      result_df[[i]] <-list(param_name, param)
    }
    return(result_df)
  }

# function to insert model parameters into workflowset

my_function_insert_parameters <-
  function (my_workflowset, list_of_parameters)
  {
    for (i in 1:length(my_workflowset$wflow_id))
    {
      my_workflowset <-
        my_workflowset %>% 
        option_add(param_info = pluck(list_of_parameters[[i]][2],1),
                   id = my_workflowset$wflow_id[i])
    }
    
    return (my_workflowset)
    
  }

#### function to collect predictions
my_function_collect_predictions <-
  function(tune_workflowset, wflow_id_tbl)
  {
    pred_summary <- tibble()
    
    for (i in 1:nrow(wflow_id_tbl))
    {
      last_fit_pred <-
        tune_workflowset %>% 
        extract_workflow(id = as.character(wflow_id_tbl[i,1])) %>% 
        finalize_workflow(tune_workflowset %>%
                            extract_workflow_set_result(id = as.character(wflow_id_tbl[i,1])) %>% 
                            select_best(metric = "roc_auc")) %>% 
        last_fit(data_split) %>% 
        collect_predictions() %>% 
        dplyr::select(.pred_Yes,
                      turnover) %>% 
        mutate(algorithm = wflow_id_tbl[i,1])
      
      pred_summary<-bind_rows(pred_summary, last_fit_pred)
      
    }
    return(pred_summary)
  }



# import split data
set.seed(2024041701)
data_train <- read_csv("train.csv")
skim(data_train)
# check missing data
table(is.na(data_train))

# 7 levels of obesity levels ie: 7 clusters
unique(data_train$NObeyesdad)


# lets try and do things outside tidymodels framework
df <-
  data_train %>% 
  # use dplyr way to create numeric data from character labels
  dplyr::mutate(Gender = case_when(Gender == "Female" ~ 0,
                                   Gender == "Male" ~ 1),
                family_history_with_overweight = case_when(family_history_with_overweight == "no" ~ 0,
                                                           family_history_with_overweight == "yes" ~ 1),
                FAVC = case_when(FAVC == "no" ~ 0,
                                 FAVC == "yes" ~ 1),
                SMOKE = case_when(SMOKE == "no" ~ 0,
                                  SMOKE == "yes" ~ 1),
                SCC = case_when(SCC == "no" ~ 0,
                                SCC == "yes" ~ 1),
                CAEC = case_when(CAEC == "no" ~ 0,
                                 CAEC == "Sometimes" ~ 1,
                                 CAEC == "Frequently" ~ 2,
                                 CAEC == "Always" ~ 3,
                                 .default = as.integer(4)),
                CALC = case_when(CALC == "no" ~ 0,
                                 CALC == "Sometimes" ~ 1,
                                 CALC == "Frequently" ~ 2,
                                 .default = as.integer(3)),
                MTRANS = case_when(MTRANS == "Public_Transportation" ~ 0,
                                   MTRANS == "Walking" ~ 1,
                                   MTRANS == "Bike" ~ 2,
                                   MTRANS == "Motorbike" ~ 3,
                                   MTRANS == "Automobile" ~ 4,
                                   .default = as.integer(5))) %>% 
  #convert id to rownames and retain for use later
  column_to_rownames(var = "id")
                
                # ,
                # NObeyesdad = case_when(NObeyesdad == "Insufficient_Weight" ~ 0,
                #                        NObeyesdad == "Normal_Weight" ~ 1,
                #                        NObeyesdad == "Overweight_Level_I" ~ 2,
                #                        NObeyesdad == "Overweight_Level_II" ~ 3,
                #                        NObeyesdad == "Obesity_Type_I" ~ 4,
                #                        NObeyesdad == "Obesity_Type_II" ~ 5,
                #                        NObeyesdad == "Obesity_Type_III" ~ 6,
                #                        .default = as.integer(7)))

# prepare data for clustering
data <-
  df %>% 
  # remove ground truth and id. 
  dplyr::select(- NObeyesdad) %>% 
  # center
  scale(center = TRUE)

# Lets investigate if k-means algorithm can correctly predict how many clusters there are

# Use voting to determine optimal k
set.seed(2024042401)
doParallel::registerDoParallel()
res_voting <-
  NbClust(data,
          distance = "euclidean",
          method = "ward.D2",
          index = "all", #index for performance (30 metrics)
          min.nc = 2,
          max.nc = 10)
# According to the majority rule, the best number of clusters is  8
# not too bad. Its quite close to the goundtruth of 7.

set.seed(2024042501)
res_cluster <-
  data %>% 
  kmeans(centers = 8,
         iter.max = 50,
         nstart = 25)

data_with_cluster <-
  cbind(df, res_cluster$cluster) %>% 
  rename(cluster = `res_cluster$cluster`)

data_train1 <-
  data_with_cluster %>% 
  filter(cluster == 1)
data_fold1 <-
  data_train1 %>% 
  vfold_cv(v = 10, strata = NObeyesdad)

data_train2 <-
  data_with_cluster %>% 
  filter(cluster == 2) 
data_fold2 <-
  data_train2 %>% 
  vfold_cv(v = 10, strata = NObeyesdad)

data_train3 <-
  data_with_cluster %>% 
  filter(cluster == 3)
data_fold3 <-
  data_train3 %>% 
  vfold_cv(v = 10, strata = NObeyesdad)

data_train4 <-
  data_with_cluster %>% 
  filter(cluster == 4)
data_fold4 <-
  data_train4 %>% 
  vfold_cv(v = 10, strata = NObeyesdad)

data_train5 <-
  data_with_cluster %>% 
  filter(cluster == 5) 
data_fold5 <-
  data_train5 %>% 
  vfold_cv(v = 10, strata = NObeyesdad)

data_train6 <-
  data_with_cluster %>% 
  filter(cluster == 6)
data_fold6 <-
  data_train6 %>% 
  vfold_cv(v = 10, strata = NObeyesdad)

data_train7 <-
  data_with_cluster %>% 
  filter(cluster == 7) 
data_fold7 <-
  data_train7 %>% 
  vfold_cv(v = 10, strata = NObeyesdad)

data_train8 <-
  data_with_cluster %>% 
  filter(cluster == 8) 
data_fold8 <-
  data_train8 %>% 
  vfold_cv(v = 10, strata = NObeyesdad)

rec_base1 <-
  recipes::recipe(formula = NObeyesdad ~.,
                  data = data_train1) %>% 
  update_role(cluster, new_role = "id") %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())

rec_base2 <-
  recipes::recipe(formula = NObeyesdad ~.,
                  data = data_train2) %>% 
  update_role(cluster, new_role = "id") %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())

rec_base3 <-
  recipes::recipe(formula = NObeyesdad ~.,
                  data = data_train3) %>% 
  update_role(cluster, new_role = "id") %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())

rec_base4 <-
  recipes::recipe(formula = NObeyesdad ~.,
                  data = data_train4) %>% 
  update_role(cluster, new_role = "id") %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())

rec_base5 <-
  recipes::recipe(formula = NObeyesdad ~.,
                  data = data_train5) %>% 
  update_role(cluster, new_role = "id") %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())

rec_base6 <-
  recipes::recipe(formula = NObeyesdad ~.,
                  data = data_train6) %>% 
  update_role(cluster, new_role = "id") %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())

rec_base7 <-
  recipes::recipe(formula = NObeyesdad ~.,
                  data = data_train7) %>% 
  update_role(cluster, new_role = "id") %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())

rec_base8 <-
  recipes::recipe(formula = NObeyesdad ~.,
                  data = data_train8) %>% 
  update_role(cluster, new_role = "id") %>% 
  step_nzv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())

# xgboost
spec_xgb <-
  boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") %>% 
  set_args(trees = tune("xgb_trees"),
           tree_depth = tune("xgb_tree_depth"),
           min_n = tune("xgb_min_n"),
           loss_reduction = tune("xgb_loss_red"),
           sample_size = tune("xgb_sample"),
           mtry = tune("xgb_mtry"),
           learn_rate = tune("xgb_learn"),
           stop_iter = 10)

metric_model <-
  metric_set(roc_auc,
             f_meas,
             accuracy)

base_set1 <- 
  workflow_set (
    # list pre-processor recipes
    list(base = rec_base1),
    # list models under evaluation
    list(xgb = spec_xgb),
    cross = TRUE)

set.seed(2024042501)
cl <- (detectCores()/2) - 1
cores <- cl*2

doParallel::registerDoParallel(cl, cores)

first_tune1 <-
  workflow_map(base_set1,
               fn = "tune_grid",
               verbose = TRUE,
               seed = 2024042501,
               grid = 11,
               resamples = data_fold1,
               metrics = metric_model,
               control = control_grid(verbose = TRUE,
                                      allow_par = TRUE,
                                      parallel_over = "everything"))

fit_xgb1 <-
  workflow() %>% 
  add_model(spec_xgb) %>% 
  add_recipe(rec_base1) %>% 
  finalize_workflow(first_tune1 %>% 
                      extract_workflow_set_result(id = "base_xgb") %>% 
                      select_best(metric = "accuracy")) %>% 
  fit(data_train1)



base_set2 <- 
  workflow_set (
    # list pre-processor recipes
    list(base = rec_base2),
    # list models under evaluation
    list(xgb = spec_xgb),
    cross = TRUE)

first_tune2 <-
  workflow_map(base_set2,
               fn = "tune_grid",
               verbose = TRUE,
               seed = 2024042501,
               grid = 11,
               resamples = data_fold2,
               metrics = metric_model,
               control = control_grid(verbose = TRUE,
                                      allow_par = TRUE,
                                      parallel_over = "everything"))

fit_xgb2 <-
  workflow() %>% 
  add_model(spec_xgb) %>% 
  add_recipe(rec_base2) %>% 
  finalize_workflow(first_tune2 %>% 
                      extract_workflow_set_result(id = "base_xgb") %>% 
                      select_best(metric = "accuracy")) %>% 
  fit(data_train2)


base_set3 <- 
  workflow_set (
    # list pre-processor recipes
    list(base = rec_base3),
    # list models under evaluation
    list(xgb = spec_xgb),
    cross = TRUE)

first_tune3 <-
  workflow_map(base_set3,
               fn = "tune_grid",
               verbose = TRUE,
               seed = 2024042501,
               grid = 11,
               resamples = data_fold3,
               metrics = metric_model,
               control = control_grid(verbose = TRUE,
                                      allow_par = TRUE,
                                      parallel_over = "everything"))

fit_xgb3 <-
  workflow() %>% 
  add_model(spec_xgb) %>% 
  add_recipe(rec_base3) %>% 
  finalize_workflow(first_tune3 %>% 
                      extract_workflow_set_result(id = "base_xgb") %>% 
                      select_best(metric = "accuracy")) %>% 
  fit(data_train3)


base_set4 <- 
  workflow_set (
    # list pre-processor recipes
    list(base = rec_base4),
    # list models under evaluation
    list(xgb = spec_xgb),
    cross = TRUE)

first_tune4 <-
  workflow_map(base_set4,
               fn = "tune_grid",
               verbose = TRUE,
               seed = 2024042501,
               grid = 11,
               resamples = data_fold4,
               metrics = metric_model,
               control = control_grid(verbose = TRUE,
                                      allow_par = TRUE,
                                      parallel_over = "everything"))

fit_xgb4 <-
  workflow() %>% 
  add_model(spec_xgb) %>% 
  add_recipe(rec_base4) %>% 
  finalize_workflow(first_tune4 %>% 
                      extract_workflow_set_result(id = "base_xgb") %>% 
                      select_best(metric = "accuracy")) %>% 
  fit(data_train4)



base_set5 <- 
  workflow_set (
    # list pre-processor recipes
    list(base = rec_base5),
    # list models under evaluation
    list(xgb = spec_xgb),
    cross = TRUE)

first_tune5 <-
  workflow_map(base_set5,
               fn = "tune_grid",
               verbose = TRUE,
               seed = 2024042501,
               grid = 11,
               resamples = data_fold5,
               metrics = metric_model,
               control = control_grid(verbose = TRUE,
                                      allow_par = TRUE,
                                      parallel_over = "everything"))

fit_xgb5 <-
  workflow() %>% 
  add_model(spec_xgb) %>% 
  add_recipe(rec_base5) %>% 
  finalize_workflow(first_tune5 %>% 
                      extract_workflow_set_result(id = "base_xgb") %>% 
                      select_best(metric = "accuracy")) %>% 
  fit(data_train5)



base_set6 <- 
  workflow_set (
    # list pre-processor recipes
    list(base = rec_base6),
    # list models under evaluation
    list(xgb = spec_xgb),
    cross = TRUE)

first_tune6 <-
  workflow_map(base_set6,
               fn = "tune_grid",
               verbose = TRUE,
               seed = 2024042501,
               grid = 11,
               resamples = data_fold6,
               metrics = metric_model,
               control = control_grid(verbose = TRUE,
                                      allow_par = TRUE,
                                      parallel_over = "everything"))

fit_xgb6 <-
  workflow() %>% 
  add_model(spec_xgb) %>% 
  add_recipe(rec_base6) %>% 
  finalize_workflow(first_tune6 %>% 
                      extract_workflow_set_result(id = "base_xgb") %>% 
                      select_best(metric = "accuracy")) %>% 
  fit(data_train6)



base_set7 <- 
  workflow_set (
    # list pre-processor recipes
    list(base = rec_base7),
    # list models under evaluation
    list(xgb = spec_xgb),
    cross = TRUE)

first_tune7 <-
  workflow_map(base_set7,
               fn = "tune_grid",
               verbose = TRUE,
               seed = 2024042501,
               grid = 11,
               resamples = data_fold7,
               metrics = metric_model,
               control = control_grid(verbose = TRUE,
                                      allow_par = TRUE,
                                      parallel_over = "everything"))

fit_xgb7 <-
  workflow() %>% 
  add_model(spec_xgb) %>% 
  add_recipe(rec_base7) %>% 
  finalize_workflow(first_tune7 %>% 
                      extract_workflow_set_result(id = "base_xgb") %>% 
                      select_best(metric = "accuracy")) %>% 
  fit(data_train7)


base_set8 <- 
  workflow_set (
    # list pre-processor recipes
    list(base = rec_base8),
    # list models under evaluation
    list(xgb = spec_xgb),
    cross = TRUE)

first_tune8 <-
  workflow_map(base_set8,
               fn = "tune_grid",
               verbose = TRUE,
               seed = 2024042501,
               grid = 11,
               resamples = data_fold8,
               metrics = metric_model,
               control = control_grid(verbose = TRUE,
                                      allow_par = TRUE,
                                      parallel_over = "everything"))

fit_xgb8 <-
  workflow() %>% 
  add_model(spec_xgb) %>% 
  add_recipe(rec_base8) %>% 
  finalize_workflow(first_tune8 %>% 
                      extract_workflow_set_result(id = "base_xgb") %>% 
                      select_best(metric = "accuracy")) %>% 
  fit(data_train8)


# now we need to cluster the test data----

df_test <-read_csv("test.csv")

df_test <-
  df_test %>% 
  # use dplyr way to create numeric data from character labels
  dplyr::mutate(Gender = case_when(Gender == "Female" ~ 0,
                                   Gender == "Male" ~ 1),
                family_history_with_overweight = case_when(family_history_with_overweight == "no" ~ 0,
                                                           family_history_with_overweight == "yes" ~ 1),
                FAVC = case_when(FAVC == "no" ~ 0,
                                 FAVC == "yes" ~ 1),
                SMOKE = case_when(SMOKE == "no" ~ 0,
                                  SMOKE == "yes" ~ 1),
                SCC = case_when(SCC == "no" ~ 0,
                                SCC == "yes" ~ 1),
                CAEC = case_when(CAEC == "no" ~ 0,
                                 CAEC == "Sometimes" ~ 1,
                                 CAEC == "Frequently" ~ 2,
                                 CAEC == "Always" ~ 3,
                                 .default = as.integer(4)),
                CALC = case_when(CALC == "no" ~ 0,
                                 CALC == "Sometimes" ~ 1,
                                 CALC == "Frequently" ~ 2,
                                 .default = as.integer(3)),
                MTRANS = case_when(MTRANS == "Public_Transportation" ~ 0,
                                   MTRANS == "Walking" ~ 1,
                                   MTRANS == "Bike" ~ 2,
                                   MTRANS == "Motorbike" ~ 3,
                                   MTRANS == "Automobile" ~ 4,
                                   .default = as.integer(5))) %>% 
  #convert id to rownames and retain for use later
  column_to_rownames(var = "id")


# prepare test data for clustering

data_test <-
  df_test %>%
  # center
  scale(center = TRUE)


#cluster the test data
res_cluster_test <-
  data_test %>% 
  kmeans(centers = 8,
         iter.max = 25,
         nstart = 25)

data_test_with_cluster <-
  cbind(df_test, res_cluster_test$cluster) %>% 
  rename(cluster = `res_cluster_test$cluster`)

data_test1 <-
  data_test_with_cluster %>% 
  filter(cluster == 1)

data_test2 <-
  data_test_with_cluster %>% 
  filter(cluster == 2)

data_test3 <-
  data_test_with_cluster %>% 
  filter(cluster == 3)

data_test4 <-
  data_test_with_cluster %>% 
  filter(cluster == 4)

data_test5 <-
  data_test_with_cluster %>% 
  filter(cluster == 5) 

data_test6 <-
  data_test_with_cluster %>% 
  filter(cluster == 6)

data_test7 <-
  data_test_with_cluster %>% 
  filter(cluster == 7) 

data_test8 <-
  data_test_with_cluster %>% 
  filter(cluster == 8)

# make predictions on each cluster

predict_xgb1 <-
  fit_xgb1 %>% 
  predict(data_test1, type = c("class"))

pred1 <-
  cbind(data_test1, predict_xgb1) %>% 
  rownames_to_column(var = "id") %>% 
  dplyr::select(id, .pred_class) %>% 
  rename(NObeyesdad = .pred_class)

predict_xgb2 <-
  fit_xgb2 %>% 
  predict(data_test2, type = c("class"))

pred2 <-
  cbind(data_test2, predict_xgb2) %>% 
  rownames_to_column(var = "id") %>% 
  dplyr::select(id, .pred_class) %>% 
  rename(NObeyesdad = .pred_class)

predict_xgb3 <-
  fit_xgb3 %>% 
  predict(data_test3, type = c("class"))

pred3 <-
  cbind(data_test3, predict_xgb3) %>% 
  rownames_to_column(var = "id") %>% 
  dplyr::select(id, .pred_class) %>% 
  rename(NObeyesdad = .pred_class)

predict_xgb4 <-
  fit_xgb4 %>% 
  predict(data_test4, type = c("class"))

pred4 <-
  cbind(data_test4, predict_xgb4) %>% 
  rownames_to_column(var = "id") %>% 
  dplyr::select(id, .pred_class) %>% 
  rename(NObeyesdad = .pred_class)

predict_xgb5 <-
  fit_xgb5 %>% 
  predict(data_test5, type = c("class"))

pred5 <-
  cbind(data_test5, predict_xgb5) %>% 
  rownames_to_column(var = "id") %>% 
  dplyr::select(id, .pred_class) %>% 
  rename(NObeyesdad = .pred_class)

predict_xgb6 <-
  fit_xgb6 %>% 
  predict(data_test6, type = c("class"))

pred6 <-
  cbind(data_test6, predict_xgb6) %>% 
  rownames_to_column(var = "id") %>% 
  dplyr::select(id, .pred_class) %>% 
  rename(NObeyesdad = .pred_class)

predict_xgb7 <-
  fit_xgb7 %>% 
  predict(data_test7, type = c("class"))

pred7 <-
  cbind(data_test7, predict_xgb7) %>% 
  rownames_to_column(var = "id") %>% 
  dplyr::select(id, .pred_class) %>% 
  rename(NObeyesdad = .pred_class)

predict_xgb8 <-
  fit_xgb8 %>% 
  predict(data_test8, type = c("class"))

pred8 <-
  cbind(data_test8, predict_xgb8) %>% 
  rownames_to_column(var = "id") %>% 
  dplyr::select(id, .pred_class) %>% 
  rename(NObeyesdad = .pred_class)

cluster_pred <-
  rbind(pred1, pred2, pred3, pred4, pred5, pred6, pred7, pred8) %>% 
  arrange(id)
write_csv(cluster_pred, "submission_cluster_ml.csv")

save.image("clustering_ml_obesity.RData")

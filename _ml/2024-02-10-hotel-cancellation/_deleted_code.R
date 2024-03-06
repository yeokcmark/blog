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



# random forest
rf_spec <-
  rand_forest() %>% 
  set_engine("ranger",
             importance = "impurity") %>% 
  set_mode("classification")

rf_spec_for_tuning <-
  rf_spec %>% 
  set_args(trees = tune(),
           mtry = tune(),
           min_n = tune())
# 
# # Classification Tree Model
# ct_spec <- 
#   decision_tree() %>%
#   set_engine(engine = 'rpart') %>%
#   set_mode('classification') 
# 
# ct_spec_for_tuning <-
#   ct_spec %>% 
#   set_args(tree_depth = tune(),
#            min_n = tune(), 
#            cost_complexity = tune())

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
  boost_tree() %>% 
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

# # # naive bayes
# 
# naive_spec <-
#   naive_Bayes() %>%
#   set_engine("naivebayes",
#              usepoisson = TRUE) %>%
#   set_mode("classification")
# 
# naive_spec_for_tuning <-
#   naive_spec %>% 
#   set_args(smoothness = tune(),
#            Laplace = tune())

# Logistic Regression Model
logistic_spec <- 
  logistic_reg() %>%
  set_engine(engine = 'glm') %>%
  set_mode('classification') 

null_spec <-
  null_model() %>% 
  set_mode("classification") %>% 
  set_engine("parsnip")

# 
# # Lasso Logistic Regression Model
# 
# logistic_lasso_spec <-
#   logistic_reg(mixture = 1, penalty = 1) %>% 
#   set_engine(engine = 'glmnet') %>%
#   set_mode('classification') 
# 
# 
# logistic_lasso_spec_for_tuning <- 
#   logistic_lasso_spec %>% 
#   set_args(penalty = tune()) #we could let penalty = tune()
# 
# nnet_spec_for_tuning <- 
#    mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>% 
#    set_engine("nnet", MaxNWts = 2600) %>% 
#    set_mode("classification")
# 
# svm_r_spec_for_tuning <- 
#    svm_rbf(cost = tune(), rbf_sigma = tune()) %>% 
#    set_engine("kernlab") %>% 
#    set_mode("classification")
# 
# svm_p_spec_for_tuning <- 
#    svm_poly(cost = tune(), degree = tune()) %>% 
#    set_engine("kernlab") %>% 
#    set_mode("classification")

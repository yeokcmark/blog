pacman::p_load("tidyverse", "tidymodels", "broom", 
               "highcharter", "psych", "factoextra")

df <- read_csv("mutagen.csv")
skim(data)
table(is.na(df))

data_pca <-
  df %>% 
  dplyr::select(-1, -2) %>% 
  prcomp(scale = TRUE, center = TRUE)

data_pca$rotation
plot(data_pca)
summary(data_pca)

#PC4 0.51
#PC8 0.598
#PC20 0.697
# PC50 0/8
# PC124 0.9

eigenvalue <-
  data_pca %>% 
  tidy(matrix = "eigenvalues") %>% 
  mutate(eigenvalues = std.dev^2) %>% 
  print (n=200)

# split data
set.seed(2024030101)
data_split <-
  df %>% 
  dplyr::select(-1) %>% 
  #slice_sample(prop = 0.2) %>% 
  initial_split(strata = outcome)

data_train <-
  data_split %>% 
  training()
data_test <-
  data_split %>% 
  testing()
data_fold <-
  data_train %>% 
  vfold_cv(v = 10, strata = outcome)


## prep recipe
rec_pc4 <-
  recipes::recipe(formula = outcome ~.,
                  data = data_train) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(all_numeric_predictors(), num_comp = 4)

rec_pc8 <-
  recipes::recipe(formula = outcome ~.,
                  data = data_train) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(all_numeric_predictors(), num_comp = 8)

rec_pc20 <-
  recipes::recipe(formula = outcome ~.,
                  data = data_train) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(all_numeric_predictors(), num_comp = 20)

rec_pc50 <-
  recipes::recipe(formula = outcome ~.,
                  data = data_train) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(all_numeric_predictors(), num_comp = 50)

rec_pc124 <-
  recipes::recipe(formula = outcome ~.,
                  data = data_train) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(all_numeric_predictors(), num_comp = 124)


# random forest
spec_rf <-
  rand_forest() %>% 
  set_engine("ranger",
             importance = "impurity") %>% 
  set_mode("classification") %>% 
  set_args(trees = tune(),
           mtry = tune(),
           min_n = tune())


# workflow set
base_set <- 
  workflow_set (
    list(rec_pc4 = rec_pc4,
         rec_pc8 = rec_pc8,
         rec_pc20 = rec_pc20,
         rec_pc50 = rec_pc50,
         rec_pc124 = rec_pc124), #preprocessor
    list(rf = spec_rf), #model
    cross = TRUE) #default is cross = TRUE

# set parameters

param_pc4 <-
  base_set %>%
  extract_workflow(id = "rec_pc4_rf") %>% 
  extract_parameter_set_dials() %>% 
  finalize(data_train)

param_pc8 <-
  base_set %>%
  extract_workflow(id = "rec_pc8_rf") %>% 
  extract_parameter_set_dials() %>% 
  finalize(data_train)

param_pc20 <-
  base_set %>%
  extract_workflow(id = "rec_pc20_rf") %>% 
  extract_parameter_set_dials() %>% 
  finalize(data_train)

param_pc50 <-
  base_set %>%
  extract_workflow(id = "rec_pc50_rf") %>% 
  extract_parameter_set_dials() %>% 
  finalize(data_train)

param_pc124 <-
  base_set %>%
  extract_workflow(id = "rec_pc124_rf") %>% 
  extract_parameter_set_dials() %>% 
  finalize(data_train)


base_set <-
  base_set %>% 
  option_add(param_info = param_pc4, id = "rec_pc4_rf") %>% 
  option_add(param_info = param_pc8, id = "rec_pc8_rf") %>% 
  option_add(param_info = param_pc20, id = "rec_pc20_rf") %>% 
  option_add(param_info = param_pc50, id = "rec_pc50_rf") %>% 
  option_add(param_info = param_pc124, id = "rec_pc124_rf")


set.seed(2024030302)
cl <- (detectCores()/2) - 1
cores <- cl*2

doParallel::registerDoParallel(cl, cores)

first_tune <-
  workflow_map(base_set,
               fn = "tune_grid",
               verbose = TRUE,
               seed = 2024030302,
               grid = 11,
               resamples = data_fold,
               metrics = metric_set(roc_auc, accuracy),
               control = control_grid(verbose = TRUE,
                                      allow_par = TRUE,
                                      parallel_over = "everything"))








data_pca$rotation[,1] %>% barplot() # loadings base R
screeplot(data_pca) #stats package

# biplot
biplot(data_pca, scaling = "symmetric")

fviz_pca_biplot(data_pca, repel = TRUE)
plot(data_pca$x[,1], data_pca$x[,2])

#loadings plot
data_pca %>% 
  tidy(matrix = "loadings") %>% 
  filter(PC <=10) %>% 
  group_by(PC) %>% 
  ggplot(aes(y = column,
             x = value)
         ) +
  geom_col() +
  facet_wrap(.~PC)
  

rec_pca <-
  recipe(formula = ~.,
         data = data) %>% 
  step_rm(...1) %>% 
  update_role(outcome, new_role = "id") %>% 
  step_normalize(all_numeric()) %>% 
  step_pca(num_comp = 2)

df <-
  rec_pca %>% 
  prep(verbose = T) %>% 
  bake(new_data = NULL)

df %>% 
  tidy(matrix = "rotation")

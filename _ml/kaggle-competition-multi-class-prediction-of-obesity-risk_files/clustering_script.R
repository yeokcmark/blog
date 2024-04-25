## This dataset came from a kaggle competition where you are asked to try and predict
## obesity (NObeyesdad) from a list of features given.

rm(list = ls())
pacman::p_load(tidyverse, # tidy DS
               tidymodels, # tidy ML
               skimr, GGally, Hmisc, broom, modelr, ggstatsplot, # EDA 1
               scales, ggthemes, gridExtra, # ggplot2::
               DT, # for interactive data display
               janitor, themis, # recipes::
               factoextra, cluster, NbClust, clevr, fossil,
               tidyclust, ggradar)


# import split data
set.seed(2024041701)
data_train <- read_csv("train.csv")
skim(data_train)
# check missing data
table(is.na(data_train))

# lets try and do things outside tidymodels framework
data <-
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
                                   .default = as.integer(5))
  ) %>% 
  # remove ground truth. See if clustering algorithm can correctly "predict" these labelled clusters 
  # using unsupervised learning
  dplyr::select(-id, -NObeyesdad) %>% 
  # center
  scale(center = TRUE) %>% 
  as_tibble()

# ground truth
ground_truth <-
  data_train %>% 
  dplyr::select(NObeyesdad)

# 7 levels of obesity levels ie: 7 clusters
distinct(ground_truth)

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
str(res_voting)

data %>% 
  fviz_nbclust(kmeans, k = 10, 
               "wss")
# looking at the chart, I would choose k=4 or k=6

# obtain cluster assignment
res_cluster <-
  res_voting$Best.partition %>% 
  as_tibble() %>% 
  rename(cluster_assign = value)

## In class, we were taught how to assess how good the culstering algo was
## based on within ss. But, if you also have ground truth, how do you then
## assess the performance of clustering algorithm?


data_with_cluster <-
  cbind(data_train, res_cluster)


ground_truth_cluster <-
  data_with_cluster %>% 
  dplyr::select(c(NObeyesdad, cluster_assign))

## use v_measure to assess ground truth vs clustering
v_measure(ground_truth_cluster$NObeyesdad, ground_truth_cluster$cluster_assign,
          beta = 1)

## there is also rand index
rand.index(ground_truth_cluster$NObeyesdad, ground_truth_cluster$cluster_assign)

save.image("clustering_obesity.RData")

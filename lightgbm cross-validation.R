################################################################################
# Instacart LightGBM Cross Validation
# Zhongke Zhang
# August 2017
################################################################################

library(lightgbm)
library(dplyr)

setwd("~/Desktop/temp/")
load("./final_train.RData")

#------------------------------------------------------------------------------#
# Features to Use
# Features list is saved in features.csv
#------------------------------------------------------------------------------#

features <- read.csv('./features.csv')
features$features_name <- features$features_name %>% as.character
features <- features[features$choose == 1, 'features_name']
features <- c('product_id', 'department_id', 'aisle_id', features)

#------------------------------------------------------------------------------#
# F1 metrics
#------------------------------------------------------------------------------#

f1 <- function(labels, preds){
  labels = strsplit(labels, split = ' ')[[1]]
  preds = strsplit(preds, split = ' ')[[1]]
  rr = intersect(labels, preds)
  precision = length(rr) / length(preds)
  recall = length(rr) / length(labels)
  if(precision + recall == 0){
    return(0)
  } else {
    f1 = 2 * precision * recall / (precision + recall)
    return(f1)
  }
}

#------------------------------------------------------------------------------#
# Customized Evaluation Function for F1
#------------------------------------------------------------------------------#

speed_up_round = 1

# lightgbm currently not support attr for lgb.Dataset object
# So the evalulation function will use global table to calulate f1 value

evalerror <- function(preds, dtrain) {
  
  # Since the eval function is slow, skip the metric calculation for first cv 100 rounds (1000 / 10 folds)
  if(speed_up_round < 1000){
    err <- speed_up_round/10000
    speed_up_round <<- speed_up_round + 1 # Update Global Variable
    return(list(name = "error", value = err, higher_better = TRUE))
  }
  
  # The size for each cross-validation group is different.
  # So from the length of preds we could know which folds we are evaluating.
  eval_df <- g.tables[[which(g.size == length(preds))]]
  eval_df$predict <- preds
  
  ground_truth <- eval_df %>% filter(reordered == 1) %>%
    group_by(order_id) %>%
    summarise(truth = paste(product_id, collapse = ' '))
  
  predicted <- eval_df %>%
    group_by(order_id) %>%
    summarise(products = exact_F1_max_none(predict, product_id))
  
  dat <- predicted %>% left_join(ground_truth, by = 'order_id')
  dat$truth[is.na(dat$truth)] = 'None'
  
  dat$f1 <- apply(dat, 1, function(x) f1(x[3], x[2]))
  err <- mean(dat$f1)
  return(list(name = "error", value = err, higher_better = TRUE))
}

#------------------------------------------------------------------------------#
# Parameters
#------------------------------------------------------------------------------#

param <- list(
  objective = 'binary',
  # metric = c('auc', 'binary_logloss'),
  num_leaves = 256,
  min_sum_hessian_in_leaf = 100,
  max_depth = 12,
  min_data_in_leaf = 800,
  learning_rate = 0.1,
  feature_fraction = 0.6,
  verbosity = 1
)

setdiff(features, colnames(train))

dtrain <- lgb.Dataset(
  as.matrix(train[,features]),
  label = train$reordered,
  categorical_feature = c(1,2,3)
)

#------------------------------------------------------------------------------#
# Cross-validation Folds
#------------------------------------------------------------------------------#

# Cross validation by user_id
# Find a good seed to generate a 10-folds split with small evaluation error

tb = c()
for(s in 1:15){
  set.seed(s)
  u_group <- unique(train$user_id) %>% length %>% runif %>% 
    cut(., breaks = seq(0,1,0.1)) %>% as.integer
  names(u_group) <- unique(train$user_id)
  train$group <- u_group[as.character(train$user_id)]
  print(table(table(train$group)))
  
  g.size <- table(train$group)
  
  g.tables <- list()
  for(i in 1:10){g.tables[[i]] <- train[train$group == i, c('order_id', 'product_id', 'reordered')]}
  
  groups <- lapply(1:10, function(x) which(train$group == x))
  
  gbm.cv <- lgb.cv(
    params = param,
    data = dtrain,
    folds = groups,
    nrounds = 1,
    eval = evalerror)
  
  err <- gbm.cv$record_evals$valid$error$eval_err
  tb <- rbind(tb, c(s, err[[1]]))
}

# 8 is a good seed.
set.seed(8)
u_group <- unique(train$user_id) %>% length %>% runif %>% 
  cut(., breaks = seq(0,1,0.1)) %>% as.integer
names(u_group) <- unique(train$user_id)
train$group <- u_group[as.character(train$user_id)]
g.size <- table(train$group)

# Tables for 10-folds to calculate corresponding f1 value.
g.tables <- list()
for(i in 1:10){g.tables[[i]] <- train[train$group == i, c('order_id', 'product_id', 'reordered')]}
groups <- lapply(1:10, function(x) which(train$group == x))

#------------------------------------------------------------------------------#
# Train Cross-validation Model
#------------------------------------------------------------------------------#

if exists("exact_F1_max_none"){
  gbm.cv <- lgb.cv(
    params = param,
    data = dtrain,
    folds = groups,
    early_stopping_rounds = 80,
    nrounds = 400,
    eval = evalerror)
} else {
  print("Load the exact_F1_max_none function from optimization f1.R")
}

#------------------------------------------------------------------------------#
# Plot and save the learning curve
#------------------------------------------------------------------------------#

eval_f1 <- gbm.cv$record_evals$valid$error$eval
plot(90:400, sapply(90:400, function(x) eval_f1[[x]]), type = 'l')
d <- sapply(1:400, function(x) eval_f1[[x]])

f1_err <- gbm.cv$record_evals$valid$error$eval
f1_err <- sapply(1:length(f1_err), function(x) f1_err[[x]])
f1_err_sd <- gbm.cv$record_evals$valid$error$eval_err
f1_err_sd <- sapply(1:length(f1_err_sd), function(x) f1_err_sd[[x]])
f1_err_df <- data.frame(n = 1:length(f1_err), f1 = f1_err, sd = f1_err_sd)
write.csv(f1_err_df, file = './result/lightgbm/f1_err_dfwhatever.csv', row.names = F)

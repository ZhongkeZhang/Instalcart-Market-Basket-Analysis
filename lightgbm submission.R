################################################################################
# Instacart LightGBM Submission
# Zhongke Zhang
# August 2017
################################################################################

library(lightgbm)
library(dplyr)

# Load train and test datasets.
load("./data/train.RData")
load("./data/test.RData")

#------------------------------------------------------------------------------#
# Features to Use
# Features list is saved in features.csv
#------------------------------------------------------------------------------#

features <- read.csv('./features.csv')
features$features_name <- features$features_name %>% as.character
features <- features[features$choose == 1, 'features_name']
features <- c('product_id', 'department_id', 'aisle_id', features)

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
# Train lightgbm model with 350 rounds
#------------------------------------------------------------------------------#

gbm <- lgb.train(params = param, data = dtrain, nrounds = 350)

#------------------------------------------------------------------------------#
# Predict
#------------------------------------------------------------------------------#

test$predict <- predict(gbm, as.matrix(test[,features]))

submission <- test %>% select(order_id, product_id, predict)
submission <- submission %>%
  group_by(order_id) %>%
  summarise(products = exact_F1_max_none(predict, product_id))
submission <- submission %>% arrange(order_id)

write.csv(submission, './submission.csv', row.names = F)

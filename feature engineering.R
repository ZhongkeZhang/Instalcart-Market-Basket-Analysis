################################################################################
# Instacart Feature Engineering
# Zhongke Zhang
# July 2017
################################################################################

library(data.table)
library(dplyr)
library(stringr)

#------------------------------------------------------------------------------#
# Orders.csv
#------------------------------------------------------------------------------#
orders <- fread("./data/orders.csv")

orders$days_since_prior_order[is.na(orders$days_since_prior_order)] = 0
orders <- orders %>% 
  group_by(user_id) %>%
  mutate(
    cum_days_since_first_order = cumsum(days_since_prior_order)
  )

orders <- orders %>%
  group_by(user_id) %>%
  mutate(
    days_gap_to_current = max(cum_days_since_first_order) - cum_days_since_first_order,
    orders_gap_to_current = max(order_number) - order_number
  )
orderp <- fread("./data/order_products__prior.csv")
orders_prod <- orders %>% inner_join(orderp)
rm(orderp)
gc()

#------------------------------------------------------------------------------#
# All possible User-Product pairs (13307953 rows including both train and test)
#------------------------------------------------------------------------------#

df <- orders_prod %>%
  filter(eval_set == 'prior') %>%
  group_by(user_id, product_id) %>%
  summarise(
    up_num_orders = n(),
    up_first_order_orders_gap = max(orders_gap_to_current),
    up_last_order_orders_gap = min(orders_gap_to_current),
    up_first_order_days_gap = max(days_gap_to_current),
    up_last_order_days_gap = min(days_gap_to_current),
    up_avg_cart_position = median(add_to_cart_order),
    up_sd_cart_position = sd(add_to_cart_order)
  )

df$up_order_rate <- df$up_num_orders / (df$up_first_order_orders_gap - df$up_last_order_orders_gap + 1)
df$up_order_days_rate <- df$up_num_orders / (df$up_first_order_days_gap - df$up_last_order_days_gap + 1)
# save(df, file = './data/df.RData')

#------------------------------------------------------------------------------#
# User-Product Features1
#------------------------------------------------------------------------------#

orders_prod$up_order_gap <- c(-1, diff(orders_prod$order_number))
orders_prod$up_order_gap[orders_prod$reordered == 0] = -1

orders_prod$up_days_since_last_purchase <- c(0 ,diff(orders_prod$cum_days_since_first_order))
orders_prod$up_days_since_last_purchase[orders_prod$reordered == 0] = 0
 
up_features1 <- orders_prod %>%
  filter(eval_set == 'prior') %>%
  filter(reordered == 1) %>%
  group_by(user_id, product_id) %>%
  summarise(
    up_avg_order_gap = mean(up_order_gap),
    up_sd_order_gap = sd(up_order_gap),
    up_max_order_gap = max(up_order_gap),
    up_min_order_gap = mean(up_order_gap),
    up_avg_order_days_gap = mean(up_days_since_last_purchase),
    up_sd_order_days_gap = sd(up_days_since_last_purchase),
    up_max_order_days_gap = max(up_days_since_last_purchase),
    up_min_order_days_gap = min(up_days_since_last_purchase)
  )

df <- df %>% left_join(up_features1)
rm(up_features1)
gc()
# save(df, file = './data/df.RData')

#------------------------------------------------------------------------------#
# User-Product Features2
#------------------------------------------------------------------------------#

up_features2 <- orders_prod %>%
  filter(eval_set == 'prior') %>%
  filter(days_gap_to_current <= 30) %>%
  filter(reordered == 1) %>%
  group_by(user_id, product_id) %>%
  summarise(
    up_recent_month_reorders = n(),
    up_recent_month_avg_order_gap = mean(up_order_gap),
    up_recent_month_sd_order_gap = sd(up_order_gap),
    up_recent_month_avg_order_days_gap = mean(up_days_since_last_purchase),
    up_recent_month_sd_order_days_gap = sd(up_days_since_last_purchase)
  )

df <- df %>% left_join(up_features2)
rm(up_features2)
gc()
# save(df, file = './data/df.RData')

#------------------------------------------------------------------------------#
# User-Product Features3
#------------------------------------------------------------------------------#

up_features3 <- orders_prod %>%
  filter(eval_set == 'prior') %>%
  filter(days_gap_to_current <= 90) %>%
  filter(reordered == 1) %>%
  group_by(user_id, product_id) %>%
  summarise(
    up_recent_quarter_reorders = n(),
    up_recent_quarter_avg_order_gap = mean(up_order_gap),
    up_recent_quarter_sd_order_gap = sd(up_order_gap),
    up_recent_quarter_avg_order_days_gap = mean(up_days_since_last_purchase),
    up_recent_quarter_sd_order_days_gap = sd(up_days_since_last_purchase)
  )

df <- df %>% left_join(up_features3)
rm(up_features3)
gc()
# save(df, file = './data/df.RData')

#------------------------------------------------------------------------------#
# User-Product Features4
#------------------------------------------------------------------------------#

up_features4 <- data.table::fread('./intermediate_data/up_order_streaks.csv')
df <- df %>% left_join(up_features4)
rm(up_features4)
gc()

#------------------------------------------------------------------------------#
# User Features 1
#------------------------------------------------------------------------------#

orders_prod <- orders_prod %>%
  group_by(user_id, product_id) %>%
  mutate(product_time = row_number())

u_features1 <- orders_prod %>%
  filter(eval_set == 'prior') %>%
  filter(order_number > 1) %>%
  group_by(user_id, product_id) %>%
  summarise(
    up_order_size = n(),
    up_reorder_size = sum(reordered == 1)
  ) %>%
  group_by(user_id) %>%
  summarise(
    u_avg_order_size = mean(up_order_size),
    u_std_order_size = sd(up_order_size),
    u_max_order_size = max(up_order_size),
    u_min_order_size = min(up_order_size),
    u_avg_reorder_size =  mean(up_reorder_size),
    u_std_reorder_size = sd(up_reorder_size),
    u_max_reorder_size = max(up_reorder_size),
    u_min_reorder_size = min(up_reorder_size)
  )

df <- df %>% left_join(u_features1)
rm(u_features1)
gc()

#------------------------------------------------------------------------------#
# User Features 2
#------------------------------------------------------------------------------#

u_features2 <- orders_prod %>%
  filter(eval_set == 'prior') %>%
  group_by(user_id) %>%
  summarise(
    u_total_counts = n(),
    u_prod_reordered_ratio = sum(product_time == 2)/sum(product_time == 1),
    u_avg_prod_order_times = 1 + sum(reordered)/sum(product_time == 1),
    u_distinct_products = n_distinct(product_id)
  )

df <- df %>% left_join(u_features2)
rm(u_features2)
gc()

#------------------------------------------------------------------------------#
# User Features 3
#------------------------------------------------------------------------------#

u_features3 <- orders_prod %>% group_by(user_id) %>%
  filter(order_number == max(order_number) - 1) %>%
  summarise(
    u_last_order_reordered_ratio = sum(reordered)/n(),
    u_last_order_size = n()
  )

df <- df %>% left_join(u_features3)
rm(u_features3)
gc()

#------------------------------------------------------------------------------#
# User Features 4
#------------------------------------------------------------------------------#

u_features4 <- orders_prod %>% group_by(user_id) %>%
  filter(order_number == max(order_number) - 2) %>%
  summarise(
    u_2ndlast_order_reordered_ratio = sum(reordered)/n(),
    u_2ndlast_order_size = n()
  )

df <- df %>% left_join(u_features4)
rm(u_features4)
gc()


#------------------------------------------------------------------------------#
# User Department, Product Name, Aisle Features
#------------------------------------------------------------------------------#

products <- fread("./data/products.csv")

products <- products %>%
  mutate(prod_organic = ifelse(
    str_detect(str_to_lower(products$product_name),'organic'),1,0))

orders_prod <- orders_prod %>% inner_join(products)
df <- df %>% inner_join(products)

ud_features <- orders_prod %>%
  filter(eval_set == 'prior') %>%
  group_by(user_id, department_id) %>%
  summarise(
    ud_count = n()
  )

df <- df %>% left_join(ud_features)
rm(ud_features)
gc()

ua_features <- orders_prod %>%
  filter(eval_set == 'prior') %>%
  group_by(user_id, aisle_id) %>%
  summarise(
    ua_count = n()
  )

df <- df %>% left_join(ua_features)
rm(ua_features)
gc()

#------------------------------------------------------------------------------#
# User Features 5
#------------------------------------------------------------------------------#

u_features5 <- orders_prod %>%
  filter(eval_set == 'prior') %>%
  group_by(user_id) %>%
  summarise(
    u_organic_count = sum(prod_organic)
  )

df <- df %>% left_join(u_features5)
rm(u_features5)

df$department_id <- NULL
df$aisle_id <- NULL
df$product_name <- NULL

#------------------------------------------------------------------------------#
# User Features 6
#------------------------------------------------------------------------------#

u_features6 <- orders %>%
  filter(eval_set == 'prior') %>%
  group_by(user_id) %>%
  summarise(
    u_recent_month_orders = sum(days_gap_to_current <= 30),
    u_recent_quarter_orders = sum(days_gap_to_current <= 90),
    u_mean_days_since_prior_order = mean(days_since_prior_order),
    u_median_days_since_prior_order = median(days_since_prior_order)
  )

df <- df %>% left_join(u_features6)
rm(u_features6)
gc()

#------------------------------------------------------------------------------#
# Product Features
#------------------------------------------------------------------------------#

prod_features <- orders_prod %>%
  arrange(user_id, order_number, product_id) %>%
  group_by(user_id, product_id) %>%
  mutate(product_time = row_number()) %>%
  ungroup() %>%
  group_by(product_id) %>%
  summarise(
    prod_orders = n(),
    prod_reorders = sum(reordered),
    prod_first_orders = sum(product_time == 1),
    prod_second_orders = sum(product_time == 2)
  )

prod_features$prod_reorder_probability <- prod_features$prod_second_orders / prod_features$prod_first_orders
prod_features$prod_reorder_times <- 1 + prod_features$prod_reorders / prod_features$prod_first_orders
prod_features$prod_reorder_ratio <- prod_features$prod_reorders / prod_features$prod_orders

prod_features <- prod_features %>% select(-prod_reorders, -prod_first_orders, -prod_second_orders)
prod_features <- prod_features %>% inner_join(prod_features1)

df <- df %>% inner_join(prod_features)
rm(prod_features)

#------------------------------------------------------------------------------#
# Product Features 1
#------------------------------------------------------------------------------#

orders_prod <- orders_prod %>%
  group_by(order_id) %>%
  mutate(order_size = n())

orders_prod$add_to_cart_order_inverted <- orders_prod$order_size - orders_prod$add_to_cart_order
orders_prod$add_to_cart_order_relative <- orders_prod$add_to_cart_order/orders_prod$order_size

prod_features1 <- orders_prod %>%
  group_by(product_id) %>%
  summarise(
    prod_distinct_user = sum(reordered == 0),
    prod_mean_add_to_cart_order = mean(add_to_cart_order),
    prod_median_add_to_cart_order = median(add_to_cart_order),
    prod_mean_add_to_cart_order_inverted = mean(add_to_cart_order_inverted),
    prod_median_add_to_cart_order_inverted = median(add_to_cart_order_inverted),
    prod_mean_add_to_cart_order_relative = mean(add_to_cart_order_relative),
    prod_median_add_to_cart_order_relative = median(add_to_cart_order_relative),
    prod_mean_order_dow = mean(order_dow),
    prod_median_order_dow = median(order_dow),
    prod_mean_order_hour = mean(order_hour_of_day),
    prod_median_order_hour = median(order_hour_of_day),
    prod_mean_days_since_prior_order = mean(days_since_prior_order),
    prod_median_days_since_prior_order = median(days_since_prior_order)
  )

df <- df %>% left_join(prod_features1)
rm(pord_features1)
gc()

#------------------------------------------------------------------------------#
# User-Product Features 5
#------------------------------------------------------------------------------#

up_features5 <- orders_prod %>%
  group_by(user_id, product_id) %>%
  summarise(
    up_mean_add_to_cart_order_inverted = mean(add_to_cart_order_inverted),
    up_median_add_to_cart_order_inverted = median(add_to_cart_order_inverted),
    up_mean_add_to_cart_order_relative = mean(add_to_cart_order_relative),
    up_median_add_to_cart_order_relative = median(add_to_cart_order_relative),
    up_mean_order_dow = mean(order_dow),
    up_median_order_dow = median(order_dow),
    up_mean_order_hour = mean(order_hour_of_day),
    up_median_order_hour = median(order_hour_of_day),
    up_mean_days_since_prior_order = mean(days_since_prior_order),
    up_median_days_since_prior_order = median(days_since_prior_order)
  )

df <- df %>% left_join(up_features5)
rm(up_features5)
gc()

#------------------------------------------------------------------------------#
# Current Order Features
#------------------------------------------------------------------------------#

df <- df %>% inner_join(orders %>% filter(eval_set != 'prior'))
df$days_gap_to_current <- NULL
df$orders_gap_to_current <- NULL

#------------------------------------------------------------------------------#
# Binary Target: Reordered
#------------------------------------------------------------------------------#

ordert <- fread('./data/order_products__train.csv')

df <- df %>% left_join(
  ordert %>% filter(reordered == 1) %>%select(order_id, product_id, reordered)
  )
df$reordered[is.na(df$reordered)] = 0

#------------------------------------------------------------------------------#
# Other Features
#------------------------------------------------------------------------------#

df$ud_order_rate <- df$ud_count / df$u_total_counts
df$ua_order_rate <- df$ua_count / df$u_total_counts
df$u_organic_rate <- df$u_organic_count / df$u_total_counts

df$up_last_purchase_equal_month <- as.integer(df$up_last_order_days_gap == 30)
df$up_order_ratio <- df$up_num_orders / df$u_total_counts

df$up_recent_month_order_rate <- df$up_recent_month_reorders / df$u_recent_month_orders
df$up_recent_quarter_order_rate <- df$up_recent_quarter_reorders / df$u_recent_quarter_orders

# save(df, file = './data/df.RData')

#------------------------------------------------------------------------------#
# Save train and test dataset
#------------------------------------------------------------------------------#

train = df %>% filter(eval_set == 'train')
save(train, file = './data/train.RData')

test = df %>% filter(eval_set == 'test')
save(test, file = './data/test.RData')

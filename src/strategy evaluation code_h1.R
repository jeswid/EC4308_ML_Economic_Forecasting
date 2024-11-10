#strategy evaluation code
# Assuming 'data' contains 'predicted_prob_logit_h1' (forecasted probability of bear market)
# and 'market_state' (actual market state: 0 = Bear, 1 = Bull)

library(tidyverse)

# load predictions from all models
results_logit <- readRDS("data/logit_predictions.rds") %>%
  filter(!is.na(predicted_prob_logit_h1))

results_lasso <- readRDS("data/lasso_logit_predictions.rds") %>%
  rename(predicted_prob_lasso_h1 = h1_forecast, 
         predicted_prob_lasso_h3 = h3_forecast,
         predicted_prob_lasso_h6 = h6_forecast) %>%
  filter(!is.na(predicted_prob_lasso_h1))

results_boosting_gbm <- readRDS("data/boosting_gbm_prediction.RDS") %>%
  rename(predicted_prob_boosting_h1 = `1-step ahead forecast`, 
         predicted_prob_boosting_h3 = `3-step ahead forecast`,
         predicted_prob_boosting_h6 = `6-step ahead forecast`) %>%
  filter(!is.na(predicted_prob_boosting_h1))

results_boosting_gbm_sample_mean <- readRDS("data/boosting_gbm_sample_mean_prediction.RDS") %>%
  rename(predicted_prob_boosting_sample_mean_h1 = `1-step ahead forecast`, 
         predicted_prob_boosting_sample_mean_h3 = `3-step ahead forecast`,
         predicted_prob_boosting_sample_mean_h6 = `6-step ahead forecast`) %>%
  filter(!is.na(predicted_prob_boosting_sample_mean_h1))

results_boosting_xgb <- readRDS("data/boosting_xgb_prediction.RDS") %>%
  rename(predicted_prob_boosting_xgb_h1 = `1-step ahead forecast`, 
         predicted_prob_boosting_xgb_h3 = `3-step ahead forecast`,
         predicted_prob_boosting_xgb_h6 = `6-step ahead forecast`) %>%
  filter(!is.na(predicted_prob_boosting_xgb_h1))

results_bagging <- readRDS("data/bagging_prediction.RDS") %>%
  rename(predicted_prob_bagging_h1 = `1-step ahead predicted probability`, 
         predicted_prob_bagging_h3 = `3-step ahead predicted probability`,
         predicted_prob_bagging_h6 = `6-step ahead predicted probability`) %>%
  filter(!is.na(predicted_prob_bagging_h1))

results_random_forest <- readRDS("data/randomforest_prediction.RDS") %>%
  rename(predicted_prob_random_forest_h1 = `1-step ahead predicted probability`, 
         predicted_prob_random_forest_h3 = `3-step ahead predicted probability`,
         predicted_prob_random_forest_h6 = `6-step ahead predicted probability`) %>%
  filter(!is.na(predicted_prob_random_forest_h1))

data = results_logit %>%
  left_join(results_lasso, by = c("date" = "Date")) %>%
  left_join(results_boosting_gbm, by = c("date" = "test_date")) %>%
  left_join(results_boosting_gbm_sample_mean, by = c("date" = "test_date")) %>%
  left_join(results_boosting_xgb, by = c("date" = "test_date")) %>%
  left_join(results_bagging, by = c("date" = "DATE")) %>%
  left_join(results_random_forest, by = c("date" = "DATE"))

# Define portfolio strategy based on forecast probabilities
data$strategy_return_logit <- ifelse(
  data$predicted_prob_logit_h1 > 0.5,  # Threshold of 0.5 for investment in stocks
  data$ret,          # Invest in stocks: Use the stock return
  data$tbl         # Invest in risk-free asset: Use T-bill rate
)
data$strategy_return_lasso <- ifelse(
  data$predicted_prob_lasso_h1 > 0.5,  # Threshold of 0.5 for investment in stocks
  data$ret,          # Invest in stocks: Use the stock return
  data$tbl         # Invest in risk-free asset: Use T-bill rate
)
data$strategy_return_boosting_gbm <- ifelse(
  data$predicted_prob_boosting_h1 > 0.5,  # Threshold of 0.5 for investment in stocks
  data$ret,          # Invest in stocks: Use the stock return
  data$tbl         # Invest in risk-free asset: Use T-bill rate
)
data$strategy_return_boosting_gbm_sample_mean <- ifelse(
  data$predicted_prob_boosting_sample_mean_h1 > 0.5,  # Threshold of 0.5 for investment in stocks
  data$ret,          # Invest in stocks: Use the stock return
  data$tbl         # Invest in risk-free asset: Use T-bill rate
)
data$strategy_return_boosting_xgb <- ifelse(
  data$predicted_prob_boosting_xgb_h1 > 0.5,  # Threshold of 0.5 for investment in stocks
  data$ret,          # Invest in stocks: Use the stock return
  data$tbl         # Invest in risk-free asset: Use T-bill rate
)
data$strategy_return_bagging <- ifelse(
  data$predicted_prob_bagging_h1 > 0.5,  # Threshold of 0.5 for investment in stocks
  data$ret,          # Invest in stocks: Use the stock return
  data$tbl         # Invest in risk-free asset: Use T-bill rate
)
data$strategy_return_random_forest <- ifelse(
  data$predicted_prob_random_forest_h1 > 0.5,  # Threshold of 0.5 for investment in stocks
  data$ret,          # Invest in stocks: Use the stock return
  data$tbl         # Invest in risk-free asset: Use T-bill rate
)

# Calculate cumulative returns from the strategy
data$cum_strategy_return_logit <- cumprod(1 + data$strategy_return_logit) - 1
data$cum_strategy_return_lasso <- cumprod(1 + data$strategy_return_lasso) - 1
data$cum_strategy_return_boosting_gbm <- cumprod(1 + data$strategy_return_boosting_gbm) - 1
data$cum_strategy_return_boosting_gbm_sample_mean <- cumprod(1 + data$strategy_return_boosting_gbm_sample_mean) - 1
data$cum_strategy_return_boosting_xgb <- cumprod(1 + data$strategy_return_boosting_xgb) - 1
data$cum_strategy_return_bagging <- cumprod(1 + data$strategy_return_bagging) - 1
data$cum_strategy_return_random_forest <- cumprod(1 + data$strategy_return_random_forest) - 1

# Print summary statistics of the strategies
summary(data$cum_strategy_return_logit)
summary(data$cum_strategy_return_lasso)
summary(data$cum_strategy_return_boosting_gbm)
summary(data$cum_strategy_return_boosting_gbm_sample_mean)
summary(data$cum_strategy_return_boosting_xgb)
summary(data$cum_strategy_return_bagging)
summary(data$cum_strategy_return_random_forest)

# Plotting cumulative returns
library(ggplot2)
ggplot(data, aes(x = date)) +
  geom_line(aes(y = cum_strategy_return_logit, color = "Logit")) +
  geom_line(aes(y = cum_strategy_return_lasso, color = "LASSO Logit")) +
  #geom_line(aes(y = cum_strategy_return_boosting_gbm, color = "Boosting GBM")) +
  #geom_line(aes(y = cum_strategy_return_boosting_gbm_sample_mean, color = "Boosting GBM using sample mean")) +
  geom_line(aes(y = cum_strategy_return_boosting_xgb, color = "Boosting XGB")) +
  geom_line(aes(y = cum_strategy_return_bagging, color = "Bagging")) +
  geom_line(aes(y = cum_strategy_return_random_forest, color = "Random Forest")) +
  labs(title = "Cumulative Portfolio Returns Using the 50% Rule", y = "Cumulative Return", x = "Date", color = "Model") +
  theme_minimal()

# Use sample average threshold of the training set instead of 50%
data_full = readRDS("data/final_cleaned_data_with_bull_bear.rds")
data_train = data_full[17:438,]
sample_avg_threshold <- mean(data_train$market_state)  # Calculate sample average of bear markets

data$strategy_return_avg_logit <- ifelse(
  data$predicted_prob_logit_h1 > sample_avg_threshold,
  data$ret,
  data$tbl
)
data$strategy_return_avg_lasso <- ifelse(
  data$predicted_prob_lasso_h1 > sample_avg_threshold,
  data$ret,
  data$tbl
)
data$strategy_return_avg_boosting_gbm <- ifelse(
  data$predicted_prob_boosting_h1 > sample_avg_threshold,
  data$ret,
  data$tbl
)
data$strategy_return_avg_boosting_sample_mean <- ifelse(
  data$predicted_prob_boosting_sample_mean_h1 > sample_avg_threshold,
  data$ret,
  data$tbl
)
data$strategy_return_avg_boosting_xgb <- ifelse(
  data$predicted_prob_boosting_xgb_h1 > sample_avg_threshold,
  data$ret,
  data$tbl
)
data$strategy_return_avg_bagging <- ifelse(
  data$predicted_prob_bagging_h1 > sample_avg_threshold,
  data$ret,
  data$tbl
)
data$strategy_return_avg_random_forest <- ifelse(
  data$predicted_prob_random_forest_h1 > sample_avg_threshold,
  data$ret,
  data$tbl
)

# Calculate cumulative returns using sample average threshold strategy
data$cum_strategy_return_avg_logit <- cumprod(1 + data$strategy_return_avg_logit) - 1
data$cum_strategy_return_avg_lasso <- cumprod(1 + data$strategy_return_avg_lasso) - 1
data$cum_strategy_return_avg_boosting_gbm <- cumprod(1 + data$strategy_return_avg_boosting_gbm) - 1
data$cum_strategy_return_avg_boosting_gbm_sample_mean <- cumprod(1 + data$strategy_return_avg_boosting_sample_mean) - 1
data$cum_strategy_return_avg_boosting_xgb <- cumprod(1 + data$strategy_return_avg_boosting_xgb) - 1
data$cum_strategy_return_avg_bagging <- cumprod(1 + data$strategy_return_avg_bagging) - 1
data$cum_strategy_return_avg_random_forest <- cumprod(1 + data$strategy_return_avg_random_forest) - 1

# Print summary statistics of the strategies
summary(data$cum_strategy_return_avg_logit)
summary(data$cum_strategy_return_avg_lasso)
summary(data$cum_strategy_return_avg_boosting_gbm)
summary(data$cum_strategy_return_avg_boosting_gbm_sample_mean)
summary(data$cum_strategy_return_avg_boosting_xgb)
summary(data$cum_strategy_return_avg_bagging)
summary(data$cum_strategy_return_avg_random_forest)

# Plotting cumulative returns
library(ggplot2)
ggplot(data, aes(x = date)) +
  geom_line(aes(y = cum_strategy_return_avg_logit, color = "Logit")) +
  geom_line(aes(y = cum_strategy_return_avg_lasso, color = "LASSO Logit")) +
  #geom_line(aes(y = cum_strategy_return_avg_boosting_gbm, color = "Boosting GBM")) +
  #geom_line(aes(y = cum_strategy_return_avg_boosting_gbm_sample_mean, color = "Boosting GBM using sample mean")) +
  geom_line(aes(y = cum_strategy_return_avg_boosting_xgb, color = "Boosting XGB")) +
  geom_line(aes(y = cum_strategy_return_avg_bagging, color = "Bagging")) +
  geom_line(aes(y = cum_strategy_return_avg_random_forest, color = "Random Forest")) +
  labs(title = "Cumulative Portfolio Returns Using Sample Average Threshold", y = "Cumulative Return", x = "Date") +
  theme_minimal()

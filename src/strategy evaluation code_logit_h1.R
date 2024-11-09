#strategy evaluation code
# Assuming 'data' contains 'predicted_prob_logit_h1' (forecasted probability of bear market)
# and 'market_state' (actual market state: 0 = Bear, 1 = Bull)

results <- readRDS("data/logit_predictions.rds") %>%
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

#results_bagging_h1 <- data %>%
#  filter(!is.na(predicted_prob_logit_h1))

data = results %>%
  left_join(results_lasso, by = c("date" = "Date")) %>%
  left_join(results_boosting_gbm, by = c("date" = "test_date"))

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

# Calculate cumulative returns from the strategy
data$cum_strategy_return_logit <- cumprod(1 + data$strategy_return_logit) - 1
data$cum_strategy_return_lasso <- cumprod(1 + data$strategy_return_lasso) - 1

# Optional: Use sample average threshold instead of 50%
sample_avg_threshold <- mean(data$market_state)  # Calculate sample average of bear markets

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

# Calculate cumulative returns using sample average threshold strategy
data$cum_strategy_return_avg_logit <- cumprod(1 + data$strategy_return_avg_logit) - 1
data$cum_strategy_return_avg_lasso <- cumprod(1 + data$strategy_return_avg_lasso) - 1

# Print summary statistics of the strategies
summary(data$cum_strategy_return_logit)
summary(data$cum_strategy_return_lasso)
summary(data$cum_strategy_return_avg_logit)
summary(data$cum_strategy_return_avg_lasso)

# Plotting cumulative returns
library(ggplot2)
ggplot(data, aes(x = date)) +
  geom_line(aes(y = cum_strategy_return_logit, color = "Logit 50% Threshold")) +
  geom_line(aes(y = cum_strategy_return_lasso, color = "LASSO 50% Threshold")) +
  geom_line(aes(y = cum_strategy_return_avg_logit, color = "Logit Sample Average Threshold")) +
  geom_line(aes(y = cum_strategy_return_avg_lasso, color = "LASSO Sample Average Threshold")) +
  labs(title = "Cumulative Portfolio Returns", y = "Cumulative Return", x = "Date") +
  theme_minimal()

#using the two sided moving average threshold
# 1. Compute the two-sided moving average of the predicted probabilities
data$ma_threshold_logit <- rollapply(
  data$predicted_prob_logit_h1, 
  width = 12,  # Example: 12-month moving average
  FUN = mean, 
  fill = NA, 
  align = "center"  # Two-sided moving average
)
data$ma_threshold_lasso <- rollapply(
  data$predicted_prob_lasso_h1, 
  width = 12,  # Example: 12-month moving average
  FUN = mean, 
  fill = NA, 
  align = "center"  # Two-sided moving average
)

# 2. Define the strategy: Buy stocks if predicted_prob_logit_h1 < ma_threshold
data <- data %>%
  filter(!is.na(ma_threshold_lasso))
data$strategy_return_2sided_avg_logit <- ifelse(
  data$predicted_prob_logit_h1 > data$ma_threshold_logit, 
  data$ret,  # Buy stocks
  data$tbl  # Switch to risk-free asset
)
data$strategy_return_2sided_avg_lasso <- ifelse(
  data$predicted_prob_lasso_h1 > data$ma_threshold_lasso, 
  data$ret,  # Buy stocks
  data$tbl  # Switch to risk-free asset
)

# 3. Calculate cumulative returns for the strategy
data$cum_strategy_return_2sided_avg_logit <- cumprod(1 + data$strategy_return_2sided_avg_logit) - 1
data$cum_strategy_return_2sided_avg_lasso <- cumprod(1 + data$strategy_return_2sided_avg_lasso) - 1

# 4. Plot the cumulative returns
library(ggplot2)
ggplot(data, aes(x = date)) +
  geom_line(aes(y = cum_strategy_return_2sided_avg_logit, color = "Logit Strategy with MA Threshold")) +
  geom_line(aes(y = cum_strategy_return_2sided_avg_lasso, color = "LASSO Strategy with MA Threshold")) +
  labs(title = "Cumulative Portfolio Returns with MA Threshold",
       y = "Cumulative Return", x = "Date") +
  theme_minimal()

# 5. Print summary of cumulative strategy returns
summary(data$cum_strategy_return_2sided_avg_logit)
summary(data$cum_strategy_return_2sided_avg_lasso)
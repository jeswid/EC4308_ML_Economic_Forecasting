#strategy evaluation code
# Assuming 'data' contains 'predicted_prob_logit_h1' (forecasted probability of bear market)
# and 'market_state' (actual market state: 1 = Bear, 0 = Bull)

data <- data %>%
  filter(!is.na(predicted_prob_logit_h1))

# Define portfolio strategy based on forecast probabilities
data$strategy_return <- ifelse(
  data$predicted_prob_logit_h1 < 0.5,  # Threshold of 0.5 for investment in stocks
  data$ret,          # Invest in stocks: Use the stock return
  data$tbl         # Invest in risk-free asset: Use T-bill rate
)

# Calculate cumulative returns from the strategy
data$cum_strategy_return <- cumprod(1 + data$strategy_return) - 1

# Optional: Use sample average threshold instead of 50%
sample_avg_threshold <- mean(data$market_state)  # Calculate sample average of bear markets

data$strategy_return_avg <- ifelse(
  data$predicted_prob_logit_h1 < sample_avg_threshold,
  data$ret,
  data$tbl
)

# Calculate cumulative returns using sample average threshold strategy
data$cum_strategy_return_avg <- cumprod(1 + data$strategy_return_avg) - 1

# Print summary statistics of the strategies
summary(data$cum_strategy_return)
summary(data$cum_strategy_return_avg)

# Plotting cumulative returns
library(ggplot2)
ggplot(data, aes(x = date)) +
  geom_line(aes(y = cum_strategy_return, color = "50% Threshold")) +
  geom_line(aes(y = cum_strategy_return_avg, color = "Sample Average Threshold")) +
  labs(title = "Cumulative Portfolio Returns", y = "Cumulative Return", x = "Date") +
  theme_minimal()

#using the two sided moving average threshold
# 1. Compute the two-sided moving average of the predicted probabilities
data$ma_threshold <- rollapply(
  data$predicted_prob_logit_h1, 
  width = 12,  # Example: 12-month moving average
  FUN = mean, 
  fill = NA, 
  align = "center"  # Two-sided moving average
)

# 2. Define the strategy: Buy stocks if predicted_prob_logit_h1 < ma_threshold
data <- data %>%
  filter(!is.na(ma_threshold))
data$strategy_return_2sided_avg <- ifelse(
  data$predicted_prob_logit_h1 < data$ma_threshold, 
  data$ret,  # Buy stocks
  data$tbl  # Switch to risk-free asset
)

# 3. Calculate cumulative returns for the strategy
data$cum_strategy_return_2sided_avg <- cumprod(1 + data$strategy_return) - 1

# 4. Plot the cumulative returns
library(ggplot2)
ggplot(data, aes(x = date)) +
  geom_line(aes(y = cum_strategy_return_2sided_avg, color = "Strategy with MA Threshold")) +
  labs(title = "Cumulative Portfolio Returns with MA Threshold",
       y = "Cumulative Return", x = "Date") +
  theme_minimal()

# 5. Print summary of cumulative strategy returns
summary(data$cum_strategy_return_2sided_avg)

results_logit_h1 = data
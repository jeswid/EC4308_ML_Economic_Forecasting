#strategy evaluation code
# Assuming 'df' contains 'predicted_prob' (forecasted probability of bear market)
# and 'market_state' (actual market state: 1 = Bear, 0 = Bull)

# Define portfolio strategy based on forecast probabilities
df$strategy_return <- ifelse(
  df$predicted_prob < 0.5,  # Threshold of 0.5 for investment in stocks
  df$ret,          # Invest in stocks: Use the stock return
  df$tbl         # Invest in risk-free asset: Use T-bill rate
)

# Calculate cumulative returns from the strategy
df$cum_strategy_return <- cumprod(1 + df$strategy_return) - 1

# Optional: Use sample average threshold instead of 50%
sample_avg_threshold <- mean(df$market_state)  # Calculate sample average of bear markets

df$strategy_return_avg <- ifelse(
  df$predicted_prob < sample_avg_threshold,
  df$ret,
  df$tbl
)

# Calculate cumulative returns using sample average threshold strategy
df$cum_strategy_return_avg <- cumprod(1 + df$strategy_return_avg) - 1

# Print summary statistics of the strategies
summary(df$cum_strategy_return)
summary(df$cum_strategy_return_avg)

# Plotting cumulative returns
library(ggplot2)
ggplot(df, aes(x = date)) +
  geom_line(aes(y = cum_strategy_return, color = "50% Threshold")) +
  geom_line(aes(y = cum_strategy_return_avg, color = "Sample Average Threshold")) +
  labs(title = "Cumulative Portfolio Returns", y = "Cumulative Return", x = "Date") +
  theme_minimal()

#using the two sided moving average threshold
# 1. Compute the two-sided moving average of the predicted probabilities
df$ma_threshold <- rollapply(
  df$predicted_prob, 
  width = 12,  # Example: 12-month moving average
  FUN = mean, 
  fill = NA, 
  align = "center"  # Two-sided moving average
)

# 2. Define the strategy: Buy stocks if predicted_prob < ma_threshold
df$strategy_return_2sided_avg <- ifelse(
  df$predicted_prob < df$ma_threshold, 
  df$stock_return,  # Buy stocks
  df$risk_free_rate  # Switch to risk-free asset
)

# 3. Calculate cumulative returns for the strategy
df$cum_strategy_return_2sided_avg <- cumprod(1 + df$strategy_return) - 1

# 4. Plot the cumulative returns
library(ggplot2)
ggplot(df, aes(x = date)) +
  geom_line(aes(y = cum_strategy_return_2sided_avg, color = "Strategy with MA Threshold")) +
  labs(title = "Cumulative Portfolio Returns with MA Threshold",
       y = "Cumulative Return", x = "Date") +
  theme_minimal()

# 5. Print summary of cumulative strategy returns
summary(df$cum_strategy_return_2sided_avg)
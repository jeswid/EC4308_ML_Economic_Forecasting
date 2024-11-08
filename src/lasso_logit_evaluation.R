library(hdm)
library(dplyr)
library(caret)

rm(list = ls())

eval_model <- function(n) {
  data <- readRDS("data/lasso_logit_predictions.rds")
  data2 <- readRDS("data/final_cleaned_data_with_bull_bear.RDS")
  data <- inner_join(data, data2, by = c("Date" = "DATE"))
  data <- data %>% filter(!is.na(.data[[n]]))
  
  data$strategy_return <- ifelse(
    data[n] > 0.5,  # Threshold of 0.5 for investment in stocks
    data$ret,          # Invest in stocks: Use the stock return
    data$tbl         # Invest in risk-free asset: Use T-bill rate
  )
  
  data$cum_strategy_return <- cumprod(1 + data$strategy_return) - 1
  
  # Optional: Use sample average threshold instead of 50%
  sample_avg_threshold <- mean(data$market_state)  # Calculate sample average of bear markets
  
  data$strategy_return_avg <- ifelse(
    data[n] < sample_avg_threshold,
    data$ret,
    data$tbl
  )
  
  data$cum_strategy_return_avg <- cumprod(1 + data$strategy_return_avg) - 1
  
  # Print summary statistics of the strategies
  print(summary(data$cum_strategy_return))
  print(summary(data$cum_strategy_return_avg))
  
  strat1_return = data[nrow(data), "cum_strategy_return"]
  strat2_return = data[nrow(data), "cum_strategy_return_avg"]
  
  data$pred_binary <- ifelse(data[,n] < 0.5, 0, 1)
  conmat <- confusionMatrix(data = as.factor(data$pred_binary), reference = as.factor(data$market_state), mode = "everything")
  print(conmat)
  return(list(strat1_return, strat2_return))
}

print(eval_model("h1_forecast"))
print(eval_model("h3_forecast"))
print(eval_model("h6_forecast"))



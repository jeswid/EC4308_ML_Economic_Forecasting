library(hdm)
library(dplyr)
library(caret)

rm(list = ls())

#new 
eval_model <- function(n) {
  data <- readRDS("data/logit_predictions.rds") %>%
    select(date, predicted_prob_logit_h1, predicted_prob_logit_h3, predicted_prob_logit_h6) %>%
    filter(!is.na(predicted_prob_logit_h1))
  data2 <- readRDS("data/final_cleaned_data_with_bull_bear.RDS")
  data <- inner_join(data, data2, by = c("date" = "DATE"))
  data <- data %>% filter(!is.na(.data[[n]]))
  
  data$strategy_return <- ifelse(
    data[[n]] > 0.5,  # Threshold of 0.5 for investment in stocks
    data$ret,          # Invest in stocks: Use the stock return
    data$tbl         # Invest in risk-free asset: Use T-bill rate
  )
  
  data$cum_strategy_return <- cumprod(1 + data$strategy_return) - 1
  
  # Optional: Use sample average threshold instead of 50%
  data_full = readRDS("data/final_cleaned_data_with_bull_bear.rds")
  data_train = data_full[17:438,]
  sample_avg_threshold <- 1- mean(data_train$market_state)
  
  data$strategy_return_avg <- ifelse(
    data[[n]] > sample_avg_threshold,
    data$ret,
    data$tbl
  )
  
  data$cum_strategy_return_avg <- cumprod(1 + data$strategy_return_avg) - 1
  
  # Print summary statistics of the strategies
  print(summary(data$cum_strategy_return))
  print(summary(data$cum_strategy_return_avg))
  
  strat1_return <- data[nrow(data), "cum_strategy_return"]
  strat2_return <- data[nrow(data), "cum_strategy_return_avg"]
  
  # Ensure factor levels are aligned
  data$pred_binary <- factor(ifelse(data[[n]] < 0.5, 0, 1), levels = c(0, 1))
  data$market_state <- factor(data$market_state, levels = c(0, 1))
  
  conmat <- confusionMatrix(data = data$pred_binary, reference = data$market_state, mode = "everything")
  print(conmat)
  return(list(strat1_return, strat2_return))
}

print(eval_model("predicted_prob_logit_h1"))
print(eval_model("predicted_prob_logit_h3"))
print(eval_model("predicted_prob_logit_h6"))
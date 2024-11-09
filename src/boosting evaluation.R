rm(list=ls())
library(caret)

# Get real Y 
data = readRDS("data/final_cleaned_data_with_bull_bear.RDS")
data$DATE <- as.Date(data$DATE)
# Sort the data by date
data <- data[order(data$DATE), ]
data <- data[-(1:17), ]  # align for missing data due to 17 lags of Y
data = head(data, -6) # remove last 6 rows due to inaccurate market state
Y = data$market_state
actual = tail(Y, 150)

# read in prediction 
gbm_predict = readRDS("data/boosting_gbm_prediction.RDS")
gbm_samplemean_predict = readRDS("data/boosting_gbm_sample_mean_prediction.RDS")
xgb_predict = readRDS("data/boosting_xgb_prediction.RDS")

# calculate F1 score
for (i in 2:4) {
  predicted = ifelse(gbm_predict[, i]>=0.5, 1, 0)
  conf_matrix = confusionMatrix(as.factor(predicted), as.factor(actual), positive = "1")
  f1_score =  2 * (as.numeric(conf_matrix$byClass['Precision']) * as.numeric(conf_matrix$byClass['Recall'])) / (as.numeric(conf_matrix$byClass['Precision']) + as.numeric(conf_matrix$byClass['Recall']))
  print(f1_score)
}

for (i in 2:4) {
  predicted = ifelse(gbm_samplemean_predict[, i]>=0.5, 1, 0)
  conf_matrix = confusionMatrix(as.factor(predicted), as.factor(actual), positive = "1")
  f1_score =  2 * (as.numeric(conf_matrix$byClass['Precision']) * as.numeric(conf_matrix$byClass['Recall'])) / (as.numeric(conf_matrix$byClass['Precision']) + as.numeric(conf_matrix$byClass['Recall']))
  print(f1_score)
}

for (i in 2:4) {
  predicted = ifelse(xgb_predict[, i]>=0.5, 1, 0)
  conf_matrix = confusionMatrix(as.factor(predicted), as.factor(actual), positive = "1")
  f1_score =  2 * (as.numeric(conf_matrix$byClass['Precision']) * as.numeric(conf_matrix$byClass['Recall'])) / (as.numeric(conf_matrix$byClass['Precision']) + as.numeric(conf_matrix$byClass['Recall']))
  print(f1_score)
}


gbm_data = gbm_predict %>%
  left_join(data, by = c("test_date" = "DATE")) %>%
  rename(predicted_prob_boosting_h1 = "1-step ahead forecast")

gbm_data$strategy_return_boosting_gbm <- ifelse(
  gbm_data$predicted_prob_boosting_h1 > 0.5,  # Threshold of 0.5 for investment in stocks
  gbm_data$ret,          # Invest in stocks: Use the stock return
  gbm_data$tbl         # Invest in risk-free asset: Use T-bill rate
)

gbm_data$cum_strategy_return_boosting_gbm <- cumprod(1 + gbm_data$strategy_return_boosting_gbm) - 1


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
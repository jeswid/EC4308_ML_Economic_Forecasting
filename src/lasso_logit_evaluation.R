library(dplyr)
library(caret)

rm(list = ls())

eval_model <- function(n) {
  data <- readRDS("lasso_logit_predictions.rds")
  data2 <- readRDS("final_cleaned_data_with_bull_bear.RDS")
  data <- inner_join(data, data2, by = c("Date" = "DATE"))
  data$pred_binary <- ifelse(data[[n]] > 0.5, 0, 1)
  conmat <- confusionMatrix(data = as.factor(data$pred_binary), reference = as.factor(data$market_state), positive = "1", mode = "everything")
  print(conmat)
}

eval_model("h1_forecast")
eval_model("h3_forecast")
eval_model("h6_forecast")

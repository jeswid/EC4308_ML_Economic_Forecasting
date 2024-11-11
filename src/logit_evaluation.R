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
  
  # Ensure factor levels are aligned
  data$pred_binary <- factor(ifelse(data[[n]] < 0.5, 0, 1), levels = c(0, 1))
  data$market_state <- factor(data$market_state, levels = c(0, 1))
  
  conmat <- confusionMatrix(data = data$pred_binary, reference = data$market_state, mode = "everything", positive = "1")
  print(conmat)
  return(list(strat1_return, strat2_return))
}

print(eval_model("predicted_prob_logit_h1"))
print(eval_model("predicted_prob_logit_h3"))
print(eval_model("predicted_prob_logit_h6"))

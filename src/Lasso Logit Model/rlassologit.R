library(hdm)
library(dplyr)

rm(list = ls())

data <- readRDS("final_cleaned_data_with_bull_bear.RDS")

# Filter out all real variables
data <- data[, !grepl("real", names(data), ignore.case = TRUE)]

#Only TR_CAPE used?
#Use "excess_CAPE_yield?
data <- data %>% select(-"CAPE", -"excess_CAPE_yield")

#Filter out all rows with N.A
data <- data[13:nrow(data),]

test_size <- 150

date <- data.frame(date = data$DATE)
date <- date[(nrow(data) - test_size + 1):nrow(data),]

predicted_probs_logit_lasso_logit_h1 <- list()
predicted_probs_logit_lasso_logit_h3 <- list()
predicted_probs_logit_lasso_logit_h6 <- list()

#All the h-step ahead forecast is made so that the first forecast starts at the same time period
#So we would have 145 total observations if we use all

for (i in 1:150) {
  #1 Step ahead forecast
  #Split data according to window
  train_set_h1 <- data[i:(nrow(data) - test_size - 6 + (i-1)), ] #Minus 6 due to future values
  test_set_h1 <- data[(nrow(data) - test_size + 1+ (i-1)):nrow(data), ]

  #Train model on window and predict using optimal lambda
  model_h1 <- rlassologit(market_state ~ . - DATE, data = train_set_h1)
  optimal_lambda_h1 <- model_h1$lambda
  predictions_h1 <- predict(model_h1, newdata = test_set_h1, type = "response", lambda = optimal_lambda_h1)

  #Store 1 step ahead forecast in list
  predicted_probs_logit_lasso_logit_h1[i] <- predictions_h1[1]

  #3 Step ahead forecast
  #Split data according to window
  train_set_h3 <- data[i:(nrow(data) - test_size - 6 + (i-1) - 2), ]
  test_set_h3 <- data[(nrow(data) - test_size + 1+ (i-1)):nrow(data), ]

  #Train model on window and predict using optimal lambda
  model_h3 <- rlassologit(market_state ~ . - DATE, data = train_set_h3)
  optimal_lambda_h3 <- model_h3$lambda
  predictions_h3 <- predict(model_h3, newdata = test_set_h3, type = "response", lambda = optimal_lambda_h3)

  #Store 3 step ahead forecast in list
  predicted_probs_logit_lasso_logit_h3[i] <- predictions_h3[3]

  #6 Step ahead forecast
  #Split data according to window
  train_set_h6 <- data[i:(nrow(data) - test_size - 6 + (i-1) - 5), ]
  test_set_h6 <- data[(nrow(data) - test_size + 1+ (i-1)):nrow(data), ]

  #Train model on window and predict using optimal lambda
  model_h6 <- rlassologit(market_state ~ . - DATE, data = train_set_h6)
  optimal_lambda_h6 <- model_h6$lambda
  predictions_h6 <- predict(model_h6, newdata = test_set_h6, type = "response", lambda = optimal_lambda_h6)

  #Store 6 step ahead forecast in list
  predicted_probs_logit_lasso_logit_h6[i] <- predictions_h1[6]
}

predictions = cbind(predicted_probs_logit_lasso_logit_h1,
                    predicted_probs_logit_lasso_logit_h3,
                    predicted_probs_logit_lasso_logit_h6)

dfpred <- as.data.frame(predictions)

lassologitprediction = cbind(date,
                             dfpred)

saveRDS(lassologitprediction, file = "lassologitpredictions.rds")

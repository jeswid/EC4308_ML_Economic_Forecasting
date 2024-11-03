library(hdm)
library(dplyr)

rm(list = ls())

data <- readRDS("final_cleaned_data_with_bull_bear.RDS")

data <- data[order(data$DATE), ]

data <- data[-(1:12), ]

# Filter out all real variables
data <- data[, !grepl("real", names(data), ignore.case = TRUE)]


test_size <- 150

predicted <- data.frame(
  h1_forecast = double(),
  h3_forecast = double(),
  h6_forecast = double()
)

date <- data.frame(date = data$DATE)
date <- date[(nrow(data) - test_size + 1):nrow(data),]

#Only useTR_CAPE
X_h1 = data %>% 
  select(starts_with("lag"), "market_state", "DATE") %>% # remove present values 
  select(-starts_with("lag7_"), -starts_with("lag8_"), -starts_with("lag9_"), -starts_with("lag10_"), -starts_with("lag11_"), -ends_with("CAPE"), ends_with("market_state")) %>% # keep lag 1-6 for X variables, but keep all lags of Y first
  select(-c(lag1_market_state, lag2_market_state, lag3_market_state, lag4_market_state, lag5_market_state, lag6_market_state, # remove first 6 lags of Y
            ends_with("excess_CAPE_yield")))

X_h3 = data %>% 
  select(starts_with("lag"), "market_state","DATE") %>% # remove present values 
  select(-starts_with("lag1_")) %>% select(-starts_with("lag2_")) %>% 
  select(-starts_with("lag9_"), -starts_with("lag10_"), -starts_with("lag11_"), -ends_with("CAPE"), ends_with("market_state")) %>% # keep lag 3-8 for X variables, but keep all lags of Y first
  select(-c(lag3_market_state, lag4_market_state, lag5_market_state, lag6_market_state, # remove first 6 lags of Y
            ends_with("excess_CAPE_yield")))

X_h6 = data %>% 
  select(starts_with("lag"), "market_state","DATE") %>% # remove present values 
  select(-starts_with("lag1_"), -starts_with("lag2_"), -starts_with("lag3_"), -starts_with("lag4_"), -starts_with("lag5_"), -ends_with("CAPE"), ends_with("market_state")) %>% # keep lag 6-11 for X variables, but keep all lags of Y first
  select(-c(lag1_market_state, lag2_market_state, lag3_market_state, lag4_market_state, lag5_market_state, lag6_market_state, # remove first 6 lags of Y
            ends_with("excess_CAPE_yield")))

for (i in 1:150) {
  #1 Step ahead forecast
  #Split data according to window
  train_set_h1 <- X_h1[i:(nrow(X_h1) - test_size + (i-1)), ]
  test_set_h1 <- X_h1[(nrow(X_h1) - test_size + 1 + (i-1)):nrow(X_h1), ]

  #Train model on window and predict using optimal lambda
  model_h1 <- rlassologit(market_state ~ . - DATE, data = train_set_h1)
  optimal_lambda_h1 <- model_h1$lambda
  predictions_h1 <- predict(model_h1, newdata = test_set_h1, type = "response", lambda = optimal_lambda_h1)

  #Store 1 step ahead forecast in list
  predicted[i,"h1_forecast"] <- predictions_h1[1]
  
  #3 Step ahead forecast
  #Split data according to window
  train_set_h3 <- X_h3[i:(nrow(X_h3) - test_size + (i-1)), ]
  test_set_h3 <- X_h3[(nrow(X_h3) - test_size + 1 + (i-1)):nrow(X_h3), ]
  
  #Train model on window and predict using optimal lambda
  model_h3 <- rlassologit(market_state ~ . - DATE, data = train_set_h3)
  optimal_lambda_h3 <- model_h3$lambda
  predictions_h3 <- predict(model_h3, newdata = test_set_h3, type = "response", lambda = optimal_lambda_h3)
  
  #Store 1 step ahead forecast in list
  predicted[i,"h3_forecast"] <- predictions_h3[1]
  
  #3 Step ahead forecast
  #Split data according to window
  train_set_h6 <- X_h6[i:(nrow(X_h6) - test_size + (i-1)), ]
  test_set_h6 <- X_h6[(nrow(X_h6) - test_size + 1 + (i-1)):nrow(X_h6), ]
  
  #Train model on window and predict using optimal lambda
  model_h6 <- rlassologit(market_state ~ . - DATE, data = train_set_h6)
  optimal_lambda_h6 <- model_h6$lambda
  predictions_h6 <- predict(model_h6, newdata = test_set_h6, type = "response", lambda = optimal_lambda_h6)
  
  #Store 1 step ahead forecast in list
  predicted[i,"h6_forecast"] <- predictions_h6[1]
}

predicted$Date <- date

saveRDS(predicted, file = "lasso_logit_predictions.rds")
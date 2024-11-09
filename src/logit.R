# Set seed for reproducibility
set.seed(42)

# Load dataset
data <- readRDS("data/final_cleaned_data_with_bull_bear.RDS")
data$date <- as.Date(data$DATE)

# Sort the data by date
data <- data[order(data$date), ]

# Define sizes for test, and training windows excluding the last 6 observations of the test set
#but keep the overall size of the test set to 150 (means discard the last 6 obsv of rows)
n_test <- 150
n_train <- 438  # for rolling window

# Initialize an empty list to store each row of predictions
predicted_probs_logit_h1 <- list()
predicted_probs_logit_h3 <- list()
predicted_probs_logit_h6 <- list()

# Rolling Window Loop
for (start in seq(1, nrow(data) - n_train - n_test + 1)) {
  
  # Define rolling window train and test data
  train_data <- data[start:(start + n_train - 1), ]
  test_data <- data[(start + n_train):(start + n_train + n_test - 1), ]
  
  # Logistic regression models for h1, h3, and h6
  logit_h1 <- glm(market_state ~ lag1_tms + lag1_ret + lag1_infl + lag1_lty, data = train_data, family = binomial(link = "logit"))
  logit_h3 <- glm(market_state ~ lag3_tms + lag3_ret + lag3_infl + lag3_lty, data = train_data, family = binomial(link = "logit"))
  logit_h6 <- glm(market_state ~ lag6_tms + lag6_ret + lag6_infl + lag6_lty, data = train_data, family = binomial(link = "logit"))
  
  # Make predictions for each horizon
  pred_h1 <- predict(logit_h1, newdata = test_data, type = "response")
  pred_h3 <- predict(logit_h3, newdata = test_data, type = "response")
  pred_h6 <- predict(logit_h6, newdata = test_data, type = "response")
  
  # Store predictions as individual rows in each list
  for (i in seq_len(nrow(test_data))) {
    predicted_probs_logit_h1[[length(predicted_probs_logit_h1) + 1]] <- data.frame(date = test_data$date[i], prob = pred_h1[i])
    predicted_probs_logit_h3[[length(predicted_probs_logit_h3) + 1]] <- data.frame(date = test_data$date[i], prob = pred_h3[i])
    predicted_probs_logit_h6[[length(predicted_probs_logit_h6) + 1]] <- data.frame(date = test_data$date[i], prob = pred_h6[i])
  }
}

# Combine lists into data frames for each horizon
predicted_probs_logit_h1_df <- do.call(rbind, predicted_probs_logit_h1)
predicted_probs_logit_h3_df <- do.call(rbind, predicted_probs_logit_h3)
predicted_probs_logit_h6_df <- do.call(rbind, predicted_probs_logit_h6)


# Join predictions back to the original dataset if desired
data <- data %>%
  left_join(predicted_probs_logit_h1_df, by = "date") %>%
  rename(predicted_prob_logit_h1 = prob) %>%
  left_join(predicted_probs_logit_h3_df, by = "date") %>%
  rename(predicted_prob_logit_h3 = prob) %>%
  left_join(predicted_probs_logit_h6_df, by = "date") %>%
  rename(predicted_prob_logit_h6 = prob)

# Save predicted values as RDS file
saveRDS(data, file = "data/logit_predictions.rds")

# Output dimensions of each prediction set for verification
cat("H1 Predictions:", nrow(predicted_probs_logit_h1_df), "\n")
cat("H3 Predictions:", nrow(predicted_probs_logit_h3_df), "\n")
cat("H6 Predictions:", nrow(predicted_probs_logit_h6_df), "\n")
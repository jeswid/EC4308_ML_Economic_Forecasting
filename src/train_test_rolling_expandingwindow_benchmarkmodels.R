# Set seed for reproducibility
set.seed(42)

# Load dataset
data <- readRDS("data/final_cleaned_data_with_bull_bear.RDS")
data$date <- as.Date(data$DATE)

# Sort the data by date
data <- data[order(data$date), ]

# Define sizes for test, validation, and training windows
n_test <- 150
n_validation <- 100
train_window_size <- 344  # for rolling window

# Initialize empty lists to store predictions for each model
predicted_probs_logit_h1 <- list()
predicted_probs_logit_h3 <- list()
predicted_probs_logit_h6 <- list()

# Rolling Window Loop
for (start in seq(1, (nrow(data) - n_test - n_validation - train_window_size), by = 10)) {
  
  # Define rolling window train and test data
  train_data <- data[start:(start + train_window_size - 1), ]
  validation_data <- data[(start + train_window_size):(start + train_window_size + n_validation - 1), ]
  test_data <- data[(start + train_window_size + n_validation):(start + train_window_size + n_validation + n_test - 1), ]
  
  # Logistic regression models for h1, h3, and h6
  logit_h1 <- glm(market_state ~ tms + lag1_ret + infl + lty, data = train_data, family = binomial(link = "logit"))
  logit_h3 <- glm(market_state ~ tms + lag4_ret + infl + lty, data = train_data, family = binomial(link = "logit"))
  logit_h6 <- glm(market_state ~ tms + lag7_ret + infl + lty, data = train_data, family = binomial(link = "logit"))
  
  # Make predictions for each horizon and store
  pred_h1 <- predict(logit_h1, newdata = test_data, type = "response")
  pred_h3 <- predict(logit_h3, newdata = test_data, type = "response")
  pred_h6 <- predict(logit_h6, newdata = test_data, type = "response")
  
  predicted_probs_logit_h1[[length(predicted_probs_logit_h1) + 1]] <- data.frame(date = test_data$date, prob = pred_h1)
  predicted_probs_logit_h3[[length(predicted_probs_logit_h3) + 1]] <- data.frame(date = test_data$date, prob = pred_h3)
  predicted_probs_logit_h6[[length(predicted_probs_logit_h6) + 1]] <- data.frame(date = test_data$date, prob = pred_h6)
}

# Combine predictions into data frames for each horizon
predicted_probs_logit_h1_df <- do.call(rbind, predicted_probs_logit_h1)
predicted_probs_logit_h3_df <- do.call(rbind, predicted_probs_logit_h3)
predicted_probs_logit_h6_df <- do.call(rbind, predicted_probs_logit_h6)

# Join predictions back to the original dataset if desired
data <- data %>%
  left_join(predicted_probs_logit_h1_df, by = "date") %>%
  left_join(predicted_probs_logit_h3_df, by = "date") %>%
  left_join(predicted_probs_logit_h6_df, by = "date")

# Output dimensions of each prediction set for verification
cat("H1 Predictions:", nrow(predicted_probs_logit_h1_df), "\n")
cat("H3 Predictions:", nrow(predicted_probs_logit_h3_df), "\n")
cat("H6 Predictions:", nrow(predicted_probs_logit_h6_df), "\n")

# Set seed for reproducibility
set.seed(42)

library(dplyr)

# Load dataset
data <- readRDS("data/final_cleaned_data_with_bull_bear.RDS")
data$date <- as.Date(data$DATE)

# Sort the data by date
data <- data[order(data$date), ]

# Remove initial rows and last 6 rows for each horizon dataset
data <- data[18:(nrow(data) - 6), ]  #standardise train set w ml model train test split (cut first 17 rows)

# Define the test set size and calculate training set size based on available data
n_test <- 150
n_train <- nrow(data) - n_test

# Initialize lists to store only the final test set predictions
final_predicted_probs_logit_h1 <- list()
final_predicted_probs_logit_h3 <- list()
final_predicted_probs_logit_h6 <- list()

# Rolling Window Loop
for (start in seq(1, nrow(data) - n_test)) {
  
  # Define a single rolling train and test dataset for all horizons
  train_data <- data[start:(start + n_train - 1), ]
  test_data <- data[(start + n_train):(start + n_train + n_test - 1), ]
  
  # Logistic regression models for h1, h3, and h6
  logit_h1 <- glm(market_state ~ lag1_tms + lag1_ret + lag1_infl + lag1_lty, data = train_data, family = binomial(link = "logit"))
  logit_h3 <- glm(market_state ~ lag3_tms + lag3_ret + lag3_infl + lag3_lty, data = train_data, family = binomial(link = "logit"))
  logit_h6 <- glm(market_state ~ lag6_tms + lag6_ret + lag6_infl + lag6_lty, data = train_data, family = binomial(link = "logit"))
  
  # Make predictions for each horizon using the standardized test data
  pred_h1 <- predict(logit_h1, newdata = test_data, type = "response")
  pred_h3 <- predict(logit_h3, newdata = test_data, type = "response")
  pred_h6 <- predict(logit_h6, newdata = test_data, type = "response")
  
  # Store predictions only for the final loop
  if (start == (nrow(data) - n_test)) {
    for (i in seq_len(nrow(test_data))) {
      final_predicted_probs_logit_h1[[i]] <- data.frame(date = test_data$date[i], prob = pred_h1[i])
      final_predicted_probs_logit_h3[[i]] <- data.frame(date = test_data$date[i], prob = pred_h3[i])
      final_predicted_probs_logit_h6[[i]] <- data.frame(date = test_data$date[i], prob = pred_h6[i])
    }
  }
}

# Combine lists into data frames for each horizon
predicted_probs_logit_h1_df <- do.call(rbind, final_predicted_probs_logit_h1)
predicted_probs_logit_h3_df <- do.call(rbind, final_predicted_probs_logit_h3)
predicted_probs_logit_h6_df <- do.call(rbind, final_predicted_probs_logit_h6)

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

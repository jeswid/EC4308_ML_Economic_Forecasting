# Set seed for reproducibility
set.seed(42)

library(dplyr)

# Load dataset
data <- readRDS("data/final_cleaned_data_with_bull_bear.RDS")
data$date <- as.Date(data$DATE)

# Sort the data by date
data <- data[order(data$date), ]

# Remove initial rows and last 6 rows for each horizon dataset
data <- data[18:(nrow(data) - 6), ]  # Standardize train set with ML model train-test split (cut first 17 rows)

# Define the test set size and calculate training set size based on available data
n_test <- 150
n_train <- nrow(data) - n_test

# Initialize empty data frames to store final predictions for all test rows
predicted_probs_logit_h1_df <- data.frame()
predicted_probs_logit_h3_df <- data.frame()
predicted_probs_logit_h6_df <- data.frame()

# Rolling Window Loop
for (start in seq(1, nrow(data) - n_train - n_test + 1)) {
  
  # Define a single rolling train and test dataset for all horizons
  train_data <- data[start:(start + n_train - 1), ]
  
  # Loop through each test row (150 rows in total)
  for (test_idx in 1:n_test) {
    test_data <- data[start + n_train + test_idx - 1, , drop = FALSE]  # Single row for each test_idx
    
    # Logistic regression models for h1, h3, and h6 with convergence check
    logit_h1 <- tryCatch({
      glm(market_state ~ lag1_tms + lag1_ret + lag1_infl + lag1_lty, data = train_data, family = binomial(link = "logit"))
    }, error = function(e) NA)
    
    logit_h3 <- tryCatch({
      glm(market_state ~ lag3_tms + lag3_ret + lag3_infl + lag3_lty, data = train_data, family = binomial(link = "logit"))
    }, error = function(e) NA)
    
    logit_h6 <- tryCatch({
      glm(market_state ~ lag6_tms + lag6_ret + lag6_infl + lag6_lty, data = train_data, family = binomial(link = "logit"))
    }, error = function(e) NA)
    
    # Make predictions for each horizon if models are valid GLM objects
    pred_h1 <- if (inherits(logit_h1, "glm")) predict(logit_h1, newdata = test_data, type = "response") else NA
    pred_h3 <- if (inherits(logit_h3, "glm")) predict(logit_h3, newdata = test_data, type = "response") else NA
    pred_h6 <- if (inherits(logit_h6, "glm")) predict(logit_h6, newdata = test_data, type = "response") else NA
    
    # Append the prediction for each test row into the respective data frames
    predicted_probs_logit_h1_df <- rbind(predicted_probs_logit_h1_df, data.frame(date = test_data$date, prob = pred_h1))
    predicted_probs_logit_h3_df <- rbind(predicted_probs_logit_h3_df, data.frame(date = test_data$date, prob = pred_h3))
    predicted_probs_logit_h6_df <- rbind(predicted_probs_logit_h6_df, data.frame(date = test_data$date, prob = pred_h6))
  }
}

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


# Final check to ensure predictions are correctly stored
cat("Final H1 Predictions:", nrow(predicted_probs_logit_h1_df), "\n")
cat("Final H3 Predictions:", nrow(predicted_probs_logit_h3_df), "\n")
cat("Final H6 Predictions:", nrow(predicted_probs_logit_h6_df), "\n")

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

# get date column
test_date = tail(data$DATE, 150)

pred_h1 = data.frame(date = test_date)
pred_h1$pred = NA
pred_h3 = data.frame(date = test_date)
pred_h3$pred = NA
pred_h6 = data.frame(date = test_date)
pred_h6$pred = NA

# Rolling window loop
for (start in seq(1, 150)) {
  # Define a single rolling train and test dataset for all horizons
  train_data <- data[start:(start + n_train - 1), ]
  test_data <- data[start + n_train, ] 
    
  logit_h1 <- glm(market_state ~ lag1_tms + lag1_ret + lag1_infl + lag1_lty, data = train_data, family = binomial(link = "logit"))
  logit_h3 <- glm(market_state ~ lag3_tms + lag3_ret + lag3_infl + lag3_lty, data = train_data, family = binomial(link = "logit"))
  logit_h6 <- glm(market_state ~ lag6_tms + lag6_ret + lag6_infl + lag6_lty, data = train_data, family = binomial(link = "logit"))
  
  pred_h1[start, 2] = predict(logit_h1, newdata = test_data, type = "response")
  pred_h3[start, 2] = predict(logit_h3, newdata = test_data, type = "response")
  pred_h6[start, 2] = predict(logit_h6, newdata = test_data, type = "response")
}

# Join predictions back to the original dataset if desired
data <- data %>%
  left_join(pred_h1, by = "date") %>%
  rename(predicted_prob_logit_h1 = pred) %>%
  left_join(pred_h3, by = "date") %>%
  rename(predicted_prob_logit_h3 = pred) %>%
  left_join(pred_h6, by = "date") %>%
  rename(predicted_prob_logit_h6 = pred)

# Save predicted values as RDS file
saveRDS(data, file = "data/logit_predictions.rds")
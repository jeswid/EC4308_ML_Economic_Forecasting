# remove all objects from memory
rm(list = ls())

# set seed for reproducibility
set.seed(42)

# load dataset
data <- readRDS("data/final_cleaned_data.RDS")

# ensure the DATE column is in date format
data$date <- as.Date(data$DATE$DATE)

# sort the data chronologically by date
data <- data[order(data$date), ]

# define the number of observations for test, validation, and the rest for training
n_test <- 150
n_validation <- 100
n_train <- nrow(data) - n_test - n_validation

# split the data in chronological order
train_data <- data[1:n_train, ]
validation_data <- data[(n_train + 1):(n_train + n_validation), ]
test_data <- data[(n_train + n_validation + 1):nrow(data), ]

# Optional: Extract dates from the test set if needed
test_data_date <- test_data %>%
  select(date)

test_df_date <- as.data.frame(test_data_date)

# Output the dimensions of each split for confirmation
cat("Train Data:", nrow(train_data), "\n")
cat("Validation Data:", nrow(validation_data), "\n")
cat("Test Data:", nrow(test_data), "\n")

## Benchmark Model: Logit
# logistic regression for h1
logit_h1 <- glm(market_state ~  tms + lag1_ret + infl + lty, data = train_data, family = binomial(link = "logit"))
# display summary of the model
summary(logit_h1)
# making predictions
predicted_prob_logit_h1 = predict(logit_h1, newdata = test_data, type = "response")
predicted_prob_logit_h1_df = as.data.frame(predicted_prob_logit_h1) %>%
  add_column(test_df_date) 

data = left_join(data,predicted_prob_logit_h1_df,by = "date")

# logistic regression for h3
logit_h3 <- glm(market_state ~  tms + lag4_ret + infl + lty, data = train_data, family = binomial(link = "logit"))
# display summary of the model
summary(logit_h3)
# making predictions
predicted_prob_logit_h3 = predict(logit_h3, newdata = test_data, type = "response")
predicted_prob_logit_h3_df = as.data.frame(predicted_prob_logit_h3) %>%
  add_column(test_df_date) 

data = left_join(data,predicted_prob_logit_h3_df,by = "date")

# logistic regression for h6
logit_h6 <- glm(market_state ~  tms + lag7_ret + infl + lty, data = train_data, family = binomial(link = "logit"))
# display summary of the model
summary(logit_h6)
# making predictions
predicted_prob_logit_h6 =predict(logit_h6, newdata = test_data, type = "response")

predicted_prob_logit_h6_df = as.data.frame(predicted_prob_logit_h6) %>%
  add_column(test_df_date) 

data = left_join(data,predicted_prob_logit_h6_df,by = "date")

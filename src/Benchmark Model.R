# Set seed for reproducibility
set.seed(42)

# Load the data
data <- readRDS("final_cleaned_data.RDS")

# Extract the DATE column and ensure it's in date format
data$date <- as.Date(data$DATE$DATE) 

# Sort the data chronologically by date
data <- data[order(data$date), ]

# Define split percentages
train_frac <- 0.8  # 80% for training
valid_frac <- 0.1  # 10% for validation
test_frac <- 0.1   # 10% for testing

# Calculate row indices for splitting
n <- nrow(data)
train_index <- floor(train_frac * n)
valid_index <- floor((train_frac + valid_frac) * n)

# Split the data in chronological order
train_data <- data[1:train_index, ]
validation_data <- data[(train_index + 1):valid_index, ]
test_data <- data[(valid_index + 1):n, ]

# Optional: Extract dates from the test set if needed
test_data_date <- test_data %>%
  select(date)

test_df_date <- as.data.frame(test_data_date)

# Output the dimensions of each split for confirmation
cat("Train Data:", nrow(train_data), "\n")
cat("Validation Data:", nrow(validation_data), "\n")
cat("Test Data:", nrow(test_data), "\n")


###ML Models

##Benchmark Model: Logit
# logistic regression for h1
logit_h1 <- glm(market_state ~  tms + lag1_ret + infl + lty, data = train_data, family = binomial(link = "logit"))
# display summary of the model
summary(logit_h1)
# making predictions
predicted_prob_logit_h1 = predict(logit_h1, newdata = test_data, type = "response")
predicted_prob_logit_h1_df = as.data.frame(predicted_prob_logit_h1) %>%
  add_column(test_df_date) 

data = left_join(data,predicted_prob_logit_h1_df,by = "date")

#logistic regression for h3
logit_h3 <- glm(market_state ~  tms + lag3_ret + infl + lty, data = train_data, family = binomial(link = "logit"))
# display summary of the model
summary(logit_h3)
# making predictions
predicted_prob_logit_h3 = predict(logit_h3, newdata = test_data, type = "response")
predicted_prob_logit_h3_df = as.data.frame(predicted_prob_logit_h3) %>%
  add_column(test_df_date) 

data = left_join(data,predicted_prob_logit_h3_df,by = "date")

#logistic regression for h6
logit_h6 <- glm(market_state ~  tms + lag6_ret + infl + lty, data = train_data, family = binomial(link = "logit"))
# display summary of the model
summary(logit_h6)
# making predictions
predicted_prob_logit_h6 =predict(logit_h6, newdata = test_data, type = "response")

predicted_prob_logit_h6_df = as.data.frame(predicted_prob_logit_h6) %>%
  add_column(test_df_date) 

data = left_join(data,predicted_prob_logit_h6_df,by = "date")

#logistic regression for h12
logit_h12 <- glm(market_state ~  tms + lag12_ret + infl + lty, data = train_data, family = binomial(link = "logit"))
# display summary of the model
summary(logit_h12)
# making predictions
predicted_prob_logit_h12 = predict(logit_h12, newdata = test_data, type = "response")

predicted_prob_logit_h12_df = as.data.frame(predicted_prob_logit_h12) %>%
  add_column(test_df_date) 

data = left_join(data,predicted_prob_logit_h12_df,by = "date")

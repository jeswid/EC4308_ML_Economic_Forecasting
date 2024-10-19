# Set seed for reproducibility
set.seed(42)

data = readRDS("final_cleaned_data.RDS") %>%
  mutate(DATE = as.Date(DATE))

# Stratified Sampling by 'market_state'
strata_cols <- c("market_state")

# Perform the stratified sampling for training set (80%)
train_data <- data %>%
  group_by(across(all_of(strata_cols))) %>%
  sample_frac(0.8) %>% # Adjust the fraction as needed for training set
  ungroup()

# Remaining data after training set
remaining_data <- anti_join(data, train_data)

# Validation set (10%)
validation_data <- remaining_data %>%
  group_by(across(all_of(strata_cols))) %>%
  sample_frac(0.5) %>% # This will give you 10% of the original data (half of the remaining 20%)
  ungroup()

# Test set (10%)
test_data <- anti_join(remaining_data, validation_data)
test_data_date = test_data %>%
  select(DATE)
test_df = as.data.frame(test_data_date)

# Check the sizes of the datasets
cat("Training set size:", nrow(train_data), "\n")
cat("Validation set size:", nrow(validation_data), "\n")
cat("Test set size:", nrow(test_data), "\n")


###ML Models

##Benchmark Model: Logit
# logistic regression for h1
logit_h1 <- glm(market_state ~  tms + lag1_ret + infl + lty, data = train_data, family = binomial(link = "logit"))
# display summary of the model
summary(logit_h1)
# making predictions
predicted_prob_logit_h1 = predict(logit_h1, newdata = test_data, type = "response")
predicted_prob_logit_h1_df = as.data.frame(predicted_prob_logit_h1) %>%
  add_column(test_data_date$DATE) %>%
  mutate(date = "DATE")

data = left_join(data,predicted_prob_logit_h1_df,by = "DATE")

#logistic regression for h3
logit_h3 <- glm(market_state ~  tms + lag3_ret + infl + lty, data = train_data, family = binomial(link = "logit"))
# display summary of the model
summary(logit_h3)
# making predictions
predicted_prob_logit_h3 = predict(logit_h3, newdata = test_data, type = "response")

#logistic regression for h6
logit_h6 <- glm(market_state ~  tms + lag6_ret + infl + lty, data = train_data, family = binomial(link = "logit"))
# display summary of the model
summary(logit_h6)
# making predictions
predicted_prob_logit_h6 =predict(logit_h6, newdata = test_data, type = "response")

#logistic regression for h12
logit_h12 <- glm(market_state ~  tms + lag12_ret + infl + lty, data = train_data, family = binomial(link = "logit"))
# display summary of the model
summary(logit_h12)
# making predictions
predicted_prob_logit_h12 = predict(logit_h12, newdata = test_data, type = "response")

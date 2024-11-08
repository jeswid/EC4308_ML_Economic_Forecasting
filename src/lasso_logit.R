library(hdm)
library(dplyr)
library(tibble)
library(ggplot2)
library(purrr)

rm(list = ls())

data <- readRDS("data/final_cleaned_data_with_bull_bear.RDS")

# Sort data according to time
data <- data[order(data$DATE), ]

# Remove the first 12 rows due to NA
data <- data[-(1:17), ]

# Remove last 6 due to how two-sided moving average
data <- data %>% filter(row_number() <= n()-6)

test_size <- 150

# Initialise dataframe to store predicted probabilities
predicted <- data.frame(
  h1_forecast = rep(0, 150),
  h3_forecast = rep(0, 150),
  h6_forecast = rep(0, 150)
)

# Generate dates for the predicted dataframe
date <- data.frame(date = data$DATE)
date <- date[(nrow(data) - test_size + 1):nrow(data),]

# data for the different h-step ahead forecast

X_h1 = data %>% 
  select(starts_with("lag"), "market_state", "DATE") %>% # remove present values 
  select(-starts_with("lag7_"), -starts_with("lag8_"), -starts_with("lag9_"), -starts_with("lag10_"), -starts_with("lag11_"), -ends_with("CAPE"), ends_with("market_state")) %>% # keep lag 1-6 for X variables, but keep all lags of Y first
  select(-c(lag1_market_state, lag2_market_state, lag3_market_state, lag4_market_state, lag5_market_state, lag6_market_state, lag13_market_state, lag14_market_state, lag15_market_state, lag16_market_state, lag17_market_state, # remove lags of Y
            ends_with("excess_CAPE_yield")))

X_h3 = data %>% 
  select(starts_with("lag"), "market_state","DATE") %>% # remove present values 
  select(-starts_with("lag1_")) %>% select(-starts_with("lag2_")) %>% 
  select(-starts_with("lag9_"), -starts_with("lag10_"), -starts_with("lag11_"), -ends_with("CAPE"), ends_with("market_state")) %>% # keep lag 3-8 for X variables, but keep all lags of Y first
  select(-c(lag3_market_state, lag4_market_state, lag5_market_state, lag6_market_state, lag7_market_state, lag8_market_state, lag15_market_state, lag16_market_state, lag17_market_state, # remove lags of Y
            ends_with("excess_CAPE_yield")))

X_h6 = data %>% 
  select(starts_with("lag"), "market_state","DATE") %>% # remove present values 
  select(-starts_with("lag1_"), -starts_with("lag2_"), -starts_with("lag3_"), -starts_with("lag4_"), -starts_with("lag5_"), -ends_with("CAPE"), ends_with("market_state")) %>% # keep lag 6-11 for X variables, but keep all lags of Y first
  select(-c(lag1_market_state, lag2_market_state, lag3_market_state, lag4_market_state, lag5_market_state, lag6_market_state, lag7_market_state, lag8_market_state, lag9_market_state, lag10_market_state, lag11_market_state, # remove lags of Y
            ends_with("excess_CAPE_yield")))

# function to run lasso logit and forecast
forecast_h_step <- function(i, data, testsize, hstep){
  #Forecast using rolling window
  #Split data according to window
  train_set <- data[i:(nrow(data) - test_size + (i-1)), ]
  test_set <- data[(nrow(data) - test_size + 1 + (i-1)):nrow(data), ]
  
  #Train model on window and predict using optimal lambda
  model <- rlassologit(market_state ~ . - DATE, data = train_set)
  optimal_lambda <- model$lambda
  predictions <- predict(model, newdata = test_set, type = "response", lambda = optimal_lambda)
  
  return(predictions[hstep])
}

predicted$h1_forecast <- map_dbl(1:150, forecast_h_step, data = X_h1, testsize = test_size, hstep = 1)
predicted$h3_forecast <- map_dbl(1:150, forecast_h_step, data = X_h3, testsize = test_size, hstep = 3)
predicted$h6_forecast <- map_dbl(1:150, forecast_h_step, data = X_h6, testsize = test_size, hstep = 6)

# Append date to predicted dataframe
predicted$Date <- date

# Shift the h-step ahead forecast to match the dates
predicted <- predicted %>%
  mutate(h3_forecast = lag(h3_forecast, n = 2))

predicted <- predicted %>%
  mutate(h6_forecast = lag(h6_forecast, n = 5))

# Save predicted values as RDS file
saveRDS(predicted, file = "lasso_logit_predictions.rds")

# Function generates a dataframe of predictors (and a boolean) that are selected in each iteration
get_selected_features <- function(i, data, testsize){
  
  train_set <- data[i:(nrow(data) - test_size + (i-1)), ]
  
  model <- rlassologit(market_state ~ . - DATE, data = train_set)
  
  # Get the boolean matrix for features. TRUE = selected, FALSE = not selected
  temp_store_feature <- data.frame(model[4])
  
  # Get only the selected features
  temp_store_feature <- temp_store_feature %>%
    filter(index == TRUE)
  
  # Process data
  temp_store_feature <- rownames_to_column(temp_store_feature, var = "predictor")
  
  return(temp_store_feature)
}

# Combine all selected features in each iteration and store it
feat_df_h1 <- bind_rows(map(1:150, get_selected_features, data = X_h1, testsize = test_size))
feat_df_h3 <- bind_rows(map(1:148, get_selected_features, data = X_h3, testsize = test_size))
feat_df_h6 <- bind_rows(map(1:145, get_selected_features, data = X_h6, testsize = test_size))

# Count TRUE, group by predictors
feat_df_h1 <- feat_df_h1 %>%
  group_by(predictor) %>%
  summarise(true_count = sum(index == TRUE))

feat_df_h3 <- feat_df_h3 %>%
  group_by(predictor) %>%
  summarise(true_count = sum(index == TRUE))

feat_df_h6 <- feat_df_h6 %>%
  group_by(predictor) %>%
  summarise(true_count = sum(index == TRUE))

#Filter for features greater than mean
feat_df_h1 <- feat_df_h1[feat_df_h1$true_count > mean(feat_df_h1$true_count),]
feat_df_h3 <- feat_df_h3[feat_df_h3$true_count > mean(feat_df_h3$true_count),]
feat_df_h6 <- feat_df_h6[feat_df_h6$true_count > mean(feat_df_h6$true_count),]

# Plot feature importance
h1plot <- feat_df_h1 %>%
  arrange(true_count) %>%    
  mutate(predictor=factor(predictor, levels=predictor)) %>%  
  ggplot( aes(x=predictor, y=true_count)) +
  geom_segment( aes(xend=predictor, yend=0)) +
  geom_point( size=4, color="orange") +
  coord_flip() +
  theme_bw() +
  xlab("")

h3plot <- feat_df_h3 %>%
  arrange(true_count) %>%    
  mutate(predictor=factor(predictor, levels=predictor)) %>%  
  ggplot( aes(x=predictor, y=true_count)) +
  geom_segment( aes(xend=predictor, yend=0)) +
  geom_point( size=4, color="orange") +
  coord_flip() +
  theme_bw() +
  xlab("")

h6plot <- feat_df_h6 %>%
  arrange(true_count) %>%    
  mutate(predictor=factor(predictor, levels=predictor)) %>%  
  ggplot( aes(x=predictor, y=true_count)) +
  geom_segment( aes(xend=predictor, yend=0)) +
  geom_point( size=4, color="orange") +
  coord_flip() +
  theme_bw() +
  xlab("")

# Show plot
print(h1plot)
print(h3plot)
print(h6plot)

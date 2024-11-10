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
data <- data[1:(nrow(data)-6),]

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
  select(-starts_with("lag7_"), -starts_with("lag8_"), -starts_with("lag9_"), -starts_with("lag10_"), -starts_with("lag11_"), ends_with("market_state")) %>% # keep lag 1-6 for X variables, but keep all lags of Y first
  select(-c(lag1_market_state, lag2_market_state, lag3_market_state, lag4_market_state, lag5_market_state, lag6_market_state, lag13_market_state, lag14_market_state, lag15_market_state, lag16_market_state, lag17_market_state, # remove lags of Y
            ))

X_h3 = data %>% 
  select(starts_with("lag"), "market_state","DATE") %>% # remove present values 
  select(-starts_with("lag1_")) %>% select(-starts_with("lag2_")) %>% 
  select(-starts_with("lag9_"), -starts_with("lag10_"), -starts_with("lag11_"), ends_with("market_state")) %>% # keep lag 3-8 for X variables, but keep all lags of Y first
  select(-c(lag3_market_state, lag4_market_state, lag5_market_state, lag6_market_state, lag7_market_state, lag8_market_state, lag15_market_state, lag16_market_state, lag17_market_state, # remove lags of Y
            ))

X_h6 = data %>% 
  select(starts_with("lag"), "market_state","DATE") %>% # remove present values 
  select(-starts_with("lag1_"), -starts_with("lag2_"), -starts_with("lag3_"), -starts_with("lag4_"), -starts_with("lag5_"), ends_with("market_state")) %>% # keep lag 6-11 for X variables, but keep all lags of Y first
  select(-c(lag1_market_state, lag2_market_state, lag3_market_state, lag4_market_state, lag5_market_state, lag6_market_state, lag7_market_state, lag8_market_state, lag9_market_state, lag10_market_state, lag11_market_state, # remove lags of Y
            ))

# function to run lasso logit and forecast
forecast_h_step <- function(i, data, testsize){
  #Forecast using rolling window
  #Split data according to window
  train_set <- data[i:(nrow(data) - test_size + (i-1)), ]
  test_set <- data[(nrow(data) - test_size + 1 + (i-1)):nrow(data), ]
  
  #Train model on window and predict using optimal lambda
  model <- rlassologit(market_state ~ . - DATE, data = train_set, intercept = TRUE)
  optimal_lambda <- model$lambda
  predictions <- predict(model, newdata = test_set, type = "response", lambda = optimal_lambda)
  
  return(predictions[1])
}

predicted$h1_forecast <- map_dbl(1:150, forecast_h_step, data = X_h1, testsize = test_size)
predicted$h3_forecast <- map_dbl(1:150, forecast_h_step, data = X_h3, testsize = test_size)
predicted$h6_forecast <- map_dbl(1:150, forecast_h_step, data = X_h6, testsize = test_size)

# Append date to predicted dataframe
predicted$Date <- date

# Save predicted values as RDS file
saveRDS(predicted, file = "lasso_logit_predictions.rds")

#Function generates a dataframe of predictors that are selected in each iteration
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
feat_df_h3 <- bind_rows(map(1:150, get_selected_features, data = X_h3, testsize = test_size))
feat_df_h6 <- bind_rows(map(1:150, get_selected_features, data = X_h6, testsize = test_size))

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

feat_df_h1 <- feat_df_h1 %>% mutate(horizon = "h = 1")
feat_df_h3 <- feat_df_h3 %>% mutate(horizon = "h = 3")
feat_df_h6 <- feat_df_h6 %>% mutate(horizon = "h = 6")

feature_importance <- bind_rows(feat_df_h1, feat_df_h3, feat_df_h6)

# groups for feature importance plot 
price_div_earn = grep("(ret(x)?|ratio|yield|payout|book_market|fbm|price|dividend|earnings|rate_gs10|TR_CAPE)$", colnames(data), value = TRUE) # ret, retx, dividend_price_ratio, dividend_yield, earnings_price_ratio, dividend_payout, book_market, fbm, price, dividend, earnings, rate_gs10, TR_CAPE 
return_yield = grep("(AA|lty|ltr|corpr|tbl|Rfree|tms|dfy|dfr|returns)$", colnames(data), value = TRUE) # AAA, BAA, lty, ltr, corpr, Rfree, term_spread, dfy, dfr, monthly_total_bond_returns 
econ_indicator = grep("(infl|ntis|ogap|wtexas|CPI|UNRATE|DFF|INDPRO|ndrbl)$", colnames(data), value = TRUE) # infl, ntis, ogap, wtexas, CPI, UNRATE, DFF, INDPRO, ndrbl 
risk_measure = grep("(svar|skvw|tail|lzrt|rdsp)$", colnames(data), value = TRUE) # svar, skvw, tail, lzrt, rdsp 
technical_indicator = grep("(dtoy|dtoat|tchi|avgcor|sntm|ygap|shtint)$", colnames(data), value = TRUE) # dtoy, dtoat, tchi, avgcor, sntm, ygap, shtint 
lag_market_state = grep("market_state$", colnames(data), value = TRUE)  

# Define the variable groups as a named list 
variable_groups <- list(
  "Price, Dividends, and Earnings" = price_div_earn,   
  "Returns and Yields" = return_yield,   
  "Economic Indicators" = econ_indicator,   
  "Risk Measures" = risk_measure,   
  "Market Sentiment and Technical Indicators" = technical_indicator,   
  "Lag Market State" = lag_market_state )

# Convert the list into a lookup table to match variables with their group
variable_lookup <- stack(variable_groups) %>%
  rename(var = values, group = ind)

feature_importance_grouped <- feature_importance %>%
  left_join(variable_lookup, by = c("predictor" = "var"))

# Calculate the relative importance in each group and get the distinct values
feature_importance_summary <- feature_importance_grouped %>%
  group_by(horizon,group) %>%
  mutate(tcount = sum(true_count)) %>%
  ungroup() %>%
  
  group_by(horizon) %>%
  mutate(norm_count = 100 * tcount/sum(true_count)) %>%
  ungroup() %>%
  
  distinct(horizon, group, norm_count)

p<-ggplot(feature_importance_summary, aes(x = factor(horizon), y = norm_count, fill = group)) +
  geom_bar(stat = "identity", position = "stack") +  # Stack bars by horizon
  geom_text(aes(label = round(norm_count, 2)),
            position = position_stack(vjust = 0.5), size = 3.5) +  # Position text labels at center of each stack
  labs(title = "LASSO Logit Feature Importance By Type", x = "Forecast Horizon", y = "Relative Importance") +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for horizons
  theme_minimal() +
  theme(legend.position = "bottom",  # Move legend below plot
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5, face = "bold"))  # Center and bold title

print(p)

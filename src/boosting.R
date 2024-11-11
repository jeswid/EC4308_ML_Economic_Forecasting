rm(list=ls())

library(randomForest)
library(gbm)
library(dplyr)
library(xgboost)

set.seed(123)
data = readRDS("data/final_cleaned_data_with_bull_bear.RDS")

data$DATE <- as.Date(data$DATE)
# Sort the data by date
data <- data[order(data$DATE), ]

data <- data[-(1:17), ]  # align for missing data due to 17 lags of Y
data = head(data, -6) # remove last 6 rows due to inaccurate market state

#Remove original variables (not differenced)
data <- data[, !grepl("original", names(data))]

Y = data$market_state

# get date column for test set
test_date = tail(data$DATE, 150)

# remove DATE column
data = data %>% select(-DATE) %>%
  select(-contains("original"))

ntest = 150
ncrossv = 100 
ntrain = nrow(data) - ncrossv - ntest

# Prepare data for different forecast horizons
# h=1, X from lag 1-6, Y from lag 7-12
X_h1 = data %>% 
  select(starts_with("lag")) %>% # remove present values 
  select(-starts_with("lag7_"), -starts_with("lag8_"), -starts_with("lag9_"), -starts_with("lag10_"), -starts_with("lag11_"), ends_with("market_state")) %>% # keep lag 1-6 for X variables, but keep all lags of Y first
  select(-c(lag1_market_state, lag2_market_state, lag3_market_state, lag4_market_state, lag5_market_state, lag6_market_state, 
            lag13_market_state, lag14_market_state, lag15_market_state, lag16_market_state, lag17_market_state)) %>% # keep lag 7-12 for Y
  as.matrix()

# h=3, X from lag 3-8, Y from lag 9-14
X_h3 = data %>% 
  select(starts_with("lag")) %>% # remove present values 
  select(-starts_with("lag1_")) %>% select(-starts_with("lag2_")) %>% 
  select(-starts_with("lag9_"), -starts_with("lag10_"), -starts_with("lag11_"), ends_with("market_state")) %>% # keep lag 3-8 for X variables, but keep all lags of Y first
  select(-c(lag3_market_state, lag4_market_state, lag5_market_state, lag6_market_state, lag7_market_state, lag8_market_state,
            lag15_market_state, lag16_market_state, lag17_market_state)) %>% # keep lag 9-14 for Y
  as.matrix()

# h=6, X from lag 6-11, Y from lag 12-17
X_h6 = data %>% 
  select(starts_with("lag")) %>% # remove present values 
  select(-starts_with("lag1_"), -starts_with("lag2_"), -starts_with("lag3_"), -starts_with("lag4_"), -starts_with("lag5_")) %>% # keep lag 6-11 for X variables
  select(-c(lag6_market_state, lag7_market_state, lag8_market_state, lag9_market_state, lag10_market_state, lag11_market_state)) %>% # keep lag 12-17 for Y
  as.matrix()


######################################################################################################################################

# using gbm package
runboost = function(X, y) {
  X_temp = as.data.frame(X)
  X.out=tail(X_temp,1)
  data = cbind(as.data.frame(y), as.data.frame(X_temp))
  temp.boost = gbm(formula = data$y ~ ., data = data, distribution="bernoulli",
                   interaction.depth = 5,n.trees = M,shrinkage = .01) #fit model for d=5
  temp.fit = predict(temp.boost, as.data.frame(X.out), n.trees = seq(1, M, 1), type = "response") #prediction for d=5
  return(list("model"=temp.boost,"pred"=temp.fit))
}

runboost2 = function(X, y) {
  X_temp = as.data.frame(X)
  X.out=tail(X_temp,1)
  data = cbind(as.data.frame(y), as.data.frame(X_temp))
  temp.boost = gbm(formula = data$y ~ ., data = data, distribution="bernoulli",
                   interaction.depth = 2,n.trees = M,shrinkage = .01) #fit model for d=2
  temp.fit = predict(temp.boost, as.data.frame(X.out), n.trees = seq(1, M, 1), type = "response") #prediction for d=2
  return(list("model"=temp.boost,"pred"=temp.fit))
}

# using xgb package
runboost_xgb = function(X, y) {
  X_temp = as.matrix(X)
  X.out=as.matrix(tail(X_temp,1))
  dtrain <- xgb.DMatrix(data = X_temp, label = y) # convert data to DMatrix format
  temp.boost = xgb.train(data = dtrain, objective = "binary:logistic",
                         max.depth = 5,nround = M,eta = .01, verbosity = 0) #fit model for d=5
  pred_matrix <- matrix(0, nrow = 1, ncol = M) # initialize a matrix to store predictions across all trees
  for (tree in 1:M) { # loop over each tree and generate prediction
    pred_matrix[1, tree] <- predict(temp.boost, X.out, ntreelimit = tree)
  }
  return(list("model"=temp.boost,"pred"=pred_matrix))
}

runboost2_xgb = function(X, y) {
  X_temp = as.matrix(X)
  X.out=as.matrix(tail(X_temp,1))
  dtrain <- xgb.DMatrix(data = X_temp, label = y) # convert data to DMatrix format
  temp.boost = xgb.train(data = dtrain, objective = "binary:logistic",
                         max.depth = 2,nround = M,eta = .01, verbosity = 0) #fit model for d=2
  pred_matrix <- matrix(0, nrow = 1, ncol = M) # initialize a matrix to store predictions across all trees
  for (tree in 1:M) { # loop over each tree and generate prediction
    pred_matrix[1, tree] <- predict(temp.boost, X.out, ntreelimit = tree)
  }
  return(list("model"=temp.boost,"pred"=pred_matrix))
}

######################################################################################################################################

# Cross Validation
M = 3000 # Max number of trees considered
boost.rolling.window=function(X,y,d,threshold) {
  X_CV = head(X, ntrain+ncrossv)   # remove test set
  y_cv = head(Y, ntrain+ncrossv)
  real = tail(y_cv, ncrossv)
  cv_boost = matrix(0,ncrossv,M) #blank for CV criteria
  for(i in ncrossv:1){#NB: backwards FOR loop: going from 100 down to 1
    X.window = X_CV[(1+ncrossv-i):(nrow(X_CV)-i),] #define the estimation window (first one: 1 to 321, then 2 to 322 etc, till 100 to 420)
    y.window = y_cv[(1+ncrossv-i):(length(y_cv)-i)]
    if (d==5) {
      boost = runboost(X.window, y.window)
    }
    else if (d==2) {
      boost = runboost2(X.window, y.window)
    }
    cv_boost[(1+ncrossv-i), ] = matrix(boost$pred > threshold) # save the forecast
    cat("iteration", (1+ncrossv-i), "\n") # display iteration number
  }
  missclass = colSums(abs(real-cv_boost))
  bestM = which.min(missclass) 
  cv_min = min(missclass/ncrossv) 
  return(list("bestM" = bestM, "cv_min" = cv_min))
}

# GBM with threshold of 0.5
h1_cv_gbm = boost.rolling.window(X_h1, Y, 5, 0.5)  # bestM = 510, cv_min = 0.07 
h1_cv_gbm2 = boost.rolling.window(X_h1, Y, 2, 0.5)  # bestM = 1163, cv_min = 0.07
h3_cv_gbm = boost.rolling.window(X_h3, Y, 5, 0.5)  # bestM = 496, cv_min = 0.07
h3_cv_gbm2 = boost.rolling.window(X_h3, Y, 2, 0.5)  # bestM = 1231, cv_min = 0.07
h6_cv_gbm = boost.rolling.window(X_h6, Y, 5, 0.5)  # bestM = 488, cv_min = 0.07
h6_cv_gbm2 = boost.rolling.window(X_h6, Y, 2, 0.5)  # bestM = 944, cv_min = 0.07

# GBM using sample mean as threshold
y_train = head(Y, ntrain)
y_sample_mean = mean(y_train)
threshold = 1-y_sample_mean

h1_cv_gbm_mean = boost.rolling.window(X_h1, Y, 5, threshold) # bestM = 952, cv_min = 0.07
h3_cv_gbm_mean = boost.rolling.window(X_h3, Y, 5, threshold) # bestM = 1058, cv_min = 0.07
h6_cv_gbm_mean = boost.rolling.window(X_h6, Y, 5, threshold) # bestM = 1090, cv_min = 0.07


boostxgb.rolling.window=function(X,y,d,threshold) {
  X_CV = head(X, ntrain+ncrossv)   # remove test set
  y_cv = head(Y, ntrain+ncrossv)
  real = tail(y_cv, ncrossv)
  cv_boost_xgb = matrix(0,ncrossv,M) #blank for CV criteria
  for(i in ncrossv:1){#NB: backwards FOR loop: going from 100 down to 1
    X.window = X_CV[(1+ncrossv-i):(nrow(X_CV)-i),] #define the estimation window (first one: 1 to 321, then 2 to 322 etc, till 100 to 420)
    y.window = y_cv[(1+ncrossv-i):(length(y_cv)-i)]
    if (d==5) {
      boost = runboost_xgb(X.window, y.window)
    }
    else if (d==2) {
      boost = runboost2_xgb(X.window, y.window)
    }
    cv_boost_xgb[(1+ncrossv-i), ] = matrix(boost$pred > threshold) # save the forecast
    cat("iteration", (1+ncrossv-i), "\n") # display iteration number
  }
  missclass_xgb = colSums(abs(real-cv_boost_xgb)) # compute misclassification rate
  bestM_xgb = which.min(missclass_xgb) 
  cv_min_xgb = min(missclass_xgb/ncrossv) 
  return(list("bestM" = bestM_xgb, "cv_min" = cv_min_xgb))
}

# xgb using threshold of 0.5
h1_cv_xgb = boostxgb.rolling.window(X_h1, Y, 5, 0.5) # bestM = 174, cv_min = 0.07
h1_cv_xgb2 = boostxgb.rolling.window(X_h1, Y, 2, 0.5) # bestM = 725, cv_min = 0.07
h3_cv_xgb= boostxgb.rolling.window(X_h3, Y, 5, 0.5) # bestM = 179, cv_min = 0.07
h3_cv_xgb2 = boostxgb.rolling.window(X_h3, Y, 2, 0.5) # bestM = 1003, cv_min = 0.07
h6_cv_xgb = boostxgb.rolling.window(X_h6, Y, 5, 0.5) # bestM = 145, cv_min = 0.07
h6_cv_xgb2 = boostxgb.rolling.window(X_h6, Y, 2, 0.5) # bestM = 980, cv_min = 0.07


######################################################################################################################################

# Test
set.seed(123)
runboost_setn = function(X, y, n) { # n is the tree size
  X_temp = as.data.frame(X)
  X.out=tail(X_temp,1)
  data = cbind(as.data.frame(y), as.data.frame(X_temp))
  temp.boost = gbm(formula = data$y ~ ., data = data, distribution="bernoulli",
                   interaction.depth = 5,n.trees = n,shrinkage = .01) #fit model for d=5
  temp.fit = predict(temp.boost, as.data.frame(X.out), n.trees = n, type = "response") #prediction for d=5
  temp.importance = summary(temp.boost, plotit = FALSE)
  return(list("model"=temp.boost,"pred"=temp.fit, "importance"=temp.importance))
}

runboost_setn_xgb = function(X, y, n) { # n is the tree size
  X_temp = as.matrix(X)
  X.out=as.matrix(tail(X_temp,1))
  dtrain <- xgb.DMatrix(data = X_temp, label = y) # convert data to DMatrix format
  temp.boost = xgb.train(data = dtrain, objective = "binary:logistic",
                         max.depth = 5,nround = n,eta = .01, verbosity = 0) #fit model for d=5
  temp.fit = predict(temp.boost, X.out, ntreelimit = n)  #prediction for d=5
  temp.importance = xgb.importance(feature_names = colnames(X_temp), model = temp.boost)
  return(list("model"=temp.boost,"pred"=temp.fit, "importance"=temp.importance))
}


boost_test.rolling.window = function(X, y, n, package) {
  test_boost = matrix(0,ntest,1) #blank for CV criteria,
  save.importance = list()
  for(i in ntest:1){#NB: backwards FOR loop: going from 150 down to 1
    X.window = X[(1+ntest-i):(nrow(X)-i),] #define the estimation window (first one: 1 to 421, then 2 to 422 etc, till 150 to 570)
    y.window = y[(1+ntest-i):(length(y)-i)]
    if (package=="gbm") {
      boost = runboost_setn(X.window, y.window, n)
    }
    else if (package=="xgb") {
      boost = runboost_setn_xgb(X.window, y.window, n)
    }
    test_boost[(1+ntest-i), 1] = boost$pred # save the forecast
    save.importance[[i]]=boost$importance #save variable importance
    cat("iteration", (1+ntest-i), "\n") # display iteration number
  }
  return(list("pred"=test_boost, "save.importance"=save.importance))
}

# Test -- GBM with threshold of 0.5
test_result <- data.frame(matrix(NA, nrow = 150, ncol = 3))
colnames(test_result) <- c("1-step ahead forecast", "3-step ahead forecast", "6-step ahead forecast")

h1_test_gbm = boost_test.rolling.window(X_h1, Y, h1_cv_gbm$bestM, "gbm") #bestM=510
test_result$`1-step ahead forecast` = h1_test_gbm$pred
h3_test_gbm = boost_test.rolling.window(X_h3, Y, h3_cv_gbm$bestM, "gbm") #bestM=496
test_result$`3-step ahead forecast` = h3_test_gbm$pred
h6_test_gbm = boost_test.rolling.window(X_h6, Y, h6_cv_gbm$bestM, "gbm") #bestM=488
test_result$`6-step ahead forecast` = h6_test_gbm$pred

# save result
test_result = cbind(test_date, test_result)
saveRDS(test_result, file = "data/boosting_gbm_prediction.RDS")


# Test -- GBM with sample mean as threshold
test_result_sample_mean <- data.frame(matrix(NA, nrow = 150, ncol = 3))
colnames(test_result_sample_mean) <- c("1-step ahead forecast", "3-step ahead forecast", "6-step ahead forecast")

h1_test_gbm_mean = boost_test.rolling.window(X_h1, Y, h1_cv_gbm_mean$bestM, "gbm") # bestM = 952
test_result_sample_mean$`1-step ahead forecast` = h1_test_gbm_mean$pred
h3_test_gbm_mean = boost_test.rolling.window(X_h3, Y, h3_cv_gbm_mean$bestM, "gbm") # bestM = 1058
test_result_sample_mean$`3-step ahead forecast` = h3_test_gbm_mean$pred
h6_test_gbm_mean = boost_test.rolling.window(X_h6, Y, h6_cv_gbm_mean$bestM, "gbm") # bestM = 1090
test_result_sample_mean$`6-step ahead forecast` = h6_test_gbm_mean$pred

# save result
test_result_sample_mean = cbind(test_date, test_result_sample_mean)
saveRDS(test_result_sample_mean, file = "data/boosting_gbm_sample_mean_prediction.RDS")


# Test -- xgb with threshold of 0.5 
test_result_xgb <- data.frame(matrix(NA, nrow = 150, ncol = 3))
colnames(test_result_xgb) <- c("1-step ahead forecast", "3-step ahead forecast", "6-step ahead forecast")


h1_test_xgb = boost_test.rolling.window(X_h1, Y, h1_cv_xgb$bestM, "xgb") # bestM = 174
test_result_xgb$`1-step ahead forecast` = h1_test_xgb$pred
h3_test_xgb = boost_test.rolling.window(X_h3, Y, h3_cv_xgb$bestM, "xgb") # bestM = 179
test_result_xgb$`3-step ahead forecast` = h3_test_xgb$pred
h6_test_xgb = boost_test.rolling.window(X_h6, Y, h6_cv_xgb$bestM, "xgb") # bestM = 145
test_result_xgb$`6-step ahead forecast` = h6_test_xgb$pred

# Save result
test_result_xgb = cbind(test_date, test_result_xgb)
saveRDS(test_result_xgb, file = "data/boosting_xgb_prediction.RDS")


# Prepare for feature importance plot
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
  "Lag Market State" = lag_market_state
)

# Convert the list into a lookup table to match variables with their group
variable_lookup <- stack(variable_groups) %>%
  rename(var = values, group = ind)


mean_importance = bind_rows(h1_test_gbm$save.importance) %>%
  group_by(var) %>%
  summarize(mean_importance = mean(`rel.inf`, na.rm = TRUE)) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(horizon = 1)

mean_importance_h3 = bind_rows(h3_test_gbm$save.importance) %>%
  group_by(var) %>%
  summarize(mean_importance = mean(`rel.inf`, na.rm = TRUE)) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(horizon = 3)

mean_importance_h6 = bind_rows(h6_test_gbm$save.importance) %>%
  group_by(var) %>%
  summarize(mean_importance = mean(`rel.inf`, na.rm = TRUE)) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(horizon = 6)


# bind importance from all 3 horizons for plotting
df_plot = rbind(mean_importance, mean_importance_h3, mean_importance_h6) %>%
  group_by(group, horizon)

# save result
saveRDS(df_plot, file = "data/boosting_gbm_importance.RDS")


# df_plot = readRDS("data/boosting_gbm_importance.RDS")
ggplot(df_plot, aes(x = factor(horizon), y = average_importance, fill = group)) + 
  geom_bar(stat = "identity", position = "stack") +  # Stack bars by horizon
  geom_text(aes(label = round(average_importance, 2)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Position text labels at center of each stack
  labs(title = "GBM Average Feature Importance By Type", x = "Forecast Horizon", y = "Average Importance") + 
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for horizons
  theme_minimal() + 
  theme(legend.position = "bottom",  # Move legend below plot
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5, face = "bold"))  # Center and bold title


##################################################################################################################################

mean_importance_samplemean = bind_rows(h1_test_gbm_mean$save.importance) %>%
  group_by(var) %>%
  summarize(mean_importance = mean(`rel.inf`, na.rm = TRUE)) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(horizon = 1)

mean_importance_samplemean_h3 = bind_rows(h3_test_gbm_mean$save.importance) %>%
  group_by(var) %>%
  summarize(mean_importance = mean(`rel.inf`, na.rm = TRUE)) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(horizon = 3)

mean_importance_samplemean_h6 = bind_rows(h6_test_gbm_mean$save.importance) %>%
  group_by(var) %>%
  summarize(mean_importance = mean(`rel.inf`, na.rm = TRUE)) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(horizon = 6)


df_plot_samplemean = rbind(mean_importance_samplemean, mean_importance_samplemean_h3, mean_importance_samplemean_h6) %>%
  group_by(group, horizon)

# save result
saveRDS(df_plot, file = "data/boosting_gbm_importance_samplemean.RDS")


# df_plot_samplemean = readRDS("data/boosting_gbm_importance_samplemean.RDS")
ggplot(df_plot_samplemean, aes(x = factor(horizon), y = average_importance, fill = group)) + 
  geom_bar(stat = "identity", position = "stack") +  # Stack bars by horizon
  geom_text(aes(label = round(average_importance, 2)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Position text labels at center of each stack
  labs(title = "GBM Average Feature Importance By Type (sample mean method)", x = "Forecast Horizon", y = "Average Importance") + 
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for horizons
  theme_minimal() + 
  theme(legend.position = "bottom",  # Move legend 3a2below plot
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5, face = "bold"))  # Center and bold title


##################################################################################################################################

mean_importance_xgb = bind_rows(h1_test_xgb$save.importance) %>% 
  group_by(`Feature`) %>%
  summarize(mean_importance = mean(`Gain`, na.rm = TRUE)) %>%
  rename(var = Feature) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(average_importance = average_importance * 100 / sum(average_importance),
         horizon = 1)

mean_importance_xgb_h3 = bind_rows(h3_test_xgb$save.importance) %>% 
  group_by(`Feature`) %>%
  summarize(mean_importance = mean(`Gain`, na.rm = TRUE)) %>%
  rename(var = Feature) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(average_importance = average_importance * 100 / sum(average_importance),
         horizon = 3)

mean_importance_xgb_h6 = bind_rows(h6_test_xgb$save.importance) %>% 
  group_by(`Feature`) %>%
  summarize(mean_importance = mean(`Gain`, na.rm = TRUE)) %>%
  rename(var = Feature) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(average_importance = average_importance * 100 / sum(average_importance),
         horizon = 6)


df_plot_xgb = rbind(mean_importance_xgb, mean_importance_xgb_h3, mean_importance_xgb_h6) %>%
  group_by(group, horizon)

# save result
saveRDS(df_plot_xgb, file = "data/boosting_xgb_importance.RDS")


# df_plot_xgb = readRDS("data/boosting_xgb_importance.RDS")
ggplot(df_plot_xgb, aes(x = factor(horizon), y = average_importance, fill = group)) + 
  geom_bar(stat = "identity", position = "stack") +  # Stack bars by horizon
  geom_text(aes(label = round(average_importance, 2)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Position text labels at center of each stack
  labs(title = "XGB Average Feature Importance By Type", x = "Forecast Horizon", y = "Average Importance") + 
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for horizons
  theme_minimal() + 
  theme(legend.position = "bottom",  # Move legend below plot
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5, face = "bold"))  # Center and bold title


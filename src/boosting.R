rm(list=ls())

# install.packages("randomForest")
# install.packages("xgboost")
library(randomForest)
library(gbm)
library(dplyr)
library(xgboost)

data = readRDS("data/final_cleaned_data_with_bull_bear.RDS")
data$DATE <- as.Date(data$DATE)
# Sort the data by date
data <- data[order(data$DATE), ]

data <- data[-(1:17), ]  # align for missing data due to 17 lags of Y
data = head(data, -6) # remove last 6 rows due to inaccurate market state
Y = data$market_state

# get date column for test set
test_date = tail(data$DATE, 150)

# remove DATE column
data = data %>% select(-DATE)

ntest = 150
ncrossv = 100 
ntrain = nrow(data) - ncrossv - ntest


# h=1, X from lag 1-6, Y starts with lag7
X_h1 = data %>% 
  select(starts_with("lag")) %>% # remove present values 
  select(-starts_with("lag7_"), -starts_with("lag8_"), -starts_with("lag9_"), -starts_with("lag10_"), -starts_with("lag11_"), ends_with("market_state")) %>% # keep lag 1-6 for X variables, but keep all lags of Y first
  select(-c(lag1_market_state, lag2_market_state, lag3_market_state, lag4_market_state, lag5_market_state, lag6_market_state, 
            lag13_market_state, lag14_market_state, lag15_market_state, lag16_market_state, lag17_market_state)) %>% # keep lag 7-12 for Y
  as.matrix()

X_h3 = data %>% 
  select(starts_with("lag")) %>% # remove present values 
  select(-starts_with("lag1_")) %>% select(-starts_with("lag2_")) %>% 
  select(-starts_with("lag9_"), -starts_with("lag10_"), -starts_with("lag11_"), ends_with("market_state")) %>% # keep lag 3-8 for X variables, but keep all lags of Y first
  select(-c(lag3_market_state, lag4_market_state, lag5_market_state, lag6_market_state, lag7_market_state, lag8_market_state,
            lag15_market_state, lag16_market_state, lag17_market_state)) %>% # keep lag 9-14 for Y
  as.matrix()

X_h6 = data %>% 
  select(starts_with("lag")) %>% # remove present values 
  select(-starts_with("lag1_"), -starts_with("lag2_"), -starts_with("lag3_"), -starts_with("lag4_"), -starts_with("lag5_")) %>% # keep lag 6-11 for X variables
  select(-c(lag6_market_state, lag7_market_state, lag8_market_state, lag9_market_state, lag10_market_state, lag11_market_state)) %>% # keep lag 12-17 for Y
  as.matrix()


######################################################################################################################################

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
  temp.fit = predict(temp.boost, as.data.frame(X.out), n.trees = seq(1, M, 1), type = "response") #prediction for d=5
  return(list("model"=temp.boost,"pred"=temp.fit))
}


######################################################################################################################################


# cross validation
# h=1, X from lag 1-6, Y starts with lag7
X_CV_h1 = head(X_h1, ntrain+ncrossv)   # remove test set
y_cv = head(Y, ntrain+ncrossv)
M = 10000 # Max number of trees considered
cv_boost = matrix(0,ncrossv,M) #blank for CV criteria, d=5
cv_boost2 = matrix(0,ncrossv,M) #blank for CV criteria, d=2
for(i in ncrossv:1){#NB: backwards FOR loop: going from 100 down to 1
  X.window = X_CV_h1[(1+ncrossv-i):(nrow(X_CV_h1)-i),] #define the estimation window (first one: 1 to 321, then 2 to 322 etc, till 100 to 420)
  y.window = y_cv[(1+ncrossv-i):(length(y_cv)-i)]
  # boost = runboost(X.window, y.window)
  boost2 = runboost2(X.window, y.window)
  # cv_boost[(1+ncrossv-i), ] = matrix(boost$pred > 0.5) # save the forecast
  cv_boost2[(1+ncrossv-i), ] = matrix(boost2$pred > 0.5) # save the forecast
  cat("iteration", (1+ncrossv-i), "\n") # display iteration number
}

real = tail(y_cv, ncrossv)
missclass = colSums(abs(real-cv_boost)) # compute misclassification rate
bestM = which.min(missclass) # bestM = 362
cv_min = min(missclass/ncrossv) # cv_min=0.14

missclass2 = colSums(abs(real-cv_boost2)) # compute misclassification rate
bestM2 = which.min(missclass2) # bestM = 1013
cv_min2 = min(missclass2/ncrossv) # cv_min = 0.13






# h=3, X from lag 3-8, Y starts with lag7
X_CV_h3 = head(X_h3, ntrain+ncrossv)   # remove test set
y_cv = head(Y, ntrain+ncrossv)
M = 10000 # Max number of trees considered
cv_boost_h3 = matrix(0,ncrossv,M) #blank for CV criteria, d=5
cv_boost2_h3 = matrix(0,ncrossv,M) #blank for CV criteria, d=2
for(i in ncrossv:1){#NB: backwards FOR loop: going from 100 down to 1
  X.window = X_CV_h3[(1+ncrossv-i):(nrow(X_CV_h3)-i),] #define the estimation window (first one: 1 to 321, then 2 to 322 etc, till 100 to 420)
  y.window = y_cv[(1+ncrossv-i):(length(y_cv)-i)]
  # boost = runboost(X.window, y.window)
  boost2 = runboost2(X.window, y.window)
  # cv_boost_h3[(1+ncrossv-i), ] = matrix(boost$pred > 0.5) # save the forecast
  cv_boost2_h3[(1+ncrossv-i), ] = matrix(boost2$pred > 0.5) # save the forecast
  cat("iteration", (1+ncrossv-i), "\n") # display iteration number
}

real = tail(y_cv, ncrossv)
missclass_h3 = colSums(abs(real-cv_boost_h3)) # compute misclassification rate
bestM_h3 = which.min(missclass_h3) # bestM = 529
cv_min_h3 = min(missclass_h3/ncrossv) # cv_min=0.14

missclass2_h3 = colSums(abs(real-cv_boost2_h3)) # compute misclassification rate
bestM2_h3 = which.min(missclass2_h3) # bestM = 1303
cv_min2_h3 = min(missclass2_h3/ncrossv) # cv_min = 0.14




# h=6, X from lag 6-11, Y starts with lag7
X_CV_h6 = head(X_h6, ntrain+ncrossv)   # remove test set
y_cv = head(Y, ntrain+ncrossv)
M = 10000 # Max number of trees considered
cv_boost_h6 = matrix(0,ncrossv,M) #blank for CV criteria, d=5
cv_boost2_h6 = matrix(0,ncrossv,M) #blank for CV criteria, d=2
for(i in ncrossv:1){#NB: backwards FOR loop: going from 100 down to 1
  X.window = X_CV_h6[(1+ncrossv-i):(nrow(X_CV_h6)-i),] #define the estimation window (first one: 1 to 321, then 2 to 322 etc, till 100 to 420)
  y.window = y_cv[(1+ncrossv-i):(length(y_cv)-i)]
  # boost = runboost(X.window, y.window)
  boost2 = runboost2(X.window, y.window)
  # cv_boost_h6[(1+ncrossv-i), ] = matrix(boost$pred > 0.5) # save the forecast
  cv_boost2_h6[(1+ncrossv-i), ] = matrix(boost2$pred > 0.5) # save the forecast
  cat("iteration", (1+ncrossv-i), "\n") # display iteration number
}

real = tail(y_cv, ncrossv)
missclass_h6 = colSums(abs(real-cv_boost_h6)) # compute misclassification rate
bestM_h6 = which.min(missclass_h6) # bestM = 572
cv_min_h6 = min(missclass_h6/ncrossv) # cv_min=0.14

missclass2_h6 = colSums(abs(real-cv_boost2_h6)) # compute misclassification rate
bestM2_h6 = which.min(missclass2_h6) # bestM = 1241
cv_min2_h6 = min(missclass2_h6/ncrossv) # cv_min = 0.14


######################################################################################################################################



# test
test_result <- data.frame(matrix(NA, nrow = 150, ncol = 3))
colnames(test_result) <- c("1-step ahead forecast", "3-step ahead forecast", "6-step ahead forecast")

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


# h=1: best model -- tree size = bestM = 362
test_X_h1 = X_h1
y_test = Y
test_boost = matrix(0,ntest,1) #blank for CV criteria,
save.importance = list()
for(i in ntest:1){#NB: backwards FOR loop: going from 150 down to 1
  X.window = test_X_h1[(1+ntest-i):(nrow(test_X_h1)-i),] #define the estimation window (first one: 1 to 421, then 2 to 422 etc, till 150 to 570)
  y.window = y_test[(1+ntest-i):(length(y_test)-i)]
  boost = runboost_setn(X.window, y.window, n=362)
  test_boost[(1+ntest-i), 1] = boost$pred # save the forecast
  save.importance[[i]]=boost$importance #save variable importance
  cat("iteration", (1+ntest-i), "\n") # display iteration number
}
test_result$`1-step ahead forecast` = test_boost

# feature importance plot
price_div_earn = grep("(ret(x)?|ratio|yield|payout|book_market|fbm|price|dividend|earnings|rate_gs10|TR_CAPE)$", colnames(data), value = TRUE) # ret, retx, dividend_price_ratio, dividend_yield, earnings_price_ratio, dividend_payout, book_market, fbm, price, dividend, earnings, rate_gs10, TR_CAPE
return_yield = grep("(AA|lty|ltr|corpr|tbl|Rfree|tms|dfy|dfr|ygap|returns)$", colnames(data), value = TRUE) # AAA, BAA, lty, ltr, corpr, Rfree, term_spread, dfy, dfr, ygap, monthly_total_bond_returns
econ_indicator = grep("(infl|ntis|ogap|wtexas|CPI|UNRATE|DFF|INDPRO)$", colnames(data), value = TRUE) # infl, ntis, ogap, wtexas, CPI, UNRATE, DFF, INDPRO
risk_measure = grep("(svar|skvw|tail|shtint|lzrt|rdsp)$", colnames(data), value = TRUE) # svar, skvw, tail, shtint, lzrt, rdsp
investment_finratio = grep("(sntm|ndrbl)$", colnames(data), value = TRUE) # sntm, ndrbl
technical_indicator = grep("(dtoy|dtoat|tchi|avgcor)$", colnames(data), value = TRUE) # dtoy, dtoat, tchi, avgcor
lag_market_state = grep("market_state$", colnames(data), value = TRUE)

# Define the variable groups as a named list
variable_groups <- list(
  "Price, Dividends, and Earnings" = price_div_earn,
  "Returns and Yields" = return_yield,
  "Economic Indicators" = econ_indicator,
  "Risk Measures" = risk_measure,
  "Investment and Financial Ratios" = investment_finratio,
  "Market Sentiment and Technical Indicators" = technical_indicator,
  "Lag Market State" = lag_market_state
)

# Convert the list into a lookup table to match variables with their group
variable_lookup <- stack(variable_groups) %>%
  rename(var = values, group = ind)

mean_importance = bind_rows(save.importance) %>%
  group_by(var) %>%
  summarize(mean_importance = mean(`rel.inf`, na.rm = TRUE)) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(horizon = 1)

# ggplot(mean_importance_grouped, aes(x = reorder(group, average_importance), y = average_importance, fill = group)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   geom_text(aes(label = round(average_importance, 2)), hjust = -0.2, size = 3.5) +  # Add text labels with rounded values
#   labs(title = "Average Feature Importance h=1", x = "", y = "Average Importance") +
#   scale_fill_brewer(palette = "Set3") +  # Use a color palette for groups
#   theme_minimal() +
#   theme(legend.position = "none",  # Remove legend as groups are labeled
#         plot.title = element_text(hjust = 0.5, face = "bold"))  # Center and bold title



# h=3: best model -- depth=5, tree size = bestM_h3 = 529
test_X_h3 = X_h3
y_test = Y
test_boost_h3 = matrix(0,ntest,1) #blank for CV criteria, d=5
save.importance_h3 = list()
for(i in ntest:1){#NB: backwards FOR loop: going from 150 down to 1
  X.window = test_X_h3[(1+ntest-i):(nrow(test_X_h3)-i),] #define the estimation window (first one: 1 to 421, then 2 to 422 etc, till 150 to 570)
  y.window = y_test[(1+ntest-i):(length(y_test)-i)]
  boost = runboost_setn(X.window, y.window, n=529)
  test_boost_h3[(1+ntest-i), 1] = boost$pred # save the forecast
  save.importance_h3[[i]]=boost$importance #save variable importance
  cat("iteration", (1+ntest-i), "\n") # display iteration number
}
test_result$`3-step ahead forecast` = test_boost_h3

mean_importance_h3 = bind_rows(save.importance_h3) %>%
  group_by(var) %>%
  summarize(mean_importance = mean(`rel.inf`, na.rm = TRUE)) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(horizon = 3)


# h=6: best model -- depth=5, tree size = bestM_h6 = 572
test_X_h6 = X_h6
y_test = Y
test_boost_h6 = matrix(0,ntest,1) #blank for CV criteria, d=5
save.importance_h6 = list()
for(i in ntest:1){#NB: backwards FOR loop: going from 150 down to 1
  X.window = test_X_h6[(1+ntest-i):(nrow(test_X_h6)-i),] #define the estimation window (first one: 1 to 421, then 2 to 422 etc, till 150 to 570)
  y.window = y_test[(1+ntest-i):(length(y_test)-i)]
  boost = runboost_setn(X.window, y.window, n=572)
  test_boost_h6[(1+ntest-i), 1] = boost$pred # save the forecast
  save.importance_h6[[i]]=boost$importance #save variable importance
  cat("iteration", (1+ntest-i), "\n") # display iteration number
}
test_result$`6-step ahead forecast` = test_boost_h6

mean_importance_h6 = bind_rows(save.importance_h6) %>%
  group_by(var) %>%
  summarize(mean_importance = mean(`rel.inf`, na.rm = TRUE)) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(horizon = 6)


df_plot = rbind(mean_importance, mean_importance_h3, mean_importance_h6) %>%
  group_by(group, horizon)


# save result
saveRDS(df_plot, file = "data/boosting_gbm_importance.RDS")


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


# save result
test_result = cbind(test_date, test_result)
saveRDS(test_result, file = "data/boosting_gbm_prediction.RDS")



######################################################################################################################################



# CV -- sample mean method, h=1
y_train = head(Y, ntrain)
y_sample_mean = mean(y_train)

cv_boost_mean = matrix(0,ncrossv,M) #blank for CV criteria, d=5
cv_boost2_mean = matrix(0,ncrossv,M) #blank for CV criteria, d=2
for(i in ncrossv:1){#NB: backwards FOR loop: going from 100 down to 1
  X.window = X_CV_h1[(1+ncrossv-i):(nrow(X_CV_h1)-i),] #define the estimation window (first one: 1 to 332, then 2 to 333 etc, till 100 to 431.)
  y.window = y_cv[(1+ncrossv-i):(length(y_cv)-i)]
  boost = runboost(X.window, y.window)
  # boost2 = runboost2(X.window, y.window)
  cv_boost_mean[(1+ncrossv-i), ] = matrix(boost$pred > y_sample_mean) # save the forecast
  # cv_boost2_mean[(1+ncrossv-i), ] = matrix(boost2$pred > y_sample_mean) # save the forecast
  cat("iteration", (1+ncrossv-i), "\n") # display iteration number
}

real = tail(y_cv, ncrossv)
missclass_mean = colSums(abs(real-cv_boost_mean)) # compute misclassification rate
bestM_mean = which.min(missclass_mean) # bestM = 737
cv_min_mean = min(missclass_mean/ncrossv) # cv_min=0.14

missclass2_mean = colSums(abs(real-cv_boost2_mean)) # compute misclassification rate
bestM2_mean = which.min(missclass2_mean) # bestM = 1602
cv_min2_mean = min(missclass2_mean/ncrossv) # cv_min=0.14


# CV -- sample mean method, h=3
cv_boost_mean_h3 = matrix(0,ncrossv,M) #blank for CV criteria, d=5
for(i in ncrossv:1){#NB: backwards FOR loop: going from 100 down to 1
  X.window = X_CV_h3[(1+ncrossv-i):(nrow(X_CV_h3)-i),] #define the estimation window (first one: 1 to 332, then 2 to 333 etc, till 100 to 431.)
  y.window = y_cv[(1+ncrossv-i):(length(y_cv)-i)]
  boost = runboost(X.window, y.window)
  cv_boost_mean_h3[(1+ncrossv-i), ] = matrix(boost$pred > y_sample_mean) # save the forecast
  cat("iteration", (1+ncrossv-i), "\n") # display iteration number
}

real = tail(y_cv, ncrossv)
missclass_mean_h3 = colSums(abs(real-cv_boost_mean_h3)) # compute misclassification rate
bestM_mean_h3 = which.min(missclass_mean_h3) # bestM = 671
cv_min_mean_h3 = min(missclass_mean_h3/ncrossv) # cv_min=0.14


# CV -- sample mean method, h=6
cv_boost_mean_h6 = matrix(0,ncrossv,M) #blank for CV criteria, d=5
for(i in ncrossv:1){#NB: backwards FOR loop: going from 100 down to 1
  X.window = X_CV_h6[(1+ncrossv-i):(nrow(X_CV_h6)-i),] #define the estimation window (first one: 1 to 332, then 2 to 333 etc, till 100 to 431.)
  y.window = y_cv[(1+ncrossv-i):(length(y_cv)-i)]
  boost = runboost(X.window, y.window)
  cv_boost_mean_h6[(1+ncrossv-i), ] = matrix(boost$pred > y_sample_mean) # save the forecast
  cat("iteration", (1+ncrossv-i), "\n") # display iteration number
}

real = tail(y_cv, ncrossv)
missclass_mean_h6 = colSums(abs(real-cv_boost_mean_h6)) # compute misclassification rate
bestM_mean_h6 = which.min(missclass_mean_h6) # bestM = 523
cv_min_mean_h6 = min(missclass_mean_h6/ncrossv) # cv_min=0.14


######################################################################################################################################



# test -- for sample mean method, h=1
test_result_sample_mean <- data.frame(matrix(NA, nrow = 150, ncol = 3))
colnames(test_result_sample_mean) <- c("1-step ahead forecast", "3-step ahead forecast", "6-step ahead forecast")
# h=1: best model -- tree size = bestM_mean = 737
test_X_h1 = X_h1
y_test = Y
test_boost_mean = matrix(0,ntest,1)
save.importance_samplemean = list()
for(i in ntest:1){#NB: backwards FOR loop: going from 150 down to 1
  X.window = test_X_h1[(1+ntest-i):(nrow(test_X_h1)-i),] #define the estimation window (first one: 1 to 421, then 2 to 422 etc, till 150 to 570)
  y.window = y_test[(1+ntest-i):(length(y_test)-i)]
  boost = runboost_setn(X.window, y.window, n=737)
  test_boost_mean[(1+ntest-i), 1] = boost$pred # save the forecast
  save.importance_samplemean[[i]]=boost$importance #save variable importance
  cat("iteration", (1+ntest-i), "\n") # display iteration number
}
test_result_sample_mean$`1-step ahead forecast` = test_boost_mean


mean_importance_samplemean = bind_rows(save.importance_samplemean) %>%
  group_by(var) %>%
  summarize(mean_importance = mean(`rel.inf`, na.rm = TRUE)) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(horizon = 1)


# h=3: best model -- tree size = bestM_mean_h3 = 671
test_X_h3 = X_h3
y_test = Y
test_boost_mean_h3 = matrix(0,ntest,1)
save.importance_samplemean_h3 = list()
for(i in ntest:1){#NB: backwards FOR loop: going from 150 down to 1
  X.window = test_X_h3[(1+ntest-i):(nrow(test_X_h3)-i),] #define the estimation window (first one: 1 to 421, then 2 to 422 etc, till 150 to 570)
  y.window = y_test[(1+ntest-i):(length(y_test)-i)]
  boost = runboost_setn(X.window, y.window, n=671)
  test_boost_mean_h3[(1+ntest-i), 1] = boost$pred # save the forecast
  save.importance_samplemean_h3[[i]]=boost$importance #save variable importance
  cat("iteration", (1+ntest-i), "\n") # display iteration number
}
test_result_sample_mean$`3-step ahead forecast` = test_boost_mean_h3


mean_importance_samplemean_h3 = bind_rows(save.importance_samplemean_h3) %>%
  group_by(var) %>%
  summarize(mean_importance = mean(`rel.inf`, na.rm = TRUE)) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(horizon = 3)


# h=6: best model -- tree size = bestM_mean_h6 = 523
test_X_h6 = X_h6
y_test = Y
test_boost_mean_h6 = matrix(0,ntest,1)
save.importance_samplemean_h6 = list()
for(i in ntest:1){#NB: backwards FOR loop: going from 150 down to 1
  X.window = test_X_h6[(1+ntest-i):(nrow(test_X_h6)-i),] #define the estimation window (first one: 1 to 421, then 2 to 422 etc, till 150 to 570)
  y.window = y_test[(1+ntest-i):(length(y_test)-i)]
  boost = runboost_setn(X.window, y.window, n=523)
  test_boost_mean_h6[(1+ntest-i), 1] = boost$pred # save the forecast
  save.importance_samplemean_h6[[i]]=boost$importance #save variable importance
  cat("iteration", (1+ntest-i), "\n") # display iteration number
}
test_result_sample_mean$`6-step ahead forecast` = test_boost_mean_h6


mean_importance_samplemean_h6 = bind_rows(save.importance_samplemean_h6) %>%
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



# save result
test_result_sample_mean = cbind(test_date, test_result_sample_mean)
saveRDS(test_result_sample_mean, file = "data/boosting_gbm_sample_mean_prediction.RDS")



######################################################################################################################################




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



# cross validation
# h=1, X from lag 1-6, Y starts with lag7
X_CV_h1 = head(X_h1, ntrain+ncrossv)   # remove test set
y_cv = head(Y, ntrain+ncrossv)
M = 10000 # Max number of trees considered
cv_boost_xgb = matrix(0,ncrossv,M) #blank for CV criteria, d=5
cv_boost2_xgb = matrix(0,ncrossv,M) #blank for CV criteria, d=2
for(i in ncrossv:1){#NB: backwards FOR loop: going from 100 down to 1
  X.window = X_CV_h1[(1+ncrossv-i):(nrow(X_CV_h1)-i),] #define the estimation window (first one: 1 to 321, then 2 to 322 etc, till 100 to 420)
  y.window = y_cv[(1+ncrossv-i):(length(y_cv)-i)]
  # boost = runboost_xgb(X.window, y.window)
  boost2 = runboost2_xgb(X.window, y.window)
  # cv_boost_xgb[(1+ncrossv-i), ] = matrix(boost$pred > 0.5) # save the forecast
  cv_boost2_xgb[(1+ncrossv-i), ] = matrix(boost2$pred > 0.5) # save the forecast
  cat("iteration", (1+ncrossv-i), "\n") # display iteration number
}

real = tail(y_cv, ncrossv)
missclass_xgb = colSums(abs(real-cv_boost_xgb)) # compute misclassification rate
bestM_xgb = which.min(missclass_xgb) # bestM = 40
cv_min_xgb = min(missclass_xgb/ncrossv) # cv_min=0.14

missclass2_xgb = colSums(abs(real-cv_boost2_xgb)) # compute misclassification rate
bestM2_xgb = which.min(missclass2_xgb) # bestM = 886
cv_min2_xgb = min(missclass2_xgb/ncrossv) # cv_min = 0.14


# h=3, X from lag 3-8, Y starts with lag7
X_CV_h3 = head(X_h3, ntrain+ncrossv)   # remove test set
y_cv = head(Y, ntrain+ncrossv)
M = 10000 # Max number of trees considered
cv_boost_xgb_h3 = matrix(0,ncrossv,M) #blank for CV criteria, d=5
cv_boost2_xgb_h3 = matrix(0,ncrossv,M) #blank for CV criteria, d=2
for(i in ncrossv:1){#NB: backwards FOR loop: going from 100 down to 1
  X.window = X_CV_h3[(1+ncrossv-i):(nrow(X_CV_h3)-i),] #define the estimation window (first one: 1 to 321, then 2 to 322 etc, till 100 to 420)
  y.window = y_cv[(1+ncrossv-i):(length(y_cv)-i)]
  # boost = runboost_xgb(X.window, y.window)
  boost2 = runboost2_xgb(X.window, y.window)
  # cv_boost_xgb_h3[(1+ncrossv-i), ] = matrix(boost$pred > 0.5) # save the forecast
  cv_boost2_xgb_h3[(1+ncrossv-i), ] = matrix(boost2$pred > 0.5) # save the forecast
  cat("iteration", (1+ncrossv-i), "\n") # display iteration number
}

real = tail(y_cv, ncrossv)
missclass_xgb_h3 = colSums(abs(real-cv_boost_xgb_h3)) # compute misclassification rate
bestM_xgb_h3 = which.min(missclass_xgb_h3) # bestM = 174
cv_min_xgb_h3 = min(missclass_xgb_h3/ncrossv) # cv_min=0.13

missclass2_xgb_h3 = colSums(abs(real-cv_boost2_xgb_h3)) # compute misclassification rate
bestM2_xgb_h3 = which.min(missclass2_xgb_h3) # bestM = 1052
cv_min2_xgb_h3 = min(missclass2_xgb_h3/ncrossv) # cv_min = 0.13



# h=6, X from lag 6-11, Y starts with lag7
X_CV_h6 = head(X_h6, ntrain+ncrossv)   # remove test set
y_cv = head(Y, ntrain+ncrossv)
M = 10000 # Max number of trees considered
cv_boost_xgb_h6 = matrix(0,ncrossv,M) #blank for CV criteria, d=5
cv_boost2_xgb_h6 = matrix(0,ncrossv,M) #blank for CV criteria, d=2
for(i in ncrossv:1){#NB: backwards FOR loop: going from 100 down to 1
  X.window = X_CV_h6[(1+ncrossv-i):(nrow(X_CV_h6)-i),] #define the estimation window (first one: 1 to 321, then 2 to 322 etc, till 100 to 420)
  y.window = y_cv[(1+ncrossv-i):(length(y_cv)-i)]
  # boost = runboost_xgb(X.window, y.window)
  boost2 = runboost2_xgb(X.window, y.window)
  # cv_boost_xgb_h6[(1+ncrossv-i), ] = matrix(boost$pred > 0.5) # save the forecast
  cv_boost2_xgb_h6[(1+ncrossv-i), ] = matrix(boost2$pred > 0.5) # save the forecast
  cat("iteration", (1+ncrossv-i), "\n") # display iteration number
}

real = tail(y_cv, ncrossv)
missclass_xgb_h6 = colSums(abs(real-cv_boost_xgb_h6)) # compute misclassification rate
bestM_xgb_h6 = which.min(missclass_xgb_h6) # bestM = 65
cv_min_xgb_h6 = min(missclass_xgb_h6/ncrossv) # cv_min=0.14

missclass2_xgb_h6 = colSums(abs(real-cv_boost2_xgb_h6)) # compute misclassification rate
bestM2_xgb_h6 = which.min(missclass2_xgb_h6) # bestM = 1034
cv_min2_xgb_h6 = min(missclass2_xgb_h6/ncrossv) # cv_min = 0.14



######################################################################################################################################



# test
test_result_xgb <- data.frame(matrix(NA, nrow = 150, ncol = 3))
colnames(test_result_xgb) <- c("1-step ahead forecast", "3-step ahead forecast", "6-step ahead forecast")

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

# h=1: best model -- depth=5, tree size = bestM_xgb = 40
test_X_h1 = X_h1
y_test = Y
save.importance_xgb = list()
test_boost_xgb = matrix(0,ntest,1) #blank for CV criteria, d=5
for(i in ntest:1){#NB: backwards FOR loop: going from 150 down to 1
  X.window = test_X_h1[(1+ntest-i):(nrow(test_X_h1)-i),] #define the estimation window (first one: 1 to 421, then 2 to 422 etc, till 150 to 570)
  y.window = y_test[(1+ntest-i):(length(y_test)-i)]
  boost = runboost_setn_xgb(X.window, y.window, n=40)
  test_boost_xgb[(1+ntest-i), 1] = boost$pred # save the forecast
  save.importance_xgb[[i]]=boost$importance #save variable importance
  cat("iteration", (1+ntest-i), "\n") # display iteration number
}
test_result_xgb$`1-step ahead forecast` = test_boost_xgb

mean_importance_xgb = bind_rows(save.importance_xgb) %>% 
  group_by(`Feature`) %>%
  summarize(mean_importance = mean(`Gain`, na.rm = TRUE)) %>%
  rename(var = Feature) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(average_importance = average_importance * 100 / sum(average_importance),
         horizon = 1)




# h=3: best model -- depth=5, tree size = bestM_xgb_h3 = 174 
test_X_h3 = X_h3
y_test = Y
save.importance_xgb_h3 = list()
test_boost_h3_xgb = matrix(0,ntest,1) #blank for CV criteria, d=5
for(i in ntest:1){#NB: backwards FOR loop: going from 150 down to 1
  X.window = test_X_h3[(1+ntest-i):(nrow(test_X_h3)-i),] #define the estimation window (first one: 1 to 421, then 2 to 422 etc, till 150 to 570)
  y.window = y_test[(1+ntest-i):(length(y_test)-i)]
  boost = runboost_setn_xgb(X.window, y.window, n=174)
  test_boost_h3_xgb[(1+ntest-i), 1] = boost$pred # save the forecast
  save.importance_xgb_h3[[i]]=boost$importance #save variable importance
  cat("iteration", (1+ntest-i), "\n") # display iteration number
}
test_result_xgb$`3-step ahead forecast` = test_boost_h3_xgb

mean_importance_xgb_h3 = bind_rows(save.importance_xgb_h3) %>% 
  group_by(`Feature`) %>%
  summarize(mean_importance = mean(`Gain`, na.rm = TRUE)) %>%
  rename(var = Feature) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(average_importance = average_importance * 100 / sum(average_importance),
         horizon = 3)



# h=6: best model -- depth=5, tree size = bestM_xgb_h6 = 65
test_X_h6 = X_h6
y_test = Y
save.importance_xgb_h6 = list()
test_boost_h6_xgb = matrix(0,ntest,1) #blank for CV criteria, d=5
for(i in ntest:1){#NB: backwards FOR loop: going from 150 down to 1
  X.window = test_X_h6[(1+ntest-i):(nrow(test_X_h6)-i),] #define the estimation window (first one: 1 to 421, then 2 to 422 etc, till 150 to 570)
  y.window = y_test[(1+ntest-i):(length(y_test)-i)]
  boost = runboost_setn_xgb(X.window, y.window, n=65)
  test_boost_h6_xgb[(1+ntest-i), 1] = boost$pred # save the forecast
  save.importance_xgb_h6[[i]]=boost$importance #save variable importance
  cat("iteration", (1+ntest-i), "\n") # display iteration number
}
test_result_xgb$`6-step ahead forecast` = test_boost_h6_xgb


mean_importance_xgb_h6 = bind_rows(save.importance_xgb_h6) %>% 
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


# Save result
test_result_xgb = cbind(test_date, test_result_xgb)
saveRDS(test_result_xgb, file = "data/boosting_xgb_prediction.RDS")




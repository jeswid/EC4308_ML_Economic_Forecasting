rm(list=ls())

# install.packages("randomForest")
# install.packages("xgboost")
library(randomForest)
library(gbm)
library(ROCR)
library(dplyr)
library(xgboost)

data = readRDS("data/final_cleaned_data_with_bull_bear.RDS")
data$DATE <- as.Date(data$DATE)
# Sort the data by date
data <- data[order(data$DATE), ]

# remove DATE column
data = data %>% select(-DATE)

data <- data[-(1:12), ]  # align for missing data due to 12 lags of Y
Y = data$market_state

ntest = 150
ncrossv = 100
ntrain = nrow(data) - ncrossv - ntest

# h=1, X from lag 1-6, Y starts with lag7
X_h1 = data %>% 
  select(starts_with("lag")) %>% # remove present values 
  select(-starts_with("lag7_"), -starts_with("lag8_"), -starts_with("lag9_"), -starts_with("lag10_"), -starts_with("lag11_"), ends_with("market_state")) %>% # keep lag 1-6 for X variables, but keep all lags of Y first
  select(-c(lag1_market_state, lag2_market_state, lag3_market_state, lag4_market_state, lag5_market_state, lag6_market_state, # remove first 6 lags of Y
            ends_with("excess_CAPE_yield"))) %>%
  as.matrix()

X_h3 = data %>% 
  select(starts_with("lag")) %>% # remove present values 
  select(-starts_with("lag1_")) %>% select(-starts_with("lag2_")) %>% 
  select(-starts_with("lag9_"), -starts_with("lag10_"), -starts_with("lag11_"), ends_with("market_state")) %>% # keep lag 3-8 for X variables, but keep all lags of Y first
  select(-c(lag3_market_state, lag4_market_state, lag5_market_state, lag6_market_state, # remove first 6 lags of Y
            ends_with("excess_CAPE_yield"))) %>%
  as.matrix()

X_h6 = data %>% 
  select(starts_with("lag")) %>% # remove present values 
  select(-starts_with("lag1_"), -starts_with("lag2_"), -starts_with("lag3_"), -starts_with("lag4_"), -starts_with("lag5_"), ends_with("market_state")) %>% # keep lag 6-11 for X variables, but keep all lags of Y first
  select(-c(lag1_market_state, lag2_market_state, lag3_market_state, lag4_market_state, lag5_market_state, lag6_market_state, # remove first 6 lags of Y
            ends_with("excess_CAPE_yield"))) %>%
  as.matrix()

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

# cross validation
# h=1, X from lag 1-6, Y starts with lag7
X_CV_h1 = head(X_h1, ntrain+ncrossv)   # remove test set
y_cv = head(Y, ntrain+ncrossv)
M = 10000 # Max number of trees considered
cv_boost = matrix(0,ncrossv,M) #blank for CV criteria, d=5
cv_boost2 = matrix(0,ncrossv,M) #blank for CV criteria, d=2
for(i in ncrossv:1){#NB: backwards FOR loop: going from 100 down to 1
  X.window = X_CV_h1[(1+ncrossv-i):(nrow(X_CV_h1)-i),] #define the estimation window (first one: 1 to 332, then 2 to 333 etc, till 100 to 431.)
  y.window = y_cv[(1+ncrossv-i):(length(y_cv)-i)]
  boost = runboost(X.window, y.window)
  # boost2 = runboost2(X.window, y.window)
  cv_boost[(1+ncrossv-i), ] = matrix(boost$pred > 0.5) # save the forecast
  # cv_boost2[(1+ncrossv-i), ] = matrix(boost2$pred > 0.5) # save the forecast
  cat("iteration", (1+ncrossv-i), "\n") # display iteration number
}

real = tail(y_cv, ncrossv)
missclass = colSums(abs(real-cv_boost)) # compute misclassification rate
bestM = which.min(missclass) # bestM = 466
cv_min = min(missclass/ncrossv) # cv_min=0.13

missclass2 = colSums(abs(real-cv_boost2)) # compute misclassification rate
bestM2 = which.min(missclass2) # bestM = 1005
cv_min2 = min(missclass2/ncrossv) # cv_min = 0.13





# h=3, X from lag 3-8, Y starts with lag7
X_CV_h3 = head(X_h3, ntrain+ncrossv)   # remove test set
y_cv = head(Y, ntrain+ncrossv)
M = 10000 # Max number of trees considered
cv_boost_h3 = matrix(0,ncrossv,M) #blank for CV criteria, d=5
cv_boost2_h3 = matrix(0,ncrossv,M) #blank for CV criteria, d=2
for(i in ncrossv:1){#NB: backwards FOR loop: going from 100 down to 1
  X.window = X_CV_h3[(1+ncrossv-i):(nrow(X_CV_h3)-i),] #define the estimation window (first one: 1 to 332, then 2 to 333 etc, till 100 to 431.)
  y.window = y_cv[(1+ncrossv-i):(length(y_cv)-i)]
  boost = runboost(X.window, y.window)
  # boost2 = runboost2(X.window, y.window)
  cv_boost_h3[(1+ncrossv-i), ] = matrix(boost$pred > 0.5) # save the forecast
  # cv_boost2_h3[(1+ncrossv-i), ] = matrix(boost2$pred > 0.5) # save the forecast
  cat("iteration", (1+ncrossv-i), "\n") # display iteration number
}

real = tail(y_cv, ncrossv)
missclass_h3 = colSums(abs(real-cv_boost_h3)) # compute misclassification rate
bestM_h3 = which.min(missclass_h3) # bestM = 523
cv_min_h3 = min(missclass_h3/ncrossv) # cv_min=0.13

missclass2_h3 = colSums(abs(real-cv_boost2_h3)) # compute misclassification rate
bestM2_h3 = which.min(missclass2_h3) # bestM = 1385
cv_min2_h3 = min(missclass2_h3/ncrossv) # cv_min = 0.14




# h=6, X from lag 6-11, Y starts with lag7
X_CV_h6 = head(X_h6, ntrain+ncrossv)   # remove test set
y_cv = head(Y, ntrain+ncrossv)
M = 10000 # Max number of trees considered
cv_boost_h6 = matrix(0,ncrossv,M) #blank for CV criteria, d=5
cv_boost2_h6 = matrix(0,ncrossv,M) #blank for CV criteria, d=2
for(i in ncrossv:1){#NB: backwards FOR loop: going from 100 down to 1
  X.window = X_CV_h6[(1+ncrossv-i):(nrow(X_CV_h6)-i),] #define the estimation window (first one: 1 to 332, then 2 to 333 etc, till 100 to 431.)
  y.window = y_cv[(1+ncrossv-i):(length(y_cv)-i)]
  boost = runboost(X.window, y.window)
  # boost2 = runboost2(X.window, y.window)
  cv_boost_h6[(1+ncrossv-i), ] = matrix(boost$pred > 0.5) # save the forecast
  # cv_boost2_h6[(1+ncrossv-i), ] = matrix(boost2$pred > 0.5) # save the forecast
  cat("iteration", (1+ncrossv-i), "\n") # display iteration number
}

real = tail(y_cv, ncrossv)
missclass_h6 = colSums(abs(real-cv_boost_h6)) # compute misclassification rate
bestM_h6 = which.min(missclass_h6) # bestM = 418
cv_min_h6 = min(missclass_h6/ncrossv) # cv_min=0.14

missclass2_h6 = colSums(abs(real-cv_boost2_h6)) # compute misclassification rate
bestM2_h6 = which.min(missclass2_h6) # bestM = 1329
cv_min2_h6 = min(missclass2_h6/ncrossv) # cv_min = 0.14


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
  return(list("model"=temp.boost,"pred"=temp.fit))
}

# h=1: best model -- depth=5, tree size=466
test_X_h1 = X_h1
y_test = Y
test_boost = matrix(0,ntest,1) #blank for CV criteria, d=5
for(i in ntest:1){#NB: backwards FOR loop: going from 150 down to 1
  X.window = test_X_h1[(1+ntest-i):(nrow(test_X_h1)-i),] #define the estimation window (first one: 1 to 432, then 2 to 433 etc, till 150 to 581.)
  y.window = y_test[(1+ntest-i):(length(y_test)-i)]
  boost = runboost_setn(X.window, y.window, n=466)
  test_boost[(1+ntest-i), 1] = boost$pred > 0.5 # save the forecast
  cat("iteration", (1+ntest-i), "\n") # display iteration number
}
test_result$`1-step ahead forecast` = test_boost

# h=3: best model -- depth=5, tree size = 523
test_X_h3 = X_h3
y_test = Y
test_boost_h3 = matrix(0,ntest,1) #blank for CV criteria, d=5
for(i in ntest:1){#NB: backwards FOR loop: going from 150 down to 1
  X.window = test_X_h3[(1+ntest-i):(nrow(test_X_h3)-i),] #define the estimation window (first one: 1 to 432, then 2 to 433 etc, till 150 to 581.)
  y.window = y_test[(1+ntest-i):(length(y_test)-i)]
  boost = runboost_setn(X.window, y.window, n=523)
  test_boost_h3[(1+ntest-i), 1] = boost$pred > 0.5 # save the forecast
  cat("iteration", (1+ntest-i), "\n") # display iteration number
}
test_result$`3-step ahead forecast` = test_boost_h3

# h=6: best model -- depth=5, tree size = 418
test_X_h6 = X_h6
y_test = Y
test_boost_h6 = matrix(0,ntest,1) #blank for CV criteria, d=5
for(i in ntest:1){#NB: backwards FOR loop: going from 150 down to 1
  X.window = test_X_h6[(1+ntest-i):(nrow(test_X_h6)-i),] #define the estimation window (first one: 1 to 432, then 2 to 433 etc, till 150 to 581.)
  y.window = y_test[(1+ntest-i):(length(y_test)-i)]
  boost = runboost_setn(X.window, y.window, n=418)
  test_boost_h6[(1+ntest-i), 1] = boost$pred > 0.5 # save the forecast
  cat("iteration", (1+ntest-i), "\n") # display iteration number
}
test_result$`6-step ahead forecast` = test_boost_h6

# save result
saveRDS(test_result, file = "data/boosting_gbm_prediction.RDS")

library(randomForest)

data = readRDS("data/final_cleaned_data_with_bull_bear.RDS")

data$DATE <- as.Date(data$DATE)
# Sort the data by date
data <- data[order(data$DATE), ]


ntest = 150
ncrossv = 100
ntrain = nrow(data) - ncrossv - ntest

data_X = data %>%
  select(-DATE) %>%
  select(-starts_with("lag")) %>%
  select(-ends_with("excess_CAPE_yield")) %>%
  select(-market_state, -"ten_year_annualized_stock_real_return", 
         -"ten_year_annualized_bonds_real_return", 
         -"real_10_year_excess_annualized_returns")


y = data$market_state

#Random Forest model
runrf=function(X,y,h){
  
  X=as.matrix(X)
  
  aux = embed(y, 12+h) #remove first 6 lags (2-sided), using 6 lags of Y
  y = as.factor(aux[,1])
  lags_y = aux[,-c(1:(6+h))]
  #print(lags_y)
  
  aux2 = embed(X, 12+h)
  lags_X = aux2[, c((ncol(X)*h+1):(ncol(X)*(6+h)))]
  
  predx = cbind(lags_y, lags_X)
  #print(sum(is.na(predx)))
  
  if(h==1){
    X.out=cbind(aux[,c(7:12)], aux2[, c(1:(ncol(X)*6))])
    X.out = tail(X.out,1)   #retrieve the last  observations if one-step forecast
  }else{
    X.out=cbind(aux[,c((6+h):(11+h))], aux2[, c((ncol(X)*(h-1)+1):(ncol(X)*(5+h)))])
    X.out = tail(X.out,1)  #last observations: y_T,y_t-1...y_t-h
  }
  #X.out = tail(X, 1)
  
  model=randomForest(predx,y,importance = TRUE) #fit the random forest on default settings
  pred=predict(model, X.out) #generate forecast
  
  return(list("model"=model,"pred"=pred)) #return the estimated model and h-step forecast
}

# na_columns <- sapply(data_X[2:594,], function(col) any(is.na(col)))
# columns_with_na <- names(data_X)[na_columns]
# print(columns_with_na)



rf.rolling.window=function(X,y,ntest, h){
  
  save.importance=list() #blank for saving variable importance
  save.pred=matrix(NA,ntest,1) ##blank for forecasts
  for(i in ntest:1){#NB: backwards FOR loop: going from 180 down to 1
    Y.window=y[(1+ntest-i):(length(y)-i)] #define the estimation window (first one: 1 to 491, then 2 to 492 etc.)
    X.window=X[(1+ntest-i):(nrow(X)-i),]
    rffit=runrf(X.window, Y.window, h)#call the function to fit the Random Forest and generate h-step forecast
    save.pred[(1+ntest-i),]=as.numeric(levels(rffit$pred)[rffit$pred])  #save the forecast
    save.importance[[i]]=importance(rffit$model) #save variable importance
    cat("iteration",(1+ntest-i),"\n") #display iteration number
  }
  #Some helpful stuff:
  real=tail(y, ntest)#get actual values
  # plot(real,type="l")
  # lines(c(rep(NA,length(real)-nprev),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  # 
  misclass = colSums(abs(real-save.pred))
  # rmse=sqrt(mean((tail(real,ntest)-save.pred)^2)) #compute RMSE
  # mae=mean(abs(tail(real,ntest)-save.pred)) #compute MAE (Mean Absolute Error)
  # errors=c("rmse"=rmse,"mae"=mae) #stack errors in a vector
  
  #return(list("pred"=save.pred,"errors"=errors,"save.importance"=save.importance)) #return forecasts, history of variable importance, and RMSE and MAE for the period.
  return(list("pred"=save.pred,"save.importance"=save.importance, "misclassification"=misclass)) #return forecasts, history of variable importance, and RMSE and MAE for the period.
  
}

test_result_rf <- data.frame(matrix(NA, nrow = 150, ncol = 3))
colnames(test_result_rf) <- c("1-step ahead forecast", "3-step ahead forecast", "6-step ahead forecast")


h1_rf = rf.rolling.window(data_X, data$market_state, ntest, 1)
test_result_rf$`1-step ahead forecast` = h1_rf$pred
h3_rf = rf.rolling.window(data_X, data$market_state, ntest, 3)
test_result_rf$`3-step ahead forecast` = h3_rf$pred
h6_rf = rf.rolling.window(data_X, data$market_state, ntest, 6)
test_result_rf$`6-step ahead forecast` = h6_rf$pred

test_result_rf$DATE = tail(data$DATE, 150)

saveRDS(test_result_rf, file = "data/randomforest_prediction.RDS")

#Bagging
runbagging=function(X,y,h){
  
  X=as.matrix(X)
  
  aux = embed(y, 12+h) #remove first 6 lags (2-sided), using 6 lags of Y
  y = as.factor(aux[,1])
  lags_y = aux[,-c(1:(6+h))]
  #print(lags_y)
  
  aux2 = embed(X, 12+h)
  lags_X = aux2[, c((ncol(X)*h+1):(ncol(X)*(6+h)))]
  
  predx = cbind(lags_y, lags_X)
  #print(sum(is.na(predx)))
  
  if(h==1){
    X.out=cbind(aux[,c(7:12)], aux2[, c(1:(ncol(X)*6))])
    X.out = tail(X.out,1)   #retrieve the last  observations if one-step forecast
  }else{
    X.out=cbind(aux[,c((6+h):(11+h))], aux2[, c((ncol(X)*(h-1)+1):(ncol(X)*(5+h)))])
    X.out = tail(X.out,1)  #last observations: y_T,y_t-1...y_t-h
  }
  #X.out = tail(X, 1)
  
  model=randomForest(predx,y,importance = TRUE, mtry = 318) #fit the random forest on default settings
  pred=predict(model, X.out) #generate forecast
  
  return(list("model"=model,"pred"=pred)) #return the estimated model and h-step forecast
}


bagging.rolling.window=function(X,y,ntest, h){
  
  save.importance=list() #blank for saving variable importance
  save.pred=matrix(NA,ntest,1) ##blank for forecasts
  for(i in ntest:1){#NB: backwards FOR loop: going from 150 down to 1
    Y.window=y[(1+ntest-i):(length(y)-i)] #define the estimation window (first one: 1 to 491, then 2 to 492 etc.)
    X.window=X[(1+ntest-i):(nrow(X)-i),]
    rffit=runbagging(X.window, Y.window, h)#call the function to fit the Random Forest and generate h-step forecast
    save.pred[(1+ntest-i),]=as.numeric(levels(rffit$pred)[rffit$pred])  #save the forecast
    save.importance[[i]]=importance(rffit$model) #save variable importance
    cat("iteration",(1+ntest-i),"\n") #display iteration number
  }
  #Some helpful stuff:
  real=tail(y, ntest)#get actual values
  # plot(real,type="l")
  # lines(c(rep(NA,length(real)-nprev),save.pred),col="red") #padded with NA for blanks, plot predictions vs. actual
  # 
  misclass = colSums(abs(real-save.pred))
  # rmse=sqrt(mean((tail(real,ntest)-save.pred)^2)) #compute RMSE
  # mae=mean(abs(tail(real,ntest)-save.pred)) #compute MAE (Mean Absolute Error)
  # errors=c("rmse"=rmse,"mae"=mae) #stack errors in a vector
  
  #return(list("pred"=save.pred,"errors"=errors,"save.importance"=save.importance)) #return forecasts, history of variable importance, and RMSE and MAE for the period.
  return(list("pred"=save.pred,"save.importance"=save.importance, "misclassification"=misclass)) #return forecasts, history of variable importance, and RMSE and MAE for the period.
  
}

test_result_bagging <- data.frame(matrix(NA, nrow = 150, ncol = 3))
colnames(test_result_bagging) <- c("1-step ahead forecast", "3-step ahead forecast", "6-step ahead forecast")


h1_bagging = bagging.rolling.window(data_X, data$market_state, ntest, 1)
test_result_bagging$`1-step ahead forecast` = h1_bagging$pred
h3_bagging = bagging.rolling.window(data_X, data$market_state, ntest, 3)
test_result_bagging$`3-step ahead forecast` = h3_bagging$pred
h6_bagging = bagging.rolling.window(data_X, data$market_state, ntest, 6)
test_result_bagging$`6-step ahead forecast` = h6_bagging$pred

test_result_bagging$DATE = tail(data$DATE, 150)

saveRDS(test_result_rf, file = "data/bagging_prediction.RDS")

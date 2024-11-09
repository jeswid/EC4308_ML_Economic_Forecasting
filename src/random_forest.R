library(dplyr)
library(ggplot2)
library(tibble)
library(randomForest)

data = readRDS("data/final_cleaned_data_with_bull_bear.RDS")

data$DATE <- as.Date(data$DATE)
# Sort the data by date
data <- data[order(data$DATE), ]
#Remove last 6 observations
#data <- head(data, -6)



ntest = 150
#ncrossv = 100
#ntrain = nrow(data) - ncrossv - ntest

data_X = data %>%
  select(-DATE) %>%
  select(-starts_with("lag")) %>%
  select(-market_state)

col_names_X = colnames(data_X)


num_lags <- 6

# Generate the lagged column names
lagged_colnames_X <- apply(expand.grid(1:num_lags, col_names_X), 1, function(x) {
  paste0("lag", x[1], "_", x[2])
})

# Arrange the result in the order
sorted_lagged_colnames_X <- lagged_colnames_X[order(sapply(lagged_colnames_X, 
                                                       function(x) {
                                                         substr(x, 1, 5)
                                                       }))]


y = data$market_state

#Random Forest model
runrf=function(X,y,h){
  
  X=as.matrix(X)
  
  aux = embed(y, 12+h) #remove first 6 lags (2-sided), using 6 lags of Y
  y = as.factor(aux[,1])
  lags_y = aux[,-c(1:(6+h))]
  colnames(lags_y) = paste0("lag", 6+seq(ncol(lags_y)), "_", "market_state")
  #print(lags_y)
  
  aux2 = embed(X, 12+h)
  lags_X = aux2[, c((ncol(X)*h+1):(ncol(X)*(6+h)))]
  colnames(lags_X) = sorted_lagged_colnames_X
  
  
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
  pred_prob=predict(model, X.out, type="prob") #generate forecast of probabilities
  
  return(list("model"=model,"pred"=pred, "pred_prob"=pred_prob)) #return the estimated model and h-step forecast
}

# na_columns <- sapply(data_X[2:594,], function(col) any(is.na(col)))
# columns_with_na <- names(data_X)[na_columns]
# print(columns_with_na)



rf.rolling.window=function(X,y,ntest, h){
  
  save.importance=list() #blank for saving variable importance
  save.pred=matrix(NA,ntest,1) ##blank for forecasts
  save.pred_prob=matrix(NA,ntest,1) ##blank for probability forecasts
  for(i in ntest:1){#NB: backwards FOR loop: going from 180 down to 1
    Y.window=y[(1+ntest-i):(length(y)-i)] #define the estimation window (first one: 1 to 491, then 2 to 492 etc.)
    X.window=X[(1+ntest-i):(nrow(X)-i),]
    rffit=runrf(X.window, Y.window, h)#call the function to fit the Random Forest and generate h-step forecast
    save.pred[(1+ntest-i),]=as.numeric(levels(rffit$pred)[rffit$pred])  #save the forecast
    #print(rffit$pred_prob[,2])
    save.pred_prob[(1+ntest-i),]=rffit$pred_prob[,2]  #save the forecast of prob y=1
    save.importance[[i]]=rownames_to_column(as.data.frame(importance(rffit$model)), var = "Feature") #save variable importance
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
  return(list("pred"=save.pred,"pred_prob"=save.pred_prob,"save.importance"=save.importance, "misclassification"=misclass)) #return forecasts, history of variable importance, and RMSE and MAE for the period.
  
}

test_result_rf <- data.frame(matrix(NA, nrow = 150, ncol = 6))
colnames(test_result_rf) <- c("1-step ahead forecast", "3-step ahead forecast", "6-step ahead forecast",
                              "1-step ahead predicted probability", "3-step ahead predicted probability", "6-step ahead predicted probability")


h1_rf = rf.rolling.window(data_X, data$market_state, ntest, 1)
test_result_rf$`1-step ahead forecast` = h1_rf$pred
test_result_rf$`1-step ahead predicted probability` = h1_rf$pred_prob
h3_rf = rf.rolling.window(data_X, data$market_state, ntest, 3)
test_result_rf$`3-step ahead forecast` = h3_rf$pred
test_result_rf$`3-step ahead predicted probability` = h3_rf$pred_prob
h6_rf = rf.rolling.window(data_X, data$market_state, ntest, 6)
test_result_rf$`6-step ahead forecast` = h6_rf$pred
test_result_rf$`6-step ahead predicted probability` = h6_rf$pred_prob

test_result_rf$DATE = tail(data$DATE, 150)

saveRDS(test_result_rf, file = "data/randomforest_prediction.RDS")



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


save.importance_rf_h1 = h1_rf$save.importance


mean_importance_rf_h1 = bind_rows(save.importance_rf_h1) %>% 
  group_by(`Feature`) %>%
  summarize(mean_importance = mean(`MeanDecreaseGini`, na.rm = TRUE)) %>%
  rename(var = Feature) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(average_importance = average_importance * 100 / sum(average_importance),
         horizon = 1)


save.importance_rf_h3 = h3_rf$save.importance

mean_importance_rf_h3 = bind_rows(save.importance_rf_h3) %>% 
  group_by(`Feature`) %>%
  summarize(mean_importance = mean(`MeanDecreaseGini`, na.rm = TRUE)) %>%
  rename(var = Feature) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(average_importance = average_importance * 100 / sum(average_importance),
         horizon = 3)


save.importance_rf_h6 = h6_rf$save.importance

mean_importance_rf_h6 = bind_rows(save.importance_rf_h6) %>% 
  group_by(`Feature`) %>%
  summarize(mean_importance = mean(`MeanDecreaseGini`, na.rm = TRUE)) %>%
  rename(var = Feature) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(average_importance = average_importance * 100 / sum(average_importance),
         horizon = 6)


df_plot_rf = rbind(mean_importance_rf_h1, mean_importance_rf_h3, mean_importance_rf_h6) %>%
  group_by(group, horizon)


saveRDS(df_plot_rf, file = "data/randomforest_importance.RDS")


ggplot(df_plot_rf, aes(x = factor(horizon), y = average_importance, fill = group)) + 
  geom_bar(stat = "identity", position = "stack") +  # Stack bars by horizon
  geom_text(aes(label = round(average_importance, 2)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Position text labels at center of each stack
  labs(title = "Random Forest Average Feature Importance By Type", x = "Forecast Horizon", y = "Average Importance") + 
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for horizons
  theme_minimal() + 
  theme(legend.position = "bottom",  # Move legend below plot
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5, face = "bold"))  # Center and bold title



#Bagging
runbagging=function(X,y,h){
  
  X=as.matrix(X)
  
  aux = embed(y, 12+h) #remove first 6 lags (2-sided), using 6 lags of Y
  y = as.factor(aux[,1])
  lags_y = aux[,-c(1:(6+h))]
  colnames(lags_y) = paste0("lag", 6+seq(ncol(lags_y)), "_", "market_state")
  #print(lags_y)
  
  aux2 = embed(X, 12+h)
  lags_X = aux2[, c((ncol(X)*h+1):(ncol(X)*(6+h)))]
  colnames(lags_X) = sorted_lagged_colnames_X
  
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
  
  model=randomForest(predx,y,importance = TRUE, mtry = 276) #fit the random forest on default settings
  pred=predict(model, X.out) #generate forecast
  pred_prob=predict(model, X.out, type = "prob") #generate forecasted probability
  
  return(list("model"=model,"pred"=pred,"pred_prob"=pred_prob)) #return the estimated model and h-step forecast
}


bagging.rolling.window=function(X,y,ntest, h){
  
  save.importance=list() #blank for saving variable importance
  save.pred=matrix(NA,ntest,1) ##blank for forecasts
  save.pred_prob=matrix(NA,ntest,1) ##blank for probability forecasts
  for(i in ntest:1){#NB: backwards FOR loop: going from 150 down to 1
    Y.window=y[(1+ntest-i):(length(y)-i)] #define the estimation window (first one: 1 to 491, then 2 to 492 etc.)
    X.window=X[(1+ntest-i):(nrow(X)-i),]
    rffit=runbagging(X.window, Y.window, h)#call the function to fit the Random Forest and generate h-step forecast
    save.pred[(1+ntest-i),]=as.numeric(levels(rffit$pred)[rffit$pred])  #save the forecast
    save.pred_prob[(1+ntest-i),]=rffit$pred_prob[,2]  #save the predicted probability of market stat = 1
    save.importance[[i]]=rownames_to_column(as.data.frame(importance(rffit$model)), var = "Feature") #save variable importance
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
  return(list("pred"=save.pred,"pred_prob"=save.pred_prob,"save.importance"=save.importance, "misclassification"=misclass)) #return forecasts, history of variable importance, and RMSE and MAE for the period.
  
}

test_result_bagging <- data.frame(matrix(NA, nrow = 150, ncol = 6))
colnames(test_result_bagging) <- c("1-step ahead forecast", "3-step ahead forecast", "6-step ahead forecast",
                                   "1-step ahead predicted probability", "3-step ahead predicted probability", "6-step ahead predicted probability")


h1_bagging = bagging.rolling.window(data_X, data$market_state, ntest, 1)
test_result_bagging$`1-step ahead forecast` = h1_bagging$pred
test_result_bagging$`1-step ahead predicted probability` = h1_bagging$pred_prob
h3_bagging = bagging.rolling.window(data_X, data$market_state, ntest, 3)
test_result_bagging$`3-step ahead forecast` = h3_bagging$pred
test_result_bagging$`3-step ahead predicted probability` = h3_bagging$pred_prob
h6_bagging = bagging.rolling.window(data_X, data$market_state, ntest, 6)
test_result_bagging$`6-step ahead forecast` = h6_bagging$pred
test_result_bagging$`6-step ahead predicted probability` = h6_bagging$pred_prob

test_result_bagging$DATE = tail(data$DATE, 150)

saveRDS(test_result_rf, file = "data/bagging_prediction.RDS")


# feature importance plot

save.importance_bagging_h1 = h1_bagging$save.importance


mean_importance_bagging_h1 = bind_rows(save.importance_bagging_h1) %>% 
  group_by(`Feature`) %>%
  summarize(mean_importance = mean(`MeanDecreaseGini`, na.rm = TRUE)) %>%
  rename(var = Feature) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(average_importance = average_importance * 100 / sum(average_importance),
         horizon = 1)


save.importance_bagging_h3 = h3_bagging$save.importance

mean_importance_bagging_h3 = bind_rows(save.importance_bagging_h3) %>% 
  group_by(`Feature`) %>%
  summarize(mean_importance = mean(`MeanDecreaseGini`, na.rm = TRUE)) %>%
  rename(var = Feature) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(average_importance = average_importance * 100 / sum(average_importance),
         horizon = 3)


save.importance_bagging_h6 = h6_bagging$save.importance

mean_importance_bagging_h6 = bind_rows(save.importance_bagging_h6) %>% 
  group_by(`Feature`) %>%
  summarize(mean_importance = mean(`MeanDecreaseGini`, na.rm = TRUE)) %>%
  rename(var = Feature) %>%
  left_join(variable_lookup, by = "var") %>%  # Join by variable name
  group_by(group) %>%                          # Group by the variable group
  summarize(average_importance = sum(mean_importance)) %>%
  arrange(desc(average_importance)) %>%
  mutate(average_importance = average_importance * 100 / sum(average_importance),
         horizon = 6)


df_plot_bagging = rbind(mean_importance_bagging_h1, mean_importance_bagging_h3, mean_importance_bagging_h6) %>%
  group_by(group, horizon)


saveRDS(df_plot_bagging, file = "data/bagging_importance.RDS")


ggplot(df_plot_bagging, aes(x = factor(horizon), y = average_importance, fill = group)) + 
  geom_bar(stat = "identity", position = "stack") +  # Stack bars by horizon
  geom_text(aes(label = round(average_importance, 2)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Position text labels at center of each stack
  labs(title = "Bagging Average Feature Importance By Type", x = "Forecast Horizon", y = "Average Importance") + 
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for horizons
  theme_minimal() + 
  theme(legend.position = "bottom",  # Move legend below plot
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5, face = "bold"))  # Center and bold title



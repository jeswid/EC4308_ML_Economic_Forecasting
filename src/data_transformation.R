# remove all objects from memory
rm(list = ls())

# load required packages
library(tseries)  # for ADF test

# load dataset
df = readRDS("data/complete_data_df.RDS")

# data transformation for ratio variables
# dividend price ratio
adf.test(df$dividend_price_ratio, alternative = "stationary") # non-stationary
# log then first difference
diff_log_series <- diff(log(df$dividend_price_ratio))
adf.test(diff_log_series) # stationary
df$dividend_price_ratio <- c(NA, diff(log(df$dividend_price_ratio)))

# SP price
adf.test(df$price, alternative = "stationary") # non-stationary
diff_log_series <- diff(log(df$price))
adf.test(diff_log_series) # stationary
df$price <- c(NA, diff(log(df$price)))

# earnings price ratio
adf.test(df$earnings_price_ratio, alternative = "stationary") # stationary

# dividend payout
adf.test(df$dividend_payout, alternative = "stationary") # stationary

# book market b/m
adf.test(df$book_market, alternative = "stationary") # not stationary
diff_log_series <- diff(log(df$book_market))
adf.test(diff_log_series) # stationary
df$book_market <- c(NA, diff(log(df$book_market)))

# b/m x-sect factor
adf.test(df$fbm, alternative = "stationary") # stationary

#CAPE - not stationary
adf.test(df$CAPE, alternative = "stationary")
diff_log_series <- diff(log(df$CAPE))
adf.test(diff_log_series) # stationary
df$CAPE <- c(NA, diff(log(df$CAPE)))

adf.test(df$TR_CAPE, alternative = "stationary")
diff_log_series <- diff(log(df$TR_CAPE))
adf.test(diff_log_series) # stationary
df$TR_CAPE <- c(NA, diff(log(df$TR_CAPE)))

adf.test(df$excess_CAPE_yield, alternative = 'stationary')
diff_log_series <- diff(log(df$excess_CAPE_yield))
df$excess_CAPE_yield <- c(NA, diff(log(df$excess_CAPE_yield)))

#data transformation for index variables that only requires first differencing
index_vars_firstdiff <- c("ret", "retx", "pce", "cay", "svar", "impvar", "vrp", "tchi", "rsvix", 
                "AAA", "BAA", "dividend_yield")

for (x in index_vars_firstdiff) {
  # Check if the column has already been differenced by ensuring only one leading NA
  if (sum(is.na(df[[x]])) <= 1) {
    df[[x]] <- c(NA, diff(df[[x]]))
  }
}

# update data
saveRDS(df, file = "data/complete_data_df_transformed.RDS")



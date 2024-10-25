# data transformation for ratio variables

df = readRDS("data/complete_data_df.RDS")

# install.packages('tseries')
library(tseries)  # for ADF test

# dividend price ratio
adf.test(df$dividend_price_ratio, alternative = "stationary") # non-stationary
# log then first difference
diff_log_series <- diff(log(df$dividend_price_ratio))
adf.test(diff_log_series) # stationary
df$dividend_price_ratio <- c(NA, diff(log(df$dividend_price_ratio)))


# dividend yield
adf.test(df$dividend_yield, alternative = "stationary") # non-stationary
diff_log_series <- diff(log(df$dividend_yield))
adf.test(diff_log_series) # stationary
df$dividend_yield <- c(NA, diff(log(df$dividend_yield)))


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

# update data
saveRDS(df, file = "data/complete_data_df.RDS")



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


adf.test(df$tbl, alternative = 'stationary') # not stationary
adf.test(diff(df$tbl) )

adf.test(df$Rfree, alternative = 'stationary') # not stationary
adf.test(diff(df$Rfree))

adf.test(df$lty, alternative = 'stationary') # not stationary
adf.test(diff(df$lty))

adf.test(df$ltr, alternative = 'stationary') # stationary
adf.test(df$corpr, alternative = 'stationary') # stationary
adf.test(df$tms, alternative = 'stationary') # stationary
adf.test(df$dfy, alternative = 'stationary') # stationary
adf.test(df$dfr, alternative = 'stationary') # stationary
adf.test(df$infl, alternative = 'stationary') # stationary
adf.test(df$ntis, alternative = 'stationary') # stationary
adf.test(df$lzrt, alternative = 'stationary') # stationary
adf.test(df$wtexas, alternative = 'stationary') # stationary
adf.test(df$sntm, alternative = 'stationary') # stationary
adf.test(df$ndrbl, alternative = 'stationary') # stationary
adf.test(df$skvw, alternative = 'stationary') # stationary
adf.test(df$tail, alternative = 'stationary') # stationary
adf.test(df$dtoy, alternative = 'stationary') # stationary
adf.test(df$dtoat, alternative = 'stationary') # stationary
adf.test(df$ygap, alternative = 'stationary') # stationary
adf.test(df$avgcor, alternative = 'stationary') # stationary
adf.test(df$monthly_total_bond_returns, alternative = 'stationary') # stationary

adf.test(df$ogap, alternative = 'stationary') # not stationary
adf.test(diff(df$ogap))

adf.test(df$sntm, alternative = 'stationary') # not stationary
adf.test(diff(df$sntm))

adf.test(df$shtint, alternative = 'stationary') # stationary
adf.test(diff(df$shtint))

adf.test(df$price, alternative = 'stationary') # stationary
adf.test(diff(df$price))

adf.test(df$dividend, alternative = 'stationary') # stationary
adf.test(diff(df$dividend))

adf.test(df$earnings, alternative = 'stationary') # stationary
adf.test((diff(df$earnings)))

adf.test(df$CPI, alternative = 'stationary') # stationary
adf.test(diff(df$CPI))

adf.test(df$rate_gs10, alternative = 'stationary') # stationary
adf.test(diff(df$rate_gs10))

adf.test(df$UNRATE, alternative = 'stationary') # stationary
adf.test(diff(df$UNRATE))

adf.test(df$DFF, alternative = 'stationary') # stationary
adf.test(diff(df$DFF))

adf.test(df$INDPRO, alternative = 'stationary') # stationary
adf.test(diff(df$INDPRO))

#data transformation for index variables that only requires first differencing
index_vars_firstdiff <- c("ret", "retx", "svar", "tchi", "AAA", "BAA", "dividend_yield", 
                          "tbl", "Rfree", "lty", "ogap", "sntm", "shtint", "price", "dividend", "earnings",
                          "CPI", "rate_gs10", "UNRATE", "DFF", "INDPRO")

for (x in index_vars_firstdiff) {
  df[[x]] <- c(NA, diff(df[[x]]))
}

# update data
saveRDS(df, file = "data/complete_data_df_transformed.RDS")

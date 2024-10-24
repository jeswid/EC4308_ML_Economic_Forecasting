# remove all objects from memory
rm(list = ls())

# load required packages
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(missMDA)
library(RANN)
library(hdm)
library(ggplot2)
library(writexl)
library(zoo)

# load datasets
df_stock_price = read_excel("data/ie_data.xls",sheet = 'Data',skip = 7)
df_stock_returns_monthly = read_excel("data/Data2023.xlsx",sheet = 'Monthly',col_names = TRUE, guess_max = 10000)
df_unemployment_rate = read.csv("data/UNRATE.csv")
df_federal_funds_rate = read.csv("data/DFF.csv")
df_industrial_production_growth_rate = read.csv("data/INDPRO.csv")

# remove NA values
df_stock_price = df_stock_price[!is.na(df_stock_price$Date), ]
# convert df_stock_price$Date to YYYY-MM-DD format
df_stock_price$Date <- sapply(df_stock_price$Date, function(x) {
  year <- floor(x)  # extract the year part
  month <- round((x - year) * 100)  # extract the month part
  date_string <- paste0(year, "-", sprintf("%02d", month), "-01")  # format as YYYY-MM-DD
  as.Date(date_string)  # convert to date type
})
# convert sapply result back to a date column in data frame
df_stock_price$Date <- as.Date(df_stock_price$Date)

df_stock_price =  df_stock_price %>% # rename df_stock_price columns
  rename(
    DATE = Date,
    price = P,
    dividend = D,
    earnings = E,
    rate_gs10 = `Rate GS10`,
    real_price = Price...8,
    real_dividend = Dividend,
    real_total_return_price = Price...10,
    real_earnings = Earnings...11,
    real_tr_scaled_earnings = Earnings...12,
    TR_CAPE = `TR CAPE`,
    excess_CAPE_yield = Yield,
    monthly_total_bond_returns = Returns...18,
    real_total_bond_returns = Returns...19,
    ten_year_annualized_stock_real_return = `Real Return...20`,
    ten_year_annualized_bonds_real_return = `Real Return...21`,
    real_10_year_excess_annualized_returns = Returns...22
  ) %>%
  select (-c(Fraction,...14,...16)) %>% # drop unwanted columns
  filter(CAPE != "NA") %>% # filter out NA values for CAPE
  mutate(CAPE = as.numeric(CAPE)) %>% # change CAPE to numeric
  mutate(TR_CAPE = as.numeric(TR_CAPE)) # change TR_CAPE to numeric

# convert df_stock_returns_monthly$yyyymm to YYYY-MM-DD format
df_stock_returns_monthly$yyyymm <- sapply(df_stock_returns_monthly$yyyymm, function(x) {
  x <- as.character(x)  # convert to character to manipulate string
  year <- substr(x, 1, 4)  # extract the first 4 characters as year
  month <- substr(x, 5, 6)  # extract the last 2 characters as month
  date_string <- paste0(year, "-", month, "-01")  # combine into YYYY-MM-DD format
  as.Date(date_string)  # convert to date type
})
# convert sapply result back to a date column in data frame
df_stock_returns_monthly$yyyymm <- as.Date(df_stock_returns_monthly$yyyymm)

df_stock_returns_monthly =  df_stock_returns_monthly %>% # rename df_stock_returns_monthly columns
  rename(
    DATE = yyyymm,
    dividend_price_ratio = `d/p`,
    dividend_yield = `d/y`,
    earnings_price_ratio = `e/p`,
    dividend_payout = `d/e`,
    book_market = `b/m`
  ) %>%
  select (-c(price,d12,e12)) %>% # drop duplicated columns
  select (-c(cay,`i/k`,pce,govik,crdstd)) %>% # drop quarterly columns
  select (-c(skew)) %>% # drop semiannual columns
  select(-c(eqis,accrul,cfacc,gpce,gip,house)) %>% # drop yearly columns
  select(-c(csp)) %>% # drop columns where the end year is less than 2002
  select(-c(vp,vrp,impvar,rsvix,disag)) %>% #drop columns where the start year is after 1980
  drop_na()

# convert to date type
df_unemployment_rate$DATE <- as.Date(df_unemployment_rate$DATE)
df_federal_funds_rate$DATE <- as.Date(df_federal_funds_rate$DATE)
df_industrial_production_growth_rate$DATE <- as.Date(df_industrial_production_growth_rate$DATE)

# join datasets
df = left_join(df_stock_returns_monthly, df_stock_price, by = "DATE") %>%
  left_join(.,df_unemployment_rate, by='DATE') %>%
  left_join(.,df_federal_funds_rate, by='DATE') %>%
  left_join(.,df_industrial_production_growth_rate, by='DATE')

# save df as an RDS file
saveRDS(df, file = "data/complete_data_df.RDS")
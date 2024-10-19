# remove all objects from memory
rm(list = ls())

# load required packages
library(readxl)
library(dplyr)
library(mice)
library(caret)
library(RANN)
library(hdm)

# load datasets
df_stock_price = read_excel("data/ie_data.xls",sheet = 'Data',skip = 7)
df_stock_returns = read_excel("data/Data2023.xlsx",sheet = 'Monthly')
df_unemployment_rate = read.csv("data/UNRATE.csv")
df_term_spread = read.csv("data/T10Y3M.csv")
df_federal_funds_rate = read.csv("data/DFF.csv")
df_3_month_tbill_rate = read.csv("data/DTB3.csv")
df_10_year_bond_rate = read.csv("data/IRLTLT01USM156N.csv")
df_inflation_rate = read.csv("data/T10YIE.csv")
df_industrial_production_growth_rate = read.csv("data/INDPRO.csv")

# clean datasets
# remove NA values from df_stock_price$Date
df_stock_price <- df_stock_price[!is.na(df_stock_price$Date), ]
# convert each value in df_stock_price$Date to YYYY-MM-DD format
df_stock_price$Date <- sapply(df_stock_price$Date, function(x) {
  year <- floor(x)  # Extract the year part
  month <- round((x - year) * 100)  # Extract the month part
  date_string <- paste0(year, "-", sprintf("%02d", month), "-01")  # Format as YYYY-MM-DD
  as.Date(date_string)  # Convert to Date
})
# convert sapply result back to a Date column in data frame
df_stock_price$Date <- as.Date(df_stock_price$Date)
# rename df_stock_price columns
df_stock_price =  df_stock_price %>% 
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
  select (-c(Fraction,...14,...16))

# convert each value in df_stock_returns$yyyymm to YYYY-MM-DD format
df_stock_returns$yyyymm <- sapply(df_stock_returns$yyyymm, function(x) {
  x <- as.character(x)  # Convert to character to manipulate string
  year <- substr(x, 1, 4)  # Extract the first 4 characters as year
  month <- substr(x, 5, 6)  # Extract the last 2 characters as month
  date_string <- paste0(year, "-", month, "-01")  # Combine into YYYY-MM-DD format
  as.Date(date_string)  # Convert to Date
})
# convert sapply result back to a Date column in data frame
df_stock_returns$yyyymm <- as.Date(df_stock_returns$yyyymm)
# rename df_stock_returns columns
df_stock_returns =  df_stock_returns %>% 
  rename(
    DATE = yyyymm
  ) %>%
  select (-c(price,d12,e12))

# convert to date type
df_unemployment_rate$DATE <- as.Date(df_unemployment_rate$DATE)
df_term_spread$DATE <- as.Date(df_term_spread$DATE)
df_federal_funds_rate$DATE <- as.Date(df_federal_funds_rate$DATE)
df_3_month_tbill_rate$DATE <- as.Date(df_3_month_tbill_rate$DATE)
df_10_year_bond_rate$DATE <- as.Date(df_10_year_bond_rate$DATE)
df_inflation_rate$DATE <- as.Date(df_inflation_rate$DATE)
df_industrial_production_growth_rate$DATE <- as.Date(df_industrial_production_growth_rate$DATE)

# join datasets
df = left_join(df_stock_price,df_stock_returns, by = "DATE") %>%
  left_join(.,df_unemployment_rate, by='DATE') %>%
  left_join(.,df_term_spread, by='DATE') %>%
  left_join(.,df_federal_funds_rate, by='DATE') %>%
  left_join(.,df_3_month_tbill_rate, by='DATE') %>%
  left_join(.,df_10_year_bond_rate, by='DATE') %>%
  left_join(.,df_inflation_rate, by='DATE') %>%
  left_join(.,df_industrial_production_growth_rate, by='DATE')

# impute missing values

# logistic regression
logit_model <- glm(am ~ mpg + hp + wt, data = mtcars, family = binomial(link = "logit"))
# display summary of the model
summary(logit_model)

# logistic LASSO regression

# 
# remove all objects from memory
rm(list = ls())

# load required packages
library(readxl)
library(dplyr)
library(missMDA)
library(RANN)
library(hdm)
library(ggplot2)

# load datasets
df_stock_price = read_excel("data/ie_data.xls",sheet = 'Data',skip = 7)
df_stock_returns_monthly = read_excel("data/Data2023.xlsx",sheet = 'Monthly')
df_stock_returns_quarterly = read_excel("data/Data2023.xlsx",sheet = 'Quarterly')
df_stock_returns_yearly = read_excel("data/Data2023.xlsx",sheet = 'Annual')
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

# convert each value in df_stock_returns_monthly$yyyymm to YYYY-MM-DD format
df_stock_returns_monthly$yyyymm <- sapply(df_stock_returns_monthly$yyyymm, function(x) {
  x <- as.character(x)  # Convert to character to manipulate string
  year <- substr(x, 1, 4)  # Extract the first 4 characters as year
  month <- substr(x, 5, 6)  # Extract the last 2 characters as month
  date_string <- paste0(year, "-", month, "-01")  # Combine into YYYY-MM-DD format
  as.Date(date_string)  # Convert to Date
})

# convert sapply result back to a Date column in data frame
df_stock_returns_monthly$yyyymm <- as.Date(df_stock_returns_monthly$yyyymm)
# rename df_stock_returns_monthly columns
df_stock_returns_monthly =  df_stock_returns_monthly %>% 
  rename(
    DATE = yyyymm,
    dividend_price_ratio = `d/p`,
    dividend_yield = `d/y`,
    earnings_price_ratio = `e/p`,
    dividend_payout = `d/e`,
    book_market = `b/m`
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
df = left_join(df_stock_price,df_stock_returns_monthly, by = "DATE") %>%
  left_join(.,df_unemployment_rate, by='DATE') %>%
  left_join(.,df_term_spread, by='DATE') %>%
  left_join(.,df_federal_funds_rate, by='DATE') %>%
  left_join(.,df_3_month_tbill_rate, by='DATE') %>%
  left_join(.,df_10_year_bond_rate, by='DATE') %>%
  left_join(.,df_inflation_rate, by='DATE') %>%
  left_join(.,df_industrial_production_growth_rate, by='DATE')

df_date <- df %>% select(DATE)

# standardize the data
df_temp = na.omit(df_stock_price)
# Convert factor or character columns to numeric (if appropriate)
# df_temp <- df_temp %>% mutate_if(is.factor, as.numeric) %>% mutate_if(is.character, as.numeric)
# Select only numeric columns
numeric_df <- df_temp[, sapply(df_temp, is.numeric)]  # Remove the non-numeric column

# principal component analysis 
pca_result <- prcomp(numeric_df, center = TRUE, scale. = TRUE)
# Check the PCA result
summary(pca_result)  
# Get the proportion of variance

#plot the screeplot
results = pca_result
var_explained = results$sdev^2 / sum(results$sdev^2)
#choose 4 PCs

#create scree plot
qplot(c(1:16), var_explained) + 
  geom_line() +   xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

# impute missing values
# Check the data types of columns
# sapply(df, class)
# Convert factor or character columns to numeric (if appropriate)
df <- df %>% mutate_if(is.factor, as.numeric) %>% mutate_if(is.character, as.numeric)
# Select only numeric columns
numeric_df <- df[, sapply(df, is.numeric)]
# Perform PCA imputation
pca_imputed_data <- imputePCA(numeric_df, ncp = 4)  # ncp = number of principal components
# Get the completed dataset
complete_data <- pca_imputed_data$completeObs
complete_data_df = as.data.frame(complete_data)

#join the date column
complete_data_df$DATE = df_date

print(complete_data_df$DATE)

write.csv(complete_data_df, file = "data/complete_data_pca.csv")

#################### in case u wanna impute using quarterly / yearly data ###############################
# join stock_returns monthly, quarterly, yearly datasets
# Convert Quarter and Month to Date for easier manipulation
#df_stock_returns_quarterly$yyyyq <- sapply(df_stock_returns_quarterly$yyyyq,function(x) {
#  year <- floor(x / 10)  # Extract the year part
#  quarter <- x %% 10  # Extract the quarter part
# Determine the starting month of the quarter
#  month <- (quarter - 1) * 3 + 1 # 1 for Q1, 4 for Q2, 7 for Q3, 10 for Q4
# date_string <- sprintf("%04d-%02d-01", year, month)  # Set day as the 1st of the month
#  as.Date(date_string)  # Convert to Date
#})
# convert sapply result back to a Date column in data frame
#df_stock_returns_quarterly$yyyyq <- as.Date(df_stock_returns_quarterly$yyyyq)
# rename df_stock_returns_quarterly columns
#df_stock_returns_quarterly =  df_stock_returns_quarterly %>% 
#  rename(
#    DATE = yyyyq,
#    dividend_price_ratio = `d/p`,
#   dividend_yield = `d/y`,
#   earnings_price_ratio = `e/p`,
#   dividend_payout = `d/e`,
#   book_market = `b/m`
#  ) %>%
#  select (-c(price,d12,e12))
# impute monthly data with quarterly data
#monthly_data_imputed <- df_stock_returns_monthly %>%
# left_join(df_stock_returns_quarterly, by = "DATE", suffix = c("", ".quarterly")) %>%  # Join on Date
# mutate(cay = ifelse(is.na(cay), cay.quarterly, cay)) %>%                 # Impute missing values
#select(DATE, cay)  # Select only the relevant columns
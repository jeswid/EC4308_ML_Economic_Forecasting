# remove all objects from memory
rm(list = ls())

# load required packages
library(readxl)
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

# join datasets


# logistic regression
logit_model <- glm(am ~ mpg + hp + wt, data = mtcars, family = binomial(link = "logit"))
# Display summary of the model
summary(logit_model)

# logistic LASSO regression

# 
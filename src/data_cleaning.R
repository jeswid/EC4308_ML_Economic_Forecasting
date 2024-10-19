#remove all objects from memory
rm(list = ls())

# load required packages
library(readxl)

# load datasets
df = read_excel("data/ie_data.xls",sheet = 'Data',skip = 7)
df_stock_returns = read_excel("data/Data2023.xlsx",sheet = 'Monthly')

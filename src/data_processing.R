# remove all objects from memory
rm(list = ls())

# load required packages
library(TTR)
library(dplyr)
library(zoo)
library(tidyverse)
library(bbdetection)

# load dataset
df <- readRDS("data/complete_data_df_transformed.RDS")

# our prediction horizon is t = 1, 3, 6 months
h1 <- 1
h2 <- 3
h3 <- 6

df <- df %>%
  arrange(DATE)

# create lagged variables up to lag 12 for all columns except DATE
cols_to_lag <- names(df)[names(df) != "DATE"]
df <- df %>%
  mutate(across(all_of(cols_to_lag), 
                list(lag1 = ~lag(., 1), lag2 = ~lag(., 2), lag3 = ~lag(., 3), 
                     lag4 = ~lag(., 4), lag5 = ~lag(., 5), lag6 = ~lag(., 6),
                      lag7 = ~lag(., 7)),
                .names = "{fn}_{col}"))

# assuming 'price' column contains the price data
prices <- df$price

# convert dates if necessary
dates <- as.Date(df$DATE)

# set parameters for the dating algorithm
# these are typical parameters, but you may need to adjust based on your data
setpar_dating_alg(t_window = 6, t_censor = 6, t_phase = 4, t_cycle = 15, max_chng = 20)

# run the dating algorithm to identify bull and bear market states
bull_states <- run_dating_alg(prices)

# print out the dating of bull-bear states
bb.dating.states(prices, bull_states, dates)

# add the bull-bear market states to the dataframe
df$market_state <- ifelse(bull_states, "Bull", "Bear")

# convert "Bull" to 1 and "Bear" to 0
df$market_state <- ifelse(df$market_state == "Bull", 1, 0)

df <- df %>%
  mutate(across(c("market_state"), list(lag1 = ~lag(., 1), lag2 = ~lag(., 2), lag3 = ~lag(., 3), 
                                        lag4 = ~lag(., 4), lag5 = ~lag(., 5), lag6 = ~lag(., 6),
                                        lag7 = ~lag(., 7), 
                                 lag8 = ~lag(., 8), 
                                 lag9 = ~lag(., 9), 
                                 lag10 = ~lag(., 10), 
                                 lag11 = ~lag(., 11), 
                                 lag12 = ~lag(., 12)),
                .names = "lag{.fn}_{.col}"))

# save the final cleaned data with bull-bear market states
saveRDS(df, "data/final_cleaned_data_with_bull_bear.RDS")
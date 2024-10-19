library(tidyverse)
load("data/complete_data_df.RData")

df = complete_data_df

#our prediction horizon is t = 1, 3, 6 months
h1 = 1
h2 = 3
h3 = 6

#first need to create lags for all the variables used (except for DATE)
# Create lagged variables for all columns except Date
df <- df %>%  mutate(across(-DATE, ~ lag(.x, n = h1), .names = "lag_{col}"))
df <- df %>%  mutate(across(-DATE, ~ lag(.x, n = h2), .names = "la_{col}"))
df <- df %>%  mutate(across(-DATE, ~ lag(.x, n = h3), .names = "la_{col}"))

#now generate bull and bear variables based on Bry-Bosch algorithm in the Nyberg 2013 paper

# Load necessary libraries
library(TTR)
library(dplyr)
library(zoo)

# Assuming df is your dataframe with stock prices under the 'prices' column
prices <- df$price


# 1. Calculate log-returns (optional for further analysis)
log_returns <- diff(log(prices))

# 2. Define functions to find peaks and troughs
find_peaks <- function(x) {
  which(diff(sign(diff(x))) == -2) + 1  # Local maxima (peaks)
}

find_troughs <- function(x) {
  which(diff(sign(diff(x))) == 2) + 1  # Local minima (troughs)
}

# 3. Identify peaks and troughs
peaks <- find_peaks(prices)
troughs <- find_troughs(prices)

# 4. Ensure proper alignment of peaks and troughs
if (length(troughs) > 0 && troughs[1] < peaks[1]) {
  troughs <- troughs[-1]  # Remove the first unmatched trough
}
min_length <- min(length(peaks), length(troughs))

# Truncate peaks and troughs to match lengths
peaks <- peaks[1:min_length]
troughs <- troughs[1:min_length]

# 5. Initialize market state (0 = Bull, 1 = Bear)
market_state <- rep(NA, length(prices))

# 6. Assign bull and bear states safely
for (i in seq_along(peaks)) {
  if (peaks[i] < troughs[i]) {
    market_state[peaks[i]:troughs[i]] <- 1  # Bear market
  }
  if (i < length(peaks) && troughs[i] < peaks[i + 1]) {
    market_state[troughs[i]:peaks[i + 1]] <- 0  # Bull market
  }
}

# 7. Handle NA values and ensure rolling window size matches
bull_bear <- rollapply(market_state, width = 6, 
                       FUN = function(x) ifelse(mean(x, na.rm = TRUE) > 0.5, 1, 0), 
                       fill = NA, align = "right")

# Ensure bull_bear matches the original dataframe length
bull_bear <- c(rep(NA, length(prices) - length(bull_bear)), bull_bear)

# 8. Add the computed market state to the original dataframe
df$market_state <- bull_bear


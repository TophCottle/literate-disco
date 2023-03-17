# Load libraries and API --------------------------------------------------
rm(list = ls())
library(tidyverse)
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(blastula)

symbols <- read.csv("/Users/tophcottle/Documents/Personal/mfx_products/data/currency_symbols.csv")
setwd("/Users/tophcottle/Documents/Personal/mfx_products/data/")
today_date <- Sys.Date()

symbol_string <- paste(symbols$code, collapse = ",")
symbol_string_encoded <- URLencode(symbol_string)
usdurl <- paste0('https://api.exchangerate.host/timeseries?start_date=2023-01-01&end_date=',today_date,"&base=USD&amount=1&source=ecb&symbols=", symbol_string_encoded)


# Download data -----------------------------------------------------------
usddata <- as.data.frame(fromJSON(usdurl)) 

usddata <- usddata %>%
  select(-motd.msg, -motd.url, -success, -timeseries, -start_date, -end_date, -base)

data_long <- usddata %>%
  pivot_longer(cols = starts_with("rates."), 
               names_to = c("prefix", "year", "month", "day", "symbol"), 
               names_pattern = "^(\\w+)\\.(\\d{4})\\.(\\d{2})\\.(\\d{2})\\.(\\w+)$", 
               values_to = "value") %>%
  select(-prefix) %>%
  mutate(date = paste(year, month, day, sep = "-")) %>%
  select(-year, -month, -day) %>%
  pivot_wider(names_from = "symbol", values_from = "value")

for (col in colnames(data_long)) {
  new_col <- paste0("USD_", col)
  colnames(data_long)[colnames(data_long) == col] <- new_col
}

eururl <- paste0('https://api.exchangerate.host/timeseries?start_date=2023-01-01&end_date=',today_date,"&base=EUR&amount=1&source=ecb&symbols=", symbol_string_encoded)

eurdata <- as.data.frame(fromJSON(eururl)) 

eurdata <- eurdata %>%
  select(-motd.msg, -motd.url, -success, -timeseries, -start_date, -end_date, -base)

data_long_eur <- eurdata %>%
  pivot_longer(cols = starts_with("rates."), 
               names_to = c("prefix", "year", "month", "day", "symbol"), 
               names_pattern = "^(\\w+)\\.(\\d{4})\\.(\\d{2})\\.(\\d{2})\\.(\\w+)$", 
               values_to = "value") %>%
  select(-prefix) %>%
  mutate(date = paste(year, month, day, sep = "-")) %>%
  select(-year, -month, -day) %>%
  pivot_wider(names_from = "symbol", values_from = "value")

for (col in colnames(data_long_eur)) {
  new_col <- paste0("EUR_", col)
  colnames(data_long_eur)[colnames(data_long_eur) == col] <- new_col
}


# Combine data and save ---------------------------------------------------
histdata <- bind_cols(data_long, data_long_eur) %>%
  select(-USD_USD, -EUR_EUR, -EUR_date) %>%
  rename(date = USD_date)

write.csv(histdata, file = "fx_2023.csv", row.names = FALSE)

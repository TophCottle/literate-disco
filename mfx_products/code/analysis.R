# Load libraries and API --------------------------------------------------
rm(list = ls())
library(tidyverse)
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(xts)
library(forecast)
# Upload data -------------------------------------------------------------
today_date <- Sys.Date()
symbols <- read.csv("currency_symbols.csv")
rates <- read.csv("/Users/tophcottle/Documents/Personal/mfx_products/data/fx_2023.csv")%>%
  select(-USD_HRK, -EUR_HRK)%>%
  map_df(.,rev)
rates_2 <- read.csv("/Users/tophcottle/Documents/Personal/mfx_products/data/fx_2022.csv")%>%
  select(-USD_RUB, -EUR_RUB, -USD_HRK, -EUR_HRK)%>%
  map_df(.,rev)
rates <- as.data.frame(rbind(rates, rates_2))%>%
  map_df(.,rev)
usd_cols <- rates %>% select("date", matches("USD"))
eur_cols <- rates %>% select("date",matches("EUR"))

forex_cols <- names(rates)[-1]
 #Loop through each forex rate column and flip the rates
for (col_name in forex_cols) {
  # Extract the column data
  col_data <- rates[, col_name]
  # Flip the rates
  flipped_data <- 1/col_data
  # Replace the original column with the flipped column
  rates[, col_name] <- flipped_data
}


# Calculate changes in FX rates -------------------------------------------
# create a new data frame for changes
changed <- data.frame(matrix(nrow=6, ncol=60))
colnames(changed) <- colnames(rates[,-1])
calc_change<- rates%>%
  select(-date)

# calculate percent change for each column and assign it to the row of the changed data frame
for (i in 1:ncol(calc_change)) {
  changed[1,i] <- ((calc_change[nrow(calc_change), i] - calc_change[nrow(calc_change)-1, i]) /
                     as.numeric(calc_change[nrow(calc_change)-1, i])) * 100
  changed[2,i] <- ((calc_change[nrow(calc_change), i] - calc_change[nrow(calc_change)-5, i]) /
                     as.numeric(calc_change[nrow(calc_change)-5, i])) * 100
  changed[3,i] <- ((calc_change[nrow(calc_change), i] - calc_change[nrow(calc_change)-21, i]) /
                     as.numeric(calc_change[nrow(calc_change)-21, i])) * 100
  changed[4,i] <- ((calc_change[nrow(calc_change), i] - calc_change[nrow(calc_change)-63, i]) /
                     as.numeric(calc_change[nrow(calc_change)-63, i])) * 100
  changed[5,i] <- ((calc_change[nrow(calc_change), i] - calc_change[nrow(calc_change)-126, i]) /
                     as.numeric(calc_change[nrow(calc_change)-126, i])) * 100
  changed[6,i] <- ((calc_change[nrow(calc_change), i] - calc_change[nrow(calc_change)-260, i]) /
                     as.numeric(calc_change[nrow(calc_change)-260, i])) * 100
}

change <- c("one_day_change", "one_week_change", "one_month_change", "three_month_change", "six_month_change", "one_year_change")
changed <- cbind(change, changed)

# Create an index ---------------------------------------------------------
# initialize fx_index as a data frame
fx_index <- data.frame(matrix(NA, nrow = nrow(rates), ncol = ncol(rates)))
# copy the date column from rates
fx_index[, 1] <- rates[, 1]
# set the first index value to 100 for each currency column
fx_index[, -1] <- 100

# loop through the fx rates for each currency and calculate the index values
for (col in 2:ncol(rates)) {
  for (i in 2:nrow(rates)) {
    # calculate the percentage change from the previous observation
    pct_change <- (rates[i, col] - rates[i-1, col]) / rates[i-1, col]
    # multiply the previous index value by (1 + the percentage change)
    fx_index[i, col] <- fx_index[i-1, col] * (1 + pct_change)
  }
}
# set the column names for the fx_index data frame
col_names <- names(rates)
names(fx_index) <- col_names

# ARIMA models ------------------------------------------------------------
# create empty data frames to store the results
acf_results <- data.frame(matrix(ncol = ncol(rates)-1, nrow = 0))
pacf_results <- data.frame(matrix(ncol = ncol(rates)-1, nrow = 0))

# loop through each column of the rates data frame
for (i in names(rates)[-1]) {
  acf_res <- acf(rates[[i]], na.action = na.pass, main = paste("ACF for", i))
  pacf_res <- pacf(rates[[i]], na.action = na.pass, main = paste("PACF for", i))
  
  # add the results to the data frames
  acf_results <- rbind(acf_results, acf_res$acf)
  pacf_results <- rbind(pacf_results, pacf_res$acf)
}

# set the column names of the data frames
colnames(acf_results) <- names(rates)[-1]
colnames(pacf_results) <- names(rates)[-1]

# Principal Component Analysis --------------------------------------------
usd_pc <- prcomp(usd_cols[,-1])
summary(usd_pc)
eur_pc <- prcomp(eur_cols[,-1])
summary(eur_pc)
cor_matrix <- cor(usd_cols[,-1])

# Individual Plots -------------------------------------------------------------------
#plot function
pdf_file <- "/Users/tophcottle/Documents/Personal/mfx_products/output/fx_plots.pdf"
fx_plot <- function(rates) {
  rates$date <- as.Date(rates$date, "%Y-%m-%d")
  # Create a line chart for each column in the data frame
  for (col in names(rates)[-1]) {
    p <- ggplot(rates, aes(x = date, y = .data[[col]])) +
      geom_line() +
      labs(title = col, subtitle = "USD", x= "Date", y = "")+
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")
    plot(p)
  }
}
pdf(pdf_file)
fx_plot(rates)
dev.off()


# Index Plots -------------------------------------------------------------
usd_ind_cols <- fx_index %>% select("date", matches("USD"))
usd_ind_cols <- usd_ind_cols %>%
  pivot_longer(cols = -date, names_to = "currency", values_to = "rate")
usd_ind_cols$date <- as.Date(usd_ind_cols$date)

# Plot the data
ggplot(usd_ind_cols) +
  geom_line(aes(x = date, y = rate, group = currency))+
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")+
  theme_classic()

eur_ind_cols <- fx_index %>% select("date",matches("EUR"))
eur_ind_cols <- eur_ind_cols %>%
  pivot_longer(cols = -date, names_to = "currency", values_to = "rate")
eur_ind_cols$date <- as.Date(eur_ind_cols$date)

# Plot the data
ggplot(eur_ind_cols) +
  geom_line(aes(x = date, y = rate, group = currency))+
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")+
  toph_theme()


toph_theme <- function() {
  theme(
    plot.title = element_text(size = rel(1.25), family = "helvetica"),
    plot.background = element_rect(fill = "#b5d1cc"),
    panel.border = element_rect(colour = "black"),
    axis.text = element_text(colour = "black", face = "italic", family = "helvetica"),
    axis.title = element_text(colour = "black", family = "helvetica"),
    axis.ticks = element_line(colour = "black"),
    legend.position = "bottom"
  )
}






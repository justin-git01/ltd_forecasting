library(FoReco)
library(tidyverse)
library(readxl)
library(fpp3)
library(urca)

# Load ltd aggregate date
ltd_agg <- read_excel("data/LTD data aggregate.xlsx") %>%
  dplyr::select(c(Date, ltd, sales, hvi))

# Load ltd unit data and join with aggregate data
ltd_unit <- read_excel("data/LTD unit records summary.xlsx") |>
  dplyr::select(dd, ltd_total, ltd_nonres, ltd_comm, ltd_ind, ltd_other, ltd_res) |>
  left_join(ltd_agg, by = c("dd" = "Date")) |>
  dplyr::select(-ltd)

# Rename ltd unit data
names(ltd_unit) <- c("Date", "Total", "NonRes", "Comm", "Ind", "Other", "Res", "Sales", "hvi")

# Convert ltd unit data to tsibble object
ltd_unit <- ltd_unit %>%
  mutate(Month = yearmonth(Date)) %>%
  select(-Date) %>%
  as_tsibble(index = Month) %>%
  relocate(Month)

# Filter first 10 years only
ltd_unit <- ltd_unit[1:120,]

# Perform cross-validation with 5 years initially
ltd_cv <- ltd_unit |>
  stretch_tsibble(.init = 60, .step = 1)

# Number of folds
folds <- length(unique(ltd_cv$.id))

# ARIMA 

# Define set of data
base_arima_forecast <- reconciled_arima <- test_set <- array(, dim = c(6, 12, folds))

for (i in 1:folds){
  # Filter to fold
  ltd_filtered <- ltd_cv %>% filter(.id == i) %>% dplyr::select(-.id)
  
  # Pre-define set of data
  data <- NULL
  base_fc <- NULL
  residuals_fc <- NULL
  test_fc <- NULL
  
  # Monthly series
  data$k1 <- ts(ltd_filtered[, -1], frequency = 12, start = c(13, 7))
  colnames(data$k1) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res", "Sales", "hvi")
  
  # BI-MONTHLY SERIES
  data$k2 <- ts(apply(data$k1, 2,
                      function(x) colSums(matrix(x, nrow = 2))),
                frequency = 6, start = c(13, 7))
  
  # QUARTERLY SERIES
  data$k3 <- ts(apply(data$k1, 2,
                      function(x) colSums(matrix(x, nrow = 3))),
                frequency = 4, start = c(13, 7))
  
  # FOUR-MONTHLY SERIES
  data$k4 <- ts(apply(data$k1, 2,
                      function(x) colSums(matrix(x, nrow = 4))),
                frequency = 3, start = c(13, 7))
  
  # SEMI-ANNUAL SERIES
  data$k6 <- ts(apply(data$k1, 2,
                      function(x) colSums(matrix(x, nrow = 6))),
                frequency = 2, start = c(13, 7))
  
  # ANNUAL SERIES
  data$k12 <- ts(apply(data$k1, 2,
                       function(x) colSums(matrix(x, nrow = 12))),
                 frequency = 1, start = c(13, 7))

  # MONTHLY FORECASTS
  base_fc$k1 <- matrix(NA, nrow = 12, ncol = ncol(data$k1)-2)
  residuals_fc$k1 <- matrix(NA, nrow = nrow(data), ncol = ncol(data$k1)-2)
  for (i in 1:6) {
    train <- values$k1[1:168, i]
    forecast_res <- auto.arima(train)
    base_fc$k1[, i] <- forecast_res[[1]]
    residuals_fc$k1[, i] <- forecast_res[[2]]
  }
  base_fc$k1 <- ts(base_fc$k1, frequency = 12, start = c(10, 1))
  colnames(base_fc$k1) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
  residuals_fc$k1 <- ts(residuals_fc$k1, frequency = 12)
  colnames(residuals_fc$k1) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
  test_fc$k1 <- data$k1[-c(1:108), c(1:6)]
  
  
  base_arima_forecast[,,i] <- matrix(rnorm(12*6, 12, 6))
  test_set[, ,i] <- matrix(rnorm(12*6, 12,6))
  reconciled_arima[, , i] <- matrix(rnorm(12*6,12,6))
}

colMeans(base_arima_forecast-test_set)
# VAR n VECM



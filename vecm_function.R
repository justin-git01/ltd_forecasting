###VECM 
library(fable)
library(tsibble)
library(dplyr)
library(feasts)
library(vars)

forecast_fun <- function(train, sales, hvi, period, length, fc_range, use_vecm = FALSE) {
  dat <- data.frame(cbind(train, sales, hvi))
  start_date <- as.Date("2013-07-01")
  end_date <- seq.Date(start_date, by = period, length.out = length)
  dat$Date <- end_date
  
  dat <- dat %>%
    mutate(Month = yearmonth(Date)) |>
    dplyr::select(-Date)  |>
    as_tsibble(index = Month) |>
    relocate(Month)
  
  if (use_vecm) {
    # Fit an auto VECM model if use_vecm is TRUE
    # The VECM model is a more sophisticated model that requires the series to be cointegrated.
    # You might use auto-selection methods or domain knowledge to choose rank and lag parameters.
    fit <- dat |> 
      model(VECM(train ~ sales + hvi))
  } else if (period == "6 months" || period == "year") {
    fit <- dat |>
      model(VAR(vars(train, sales, hvi) ~ AR(p = 1)))
  } else {
    fit <- dat |>
      model(VAR(vars(train, sales, hvi)))
  }
  
  # Extract residuals
  residuals <- residuals(fit)
  
  # Replace missing values in residuals
  residuals$train[is.na(residuals$train)] <- mean(residuals$train, na.rm = TRUE)
  
  forecast_res <- fit |>
    fabletools::forecast(h = fc_range)
  
  train_res <- as.numeric(residuals$train)
  forecast_output <- as.numeric(forecast_res$.mean[, "train"])
  
  output <- list(forecast_output, train_res)
  return(output)
}

# Example usage:

train <- data$k6[1:18, 1]
sales <- data$k6[1:18, 7]
hvi <- data$k6[1:18, 8]

dat <- data.frame(cbind(train, sales, hvi))
start_date <- as.Date("2013-07-01")
end_date <- seq.Date(start_date, by = "6 months", length.out = 18)
dat$Date <- end_date

dat <- dat %>%
  mutate(Month = yearmonth(Date)) |>
  dplyr::select(-Date)  |>
  as_tsibble(index = Month) |>
  relocate(Month)

dat1 <- ts(dat[,-1], frequency = 2)

# Johansen cointegration test to determine the rank
coint_test <- ca.jo(dat1, spec = "longrun", K = 2)  # K is the lag order, adjust based on your data

# Fit the VECM model using the rank from Johansen test
vecm_fit <- vec2var(coint_test, r = 1) 
vecm_fit <- vec2var(coint_test, r = coint_test@rank)  # Convert VECM to VAR for forecasting

residuals(vecm_fit)
# Forecast
forecast_res <- as.numeric(predict(vecm_fit, n.ahead = 2)$fcst$train[,1])

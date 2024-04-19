
library(fpp3)

#### Final
forecast_fun <- function(train, sales, hvi, period, length, fc_range){
dat <- data.frame(train = log(train),
                  sales = log(sales),
                  hvi = log(hvi),
                  Date = seq(as.Date("2013-07-01"), by = period, length.out = length))


dat <- dat %>% 
  mutate(Month = yearmonth(Date)) |>
  dplyr::select(-Date)  |>
  as_tsibble(index = Month) |>
  relocate(Month)

if (period == "6 months" | period == "year"){
  fit <- dat |>
    model(VAR(vars(train, sales, hvi) ~ AR(p = 1)))
}
else {
  fit <- dat |>
    model(VAR(vars(train, sales, hvi)))
}

# Extract residuals
fitted_values <- fitted(fit)
dat <- mutate(dat, 
              fitted_train = exp(fitted_values$train),
              fitted_sales = exp(fitted_values$sales),
              fitted_hvi = exp(fitted_values$hvi))

# Compute residuals in original scale
dat <- mutate(dat,
              residual_train_original = exp(train) - fitted_train,
              residual_sales_original = exp(sales) - fitted_sales,
              residual_hvi_original = exp(hvi) - fitted_hvi)

# Forecasting
forecast_res <- forecast(fit, h = fc_range)
forecast_output <- exp(as.numeric(forecast_res$.mean[, "train"]))  # Convert back from log scale

# Collecting forecast and residuals
residuals_original <- select(dat, residual_train_original, residual_sales_original, residual_hvi_original)
output <- list(forecast_output = forecast_output, residuals_original = residuals_original)
return(output)

output <- list(forecast_output, train_res)
return(output)
}



train <- log(data$k1[1:108, 1])
sales <- log(data$k1[1:108, 7])
hvi <- log(data$k1[1:108, 8])

dat <- data.frame(train = train,
                  sales = sales,
                  hvi = hvi,
                  Date = seq(as.Date("2013-07-01"), by = "month", length.out = 108))


dat <- dat %>% 
  mutate(Month = yearmonth(Date)) |>
  dplyr::select(-Date)  |>
  as_tsibble(index = Month) |>
  relocate(Month)

fit <- dat |>
  model(VAR(vars(train, sales, hvi)))

forecast_res <- fit |>
  fabletools::forecast(h = 12) 

residuals <- residuals(fit)

train_res <- as.numeric(residuals$train)
forecast_output <- exp(as.numeric(forecast_res$.mean[, "train"]))


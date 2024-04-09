
library(fpp3)

#### Final
forecast_fun <- function(train, sales, hvi, period, length, fc_range){
dat <- data.frame(cbind(train, sales, hvi))
start_date <- as.Date("2013-07-01") 
end_date <- seq.Date(start_date, by = period, length.out = length)
dat$Date <- end_date

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


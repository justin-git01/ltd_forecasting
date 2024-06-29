
library(fpp3)

#### VAR
var_forecast_fun <- function(train, sales, hvi, lending, period, length, fc_range, include_lending = TRUE){

  if(include_lending){
    dat <- data.frame(train = log(train),
                      sales = log(sales),
                      lending = log(lending),
                      Date = seq(as.Date("2013-07-01"), by = period, length.out = length))
  } else {
    dat <- data.frame(train = log(train),
                      sales = log(sales),
                      hvi = log(hvi),
                      Date = seq(as.Date("2013-07-01"), by = period, length.out = length))
  }

  dat <- dat %>% 
    mutate(Month = yearmonth(Date)) |>
    dplyr::select(-Date)  |>
    as_tsibble(index = Month) |>
    relocate(Month)
  
  if (include_lending){
    if (period %in% c("6 months", "year")){
      fit <- dat |>
        model(VAR(vars(train, sales, lending) ~ AR(p = 1)))
    }
    else {
      fit <- dat |>
        model(VAR(vars(train, sales, lending)))
    }
  }
  else {
    if (period == "6 months" | period == "year"){
      fit <- dat |>
        model(VAR(vars(train, sales, hvi) ~ AR(p = 1)))
    }
    else {
      fit <- dat |>
        model(VAR(vars(train, sales, hvi)))
    } 
  }
  
  fitted_values <- fitted(fit)
  
  # Compute residuals in original scale
  dat <- mutate(dat,
                residual_train = exp(train) - exp(fitted_values$train)) 
  
  dat$residual_train[is.na(dat$residual_train)] <- mean(dat$residual_train, na.rm = TRUE)
  
  forecast_res <- fit |>
    fabletools::forecast(h = fc_range) 
  
  train_res <- dat$residual_train
  forecast_output <- exp(as.numeric(forecast_res$.mean[, "train"]))
  
  output <- list(forecast_output, train_res)
  return(output)
}

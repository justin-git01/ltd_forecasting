###VECM 

vecm_forecast_fun <- function(train, sales, hvi, period, length, fc_range, lag){
  dat <- data.frame(train = log(train),
                    sales = log(sales),
                    hvi = log(hvi),
                    Date = seq(as.Date("2013-07-01"), by = period, length.out = length))
  
  dat_tsibble <- dat %>%
    mutate(Month = yearmonth(Date)) %>%
    dplyr::select(-Date) %>%
    as_tsibble(index = Month) %>%
    relocate(Month)
  
  dat1 <- ts(dat_tsibble[,-1], frequency = fc_range)
  
  # Johansen cointegration test to determine the rank
  coint_test <- ca.jo(dat1, spec = "longrun", K = lag)  
  
  r <- sum(coint_test@teststat > coint_test@cval[, "5pct"])  # Example: Using r=1
  
  # Limit r to be 2
  if (r > 2){
    r = 2
  }

  # Condition test for r 
  if (r != 0){
    # Fit the VECM model using the rank from Johansen test
    vecm_fit <- vars::vec2var(coint_test, r = r) 
    
    # Construct residuals
    fitted_values <- fitted(vecm_fit)
    
    fitted_train <- c(rep(mean(fitted_values[,1], na.rm = T),lag), fitted_values[,1] )
    
    dat_tsibble <- mutate(dat_tsibble,
                          residual_train = exp(train) - exp(fitted_train)) 
    
    # Forecasting
    forecast_output <- exp(as.numeric(predict(vecm_fit, n.ahead = fc_range)$fcst$train[,1]))
    lower <- exp(as.numeric(predict(vecm_fit, n.ahead = fc_range)$fcst$train[,2]))
    upper <- exp(as.numeric(predict(vecm_fit, n.ahead = fc_range)$fcst$train[,3]))
    train_res <- dat_tsibble$residual_train

    output <- list(fc = forecast_output, 
                   res = train_res,
                   lower = lower,
                   upper = upper)
    return(output)
  }
  else {
    warning("There is no cointegration (r=0), VAR model fitted instead")
    var_forecast_fun(train, sales, hvi, period, length, fc_range)
  }
}

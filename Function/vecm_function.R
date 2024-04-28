###VECM 

vecm_forecast_fun <- function(train, sales, hvi, period, length, fc_range){
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
  coint_test <- ca.jo(dat1, spec = "longrun", K = 2)  
  
  r <- sum(coint_test@teststat > coint_test@cval[, "5pct"])  # Example: Using r=1
  
  # Condition test for r 
  if (r != 0){
    # Fit the VECM model using the rank from Johansen test
    vecm_fit <- vars::vec2var(coint_test, r = r) 
    
    # Construct residuals
    fitted_values <- fitted(vecm_fit)
    
    fitted_train <- c(rep(mean(fitted_values[,1], na.rm = T),2), fitted_values[,1] )
    
    dat_tsibble <- mutate(dat_tsibble,
                          residual_train = exp(train) - exp(fitted_train)) 
    
    # Forecasting
    forecast_res <- as.numeric(predict(vecm_fit, n.ahead = fc_range)$fcst$train[,1])
    
    train_res <- dat_tsibble$residual_train
    forecast_output <- exp(forecast_res)
    
    output <- list(forecast_output, train_res)
    return(output)
  }
  else {
    warning("There is no cointegration (r=0), use VAR instead")
    var_forecast_fun(train, sales, hvi, period, length, fc_range)
  }
}
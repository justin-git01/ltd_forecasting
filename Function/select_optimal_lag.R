# This function takes in a lag range based on the temporal aggregation 
# and then choose the most optimal lag length based on AICc

select_optimal_lag <- function(dat1, lag_range, include_lending){
  aicc_list <- sapply(lag_range, function(lag){
    coint_test <- ca.jo(dat1, spec = "longrun", K = lag)
    r <- sum(coint_test@teststat > coint_test@cval[, "5pct"])
    
    if (r>2){
      r = 2
    }
    
    if (r != 0){
      vecm_fit <- vars::vec2var(coint_test, r = r)
      aic_value <- AIC(vecm_fit)  
      k <- vecm_fit$K
      n <- nrow(dat1)
      aicc_value <- aic_value + (2 * k * (k + 1)) / (n - k - 1)
      return(aicc_value)
    } else {
      return(Inf)  # If no cointegration, return Inf as AICc
    }
  })
  
  optimal_lag <- lag_range[which.min(aicc_list)]
  return(optimal_lag)
}
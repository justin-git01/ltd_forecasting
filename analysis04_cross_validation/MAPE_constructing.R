###################### COMPUTING MAPE ##################################

# Understanding the Scale:
#   
# 0% MAPE: This indicates a perfect forecast with no error.
# 0-10% MAPE: Generally considered a highly accurate forecast.
# 10-20% MAPE: Indicates good accuracy.
# 20-50% MAPE: Indicates reasonable or moderate accuracy.
# 50%+ MAPE: Suggests poor accuracy, and the forecasting model may need significant improvements.

# # Base
# base_arima_forecast
# base_vecm_forecast
# 
# # ARIMA
# cross_rec_arima # cross-sec
# temp_rec_arima # temp
# reconciled_arima # cross-temp
# 
# # VECM
# vecm_hts_reconciled # cross-sec
# vecm_thf_reconciled # temp
# reconciled_vecm_tcs # cross-temp
# 
# # Test set
# test_set

# ARIMA
## Base forecast
errors_arima_base <- base_arima_forecast - test_set
percent_errors_arima_base <- errors_arima_base / test_set
mape_arima_base <- 100 * apply(abs(percent_errors_arima_base), c(1,2), mean)

## Reconciled forecast
### cross-sec (hts)
errors_arima_cross_sec <- cross_rec_arima - test_set
percent_errors_arima_cross_sec <- errors_arima_cross_sec / test_set
mape_arima_cross_sec <- 100 * apply(abs(percent_errors_arima_cross_sec), c(1,2), mean)

### temp (thf)
errors_arima_temp <- temp_rec_arima - test_set
percent_errors_arima_temp <- errors_arima_temp / test_set
mape_arima_temp <- 100 * apply(abs(percent_errors_arima_temp), c(1,2), mean)

### cross-temp (tcs)
errors_arima_cross_temp <- reconciled_arima - test_set
percent_errors_arima_cross_temp <- errors_arima_cross_temp / test_set
mape_arima_cross_temp <- 100 * apply(abs(percent_errors_arima_cross_temp), c(1,2), mean)


# VECM
## Base forecast
errors_vecm_base <- base_vecm_forecast - test_set
percent_errors_vecm_base <- errors_vecm_base / test_set
mape_vecm_base <- 100 * apply(abs(percent_errors_vecm_base), c(1,2), mean)

## Reconciled forecast
### cross-sec (hts)
errors_vecm_cross_sec <- vecm_hts_reconciled - test_set
percent_errors_vecm_cross_sec <- errors_vecm_cross_sec / test_set
mape_vecm_cross_sec <- 100 * apply(abs(percent_errors_vecm_cross_sec), c(1,2), mean)

### temp (thf)
errors_vecm_temp <- vecm_thf_reconciled - test_set
percent_errors_vecm_temp <- errors_vecm_temp / test_set
mape_vecm_temp <- 100 * apply(abs(percent_errors_vecm_temp), c(1,2), mean)

### cross-temp (tcs)
errors_vecm_cross_temp <- reconciled_vecm_tcs - test_set
percent_errors_vecm_cross_temp <- errors_vecm_cross_temp / test_set
mape_vecm_cross_temp <- 100 * apply(abs(percent_errors_vecm_cross_temp), c(1,2), mean)

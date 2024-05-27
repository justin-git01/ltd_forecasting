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

## RMSE for base forecast (row: level , col: h_step)
RMSE_array_arima_base <- sqrt((base_arima_forecast-test_set)^2)
RMSE_arima_h_base <- apply(RMSE_array_arima_base, c(1,2), mean)

## RMSE for reconciled forecast (row: level, col: h_step)

### cross-sec
RMSE_array_arima_cross_rec <- sqrt((cross_rec_arima-test_set)^2)
RMSE_arima_h_cross_rec <- apply(RMSE_array_arima_cross_rec, c(1,2), mean)

### temp
RMSE_array_arima_temp_rec <- sqrt((temp_rec_arima-test_set)^2)
RMSE_arima_h_temp_rec <- apply(RMSE_array_arima_temp_rec, c(1,2), mean)

### cross-temp
RMSE_array_arima_rec <- sqrt((reconciled_arima-test_set)^2)
RMSE_arima_h_rec <- apply(RMSE_array_arima_rec, c(1,2), mean)


## reconciled RMSE / base RMSE (< 1 means improvement of reconciled forecast)
RMSE_arima_prop <- RMSE_arima_h_rec / RMSE_arima_h_base

## Filter out total level RMSE proportion
RMSE_arima_prop[1,]


# VECM

## RMSE for base forecast (row: level , col: h_step)
RMSE_array_vecm_base <- sqrt((base_vecm_forecast-test_set)^2)
RMSE_vecm_h_base <- apply(RMSE_array_vecm_base, c(1,2), mean)

## RMSE for reconciled forecast (row: level, col: h_step)

### cross-sec
RMSE_array_vecm_hts_rec <- sqrt((vecm_hts_reconciled-test_set)^2)
RMSE_vecm_h_hts_rec <- apply(RMSE_array_vecm_hts_rec, c(1,2), mean)

### reconciled RMSE / base RMSE  (< 1 means improvement of reconciled forecast)
RMSE_vecm_hts_prop <- RMSE_vecm_h_hts_rec / RMSE_vecm_h_base

### Filter out total level RMSE proportion 
RMSE_vecm_hts_prop[1,]

### tcs cross-temp
RMSE_array_vecm_tcs_rec <- sqrt((reconciled_vecm_tcs-test_set)^2)
RMSE_vecm_h_tcs_rec <- apply(RMSE_array_vecm_tcs_rec, c(1,2), mean)

## reconciled RMSE / base RMSE  (< 1 means improvement of reconciled forecast)
RMSE_vecm_tcs_prop <- RMSE_vecm_h_tcs_rec / RMSE_vecm_h_base

## Filter out total level RMSE proportion 
RMSE_vecm_tcs_prop[1,]


### temp (thf)
RMSE_array_vecm_thf_rec <- sqrt((vecm_thf_reconciled-test_set)^2)
RMSE_vecm_h_thf_rec <- apply(RMSE_array_vecm_thf_rec, c(1,2), mean)

## reconciled RMSE / base RMSE  (< 1 means improvement of reconciled forecast)
RMSE_vecm_thf_prop <- RMSE_vecm_h_thf_rec / RMSE_vecm_h_base

## Filter out total level RMSE proportion 
RMSE_vecm_thf_prop[1,]


base_arima_forecast
base_vecm_forecast
reconciled_arima
reconciled_vecm_struc
reconciled_vecm_tcs
test_set


# ARIMA

## RMSE for base forecast (row: level , col: h_step)
RMSE_array_arima_base <- sqrt((base_arima_forecast-test_set)^2)
RMSE_arima_h_base <- apply(RMSE_array_arima_base, c(1,2), mean)

## RMSE for reconciled forecast (row: level, col: h_step)
RMSE_array_arima_rec <- sqrt((reconciled_arima-test_set)^2)
RMSE_arima_h_rec <- apply(RMSE_array_arima_rec, c(1,2), mean)

## reconciled RMSE / base RMSE (< 1 means improvement of reconciled forecast)
RMSE_arima_prop <- RMSE_arima_h_rec / RMSE_arima_h_base

## Filter out total level RMSE proportion
RMSE_arima_prop[1,]


# VECM struc

## RMSE for base forecast (row: level , col: h_step)
RMSE_array_vecm_base <- sqrt((base_vecm_forecast-test_set)^2)
RMSE_vecm_h_base <- apply(RMSE_array_vecm_base, c(1,2), mean)

## RMSE for reconciled forecast (row: level, col: h_step)
RMSE_array_vecm_struc_rec <- sqrt((reconciled_vecm_struc-test_set)^2)
RMSE_vecm_h_struc_rec <- apply(RMSE_array_vecm_struc_rec, c(1,2), mean)

## reconciled RMSE / base RMSE  (< 1 means improvement of reconciled forecast)
RMSE_vecm_struc_prop <- RMSE_vecm_h_struc_rec / RMSE_vecm_h_base

## Filter out total level RMSE proportion 
RMSE_vecm_struc_prop[1,]

# VECM tcs

## RMSE for reconciled forecast (row: level, col: h_step)
RMSE_array_vecm_tcs_rec <- sqrt((reconciled_vecm_tcs-test_set)^2)
RMSE_vecm_h_tcs_rec <- apply(RMSE_array_vecm_tcs_rec, c(1,2), mean)

## reconciled RMSE / base RMSE  (< 1 means improvement of reconciled forecast)
RMSE_vecm_tcs_prop <- RMSE_vecm_h_tcs_rec / RMSE_vecm_h_base

## Filter out total level RMSE proportion 
RMSE_vecm_tcs_prop[1,]

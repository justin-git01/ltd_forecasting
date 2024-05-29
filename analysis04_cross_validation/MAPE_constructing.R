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
# hts_reconciled_vecm # cross-sec
# thf_reconciled_vecm # temp
# reconciled_vecm_tcs # cross-temp
# 
# # Test set
# test_set

load(here::here("data/base_arima_fc.RData"))
load(here::here("data/rec_arima.RData"))
load(here::here("data/temp_rec_arima.RData"))
load(here::here("data/cross_sec_rec_arima.RData"))

load(here::here("data/base_vecm_fc.RData"))
load(here::here("data/rec_vecm.RData"))
load(here::here("data/temp_rec_vecm.RData"))
load(here::here("data/cross_sec_rec_vecm.RData"))

load(here::here("data/test_set.RData"))

# ARIMA
## Base forecast
errors_arima_base <- base_arima_forecast - test_set
percent_errors_arima_base <- abs(errors_arima_base / test_set) * 100
mape_arima_base <- apply((percent_errors_arima_base), c(1,2), mean)

## Reconciled forecast
### cross-sec (hts)
errors_arima_cross_sec <- cross_sec_arima - test_set
percent_errors_arima_cross_sec <- abs(errors_arima_cross_sec / test_set)*100
mape_arima_cross_sec <- apply((percent_errors_arima_cross_sec), c(1,2), mean)

### temp (thf)
errors_arima_temp <- temp_rec_arima - test_set
percent_errors_arima_temp <- abs(errors_arima_temp / test_set)*100
mape_arima_temp <- apply((percent_errors_arima_temp), c(1,2), mean)

### cross-temp (tcs)
errors_arima_cross_temp <- reconciled_arima - test_set
percent_errors_arima_cross_temp <- abs(errors_arima_cross_temp / test_set)*100
mape_arima_cross_temp <- apply((percent_errors_arima_cross_temp), c(1,2), mean)


# VECM
## Base forecast
errors_vecm_base <- base_vecm_forecast - test_set
percent_errors_vecm_base <- abs(errors_vecm_base / test_set)*100
mape_vecm_base <- apply((percent_errors_vecm_base), c(1,2), mean)

## Reconciled forecast
### cross-sec (hts)
errors_vecm_cross_sec <- hts_reconciled_vecm - test_set
percent_errors_vecm_cross_sec <- abs(errors_vecm_cross_sec / test_set)*100
mape_vecm_cross_sec <- apply((percent_errors_vecm_cross_sec), c(1,2), mean)

### temp (thf)
errors_vecm_temp <- thf_reconciled_vecm - test_set
percent_errors_vecm_temp <- abs(errors_vecm_temp / test_set)*100
mape_vecm_temp <- apply((percent_errors_vecm_temp), c(1,2), mean)

### cross-temp (tcs)
errors_vecm_cross_temp <- reconciled_vecm_tcs - test_set
percent_errors_vecm_cross_temp <- abs(errors_vecm_cross_temp / test_set)*100
mape_vecm_cross_temp <-  apply((percent_errors_vecm_cross_temp), c(1,2), mean)

save(mape_arima_base, file = "data/mape_base_arima.RData")
save(mape_vecm_base, file = "data/mape_base_vecm.RData")
save(mape_arima_cross_temp, file = "data/mape_cross_temp_arima.RData")
save(mape_vecm_cross_temp, file = "data/mape_cross_temp_vecm.RData")
save(mape_arima_temp, file = "data/mape_temp_arima.RData")
save(mape_vecm_temp, file = "data/mape_temp_vecm.RData")
save(mape_arima_cross_sec, file = "data/mape_cross_sec_arima.RData")
save(mape_vecm_cross_sec, file = "data/mape_cross_sec_vecm.RData")




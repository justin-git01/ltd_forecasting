###################### COMPUTING MAPE ##################################

# Understanding the Scale:
#   
# 0% MAPE: This indicates a perfect forecast with no error.
# 0-10% MAPE: Generally considered a highly accurate forecast.
# 10-20% MAPE: Indicates good accuracy.
# 20-50% MAPE: Indicates reasonable or moderate accuracy.
# 50%+ MAPE: Suggests poor accuracy, and the forecasting model may need significant improvements.

# final_base
# final_hts # cross-sec
# final_thf # temp
# final_tcs # cross-temp
# 
# # Test set
# test_set

load(here::here("data/sa_base.RData"))
load(here::here("data/sa_hts.RData"))
load(here::here("data/sa_thf.RData"))
load(here::here("data/sa_tcs.RData"))

load(here::here("data/test_set.RData"))

# VECM
## Base forecast
errors_vecm_base <- final_base - test_set
percent_errors_vecm_base <- abs(errors_vecm_base / test_set)*100
mape_vecm_base <- apply((percent_errors_vecm_base), c(1,2), mean)

## Reconciled forecast
### cross-sec (hts)
errors_vecm_cross_sec <- final_hts - test_set
percent_errors_vecm_cross_sec <- abs(errors_vecm_cross_sec / test_set)*100
mape_vecm_cross_sec <- apply((percent_errors_vecm_cross_sec), c(1,2), mean)

### temp (thf)
errors_vecm_temp <- final_thf - test_set
percent_errors_vecm_temp <- abs(errors_vecm_temp / test_set)*100
mape_vecm_temp <- apply((percent_errors_vecm_temp), c(1,2), mean)

### cross-temp (tcs)
errors_vecm_cross_temp <- final_tcs - test_set
percent_errors_vecm_cross_temp <- abs(errors_vecm_cross_temp / test_set)*100
mape_vecm_cross_temp <-  apply((percent_errors_vecm_cross_temp), c(1,2), mean)

save(mape_vecm_base, file = "data/mape_base_vecm.RData")
save(mape_vecm_cross_temp, file = "data/mape_cross_temp_vecm.RData")
save(mape_vecm_temp, file = "data/mape_temp_vecm.RData")
save(mape_vecm_cross_sec, file = "data/mape_cross_sec_vecm.RData")




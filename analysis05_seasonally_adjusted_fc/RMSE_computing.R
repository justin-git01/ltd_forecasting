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

## RMSE for base forecast (row: level , col: h_step)
RMSE_array_vecm_base <- ((final_base-test_set)^2)
RMSE_vecm_h_base <- sqrt(apply(RMSE_array_vecm_base, c(1,2), mean))

## RMSE for reconciled forecast (row: level, col: h_step)

### cross-sec
RMSE_array_vecm_hts_rec <- ((final_hts-test_set)^2)
RMSE_vecm_h_hts_rec <- sqrt(apply(RMSE_array_vecm_hts_rec, c(1,2), mean))

### temp (thf)
RMSE_array_vecm_thf_rec <- ((final_thf-test_set)^2)
RMSE_vecm_h_thf_rec <- sqrt(apply(RMSE_array_vecm_thf_rec, c(1,2), mean))

### tcs cross-temp
RMSE_array_vecm_tcs_rec <- ((final_tcs-test_set)^2)
RMSE_vecm_h_tcs_rec <- sqrt(apply(RMSE_array_vecm_tcs_rec, c(1,2), mean))


save(RMSE_vecm_h_base, file = "data/rmse_base_vecm.RData")
save(RMSE_vecm_h_tcs_rec, file = "data/rmse_cross_temp_vecm.RData")
save(RMSE_vecm_h_thf_rec, file = "data/rmse_temp_vecm.RData")
save(RMSE_vecm_h_hts_rec, file = "data/rmse_cross_sec_vecm.RData")



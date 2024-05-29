library(FoReco)
library(tidyverse)
library(readxl)
library(fpp3)
library(urca)

source("Function/adjust_series_fun.R")
source("Function/test_extract_fun.R")

# Load ltd aggregate date
ltd_agg <- read_excel("data/LTD_new.xlsx", sheet = 1) |>
  rename(Date = ...1,
         ltd = LTD,
         sales = SALES,
         hvi = HVI) |>
  dplyr::select(c(Date, ltd, sales, hvi))

# Load ltd unit data and join with aggregate data
ltd_unit <- read_excel("data/LTD_new.xlsx", sheet = 2) |>
  rename(Date = ...1) |>
  dplyr::select(Date, ltd_total, ltd_nonres, ltd_comm, ltd_ind, ltd_other, ltd_res) |>
  left_join(ltd_agg, by = c("Date")) |>
  dplyr::select(-ltd)

# Rename ltd unit data
names(ltd_unit) <- c("Date", "Total", "NonRes", "Comm", "Ind", "Other", "Res", "Sales", "hvi")

# Convert ltd unit data to tsibble object
ltd_unit <- ltd_unit %>%
  mutate(Month = yearmonth(Date)) %>%
  select(-Date) %>%
  as_tsibble(index = Month) %>%
  relocate(Month)

# Train-test set 
ltd_unit_train <- ltd_unit[1:117,]
ltd_unit_test <- ltd_unit[-c(1:117),]

# Perform cross-validation with 6 years initially
ltd_cv <- ltd_unit_train |>
  stretch_tsibble(.init = 108, .step = 1)

# Number of folds
folds <- length(unique(ltd_cv$.id))

# ARIMA 

# Define set of data
base_arima_forecast <- reconciled_arima <- cross_sec_arima <- temp_rec_arima <-  test_set <- array(, dim = c(6, 12, folds))

# Add row names for the array
rownames(base_arima_forecast) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
rownames(reconciled_arima) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
rownames(cross_rec_arima) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
rownames(temp_rec_arima) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
rownames(test_set) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")


for (t in 1:folds){
  # Filter to fold
  ltd_filtered <- ltd_cv %>% filter(.id == t) %>% dplyr::select(-.id)

  # Extract test set
  test_set[, ,t] <- test_extract(ltd_filtered)
  
  # Pre-define set of data
  data <- NULL
  base_fc <- NULL
  residuals_fc <- NULL

  # Monthly series
  data$k1 <- adjust_series(ltd_filtered[, -1], freq = 12)
  colnames(data$k1) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res", "Sales", "hvi")

  # BI-MONTHLY SERIES
  data$k2 <- adjust_series(data$k1, freq = 6)

  # QUARTERLY SERIES
  data$k3 <- adjust_series(data$k1, freq = 4)

  # FOUR-MONTHLY SERIES
  data$k4 <- adjust_series(data$k1, freq = 3)

  # SEMI-ANNUAL SERIES
  data$k6 <- adjust_series(data$k1, freq = 2)

  # ANNUAL SERIES
  data$k12 <- adjust_series(data$k1, freq = 1)

  # MONTHLY FORECASTS
  base_fc$k1 <- matrix(NA, nrow = 12, ncol = ncol(data$k1)-2)
  residuals_fc$k1 <- matrix(NA, nrow = nrow(data$k1), ncol = ncol(data$k1)-2)
  for (i in 1:6) {
    train <- data$k1[, i]
    forecast_arima <- forecast::forecast(forecast::auto.arima(train), h = 12)
    base_fc$k1[, i] <- forecast_arima$mean
    residuals_fc$k1[, i] <- forecast_arima$residuals
  }
  base_fc$k1 <- ts(base_fc$k1, frequency = 12)
  colnames(base_fc$k1) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
  residuals_fc$k1 <- ts(residuals_fc$k1, frequency = 12)
  colnames(residuals_fc$k1) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")

  # BI-MONTHLY FORECASTS
  base_fc$k2 <- matrix(NA, nrow = 6, ncol = ncol(data$k2)-2)
  residuals_fc$k2 <- matrix(NA, nrow = nrow(data$k2), ncol = ncol(data$k2)-2)
  for (i in 1:6) {
    train <- data$k2[, i]
    forecast_arima <- forecast::forecast(forecast::auto.arima(train), h = 6)
    base_fc$k2[, i] <- forecast_arima$mean
    residuals_fc$k2[, i] <- forecast_arima$residuals
  }
  base_fc$k2 <- ts(base_fc$k2, frequency = 6)
  colnames(base_fc$k2) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
  residuals_fc$k2 <- ts(residuals_fc$k2, frequency = 6)
  colnames(residuals_fc$k2) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")

  # Quarterly
  base_fc$k3 <- matrix(NA, nrow = 4, ncol = ncol(data$k3)-2)
  residuals_fc$k3 <- matrix(NA, nrow = nrow(data$k3), ncol = ncol(data$k3)-2)
  for (i in 1:6) {
    train <- data$k3[, i]
    forecast_arima <- forecast::forecast(forecast::auto.arima(train), h = 4)
    base_fc$k3[, i] <- forecast_arima$mean
    residuals_fc$k3[, i] <- forecast_arima$residuals
  }
  base_fc$k3 <- ts(base_fc$k3, frequency = 4)
  colnames(base_fc$k3) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
  residuals_fc$k3 <- ts(residuals_fc$k3, frequency = 4)
  colnames(residuals_fc$k3) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")

  # Four-monthly
  base_fc$k4 <- matrix(NA, nrow = 3, ncol = ncol(data$k4)-2)
  residuals_fc$k4 <- matrix(NA, nrow = nrow(data$k4), ncol = ncol(data$k4)-2)
  for (i in 1:6) {
    train <- data$k4[, i]
    forecast_arima <- forecast::forecast(forecast::auto.arima(train), h = 3)
    base_fc$k4[, i] <- forecast_arima$mean
    residuals_fc$k4[, i] <- forecast_arima$residuals
  }
  base_fc$k4 <- ts(base_fc$k4, frequency = 3)
  colnames(base_fc$k4) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
  residuals_fc$k4 <- ts(residuals_fc$k4, frequency = 3)
  colnames(residuals_fc$k4) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")


  # Semi-annual
  base_fc$k6 <- matrix(NA, nrow = 2, ncol = ncol(data$k6)-2)
  residuals_fc$k6 <- matrix(NA, nrow = nrow(data$k6), ncol = ncol(data$k6)-2)
  for (i in 1:6) {
    train <- data$k6[, i]
    forecast_arima <- forecast::forecast(forecast::auto.arima(train), h = 2)
    base_fc$k6[, i] <- forecast_arima$mean
    residuals_fc$k6[, i] <- forecast_arima$residuals
  }
  base_fc$k6 <- ts(base_fc$k6, frequency = 2)
  colnames(base_fc$k6) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
  residuals_fc$k6 <- ts(residuals_fc$k6, frequency = 2)
  colnames(residuals_fc$k6) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")


  # Annual
  base_fc$k12 <- matrix(NA, nrow = 1, ncol = ncol(data$k12)-2)
  residuals_fc$k12 <- matrix(NA, nrow = nrow(data$k12), ncol = ncol(data$k12)-2)
  for (i in 1:6) {
    train <- data$k12[, i]
    forecast_arima <- forecast::forecast(forecast::auto.arima(train), h = 1)
    base_fc$k12[, i] <- forecast_arima$mean
    residuals_fc$k12[, i] <- forecast_arima$residuals
  }
  base_fc$k12 <- ts(base_fc$k12, frequency = 1)
  colnames(base_fc$k12) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
  residuals_fc$k12 <- ts(residuals_fc$k12, frequency = 1)
  colnames(residuals_fc$k12) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")

  base <- t(do.call(rbind, rev(base_fc)))
  res <- t(do.call(rbind, rev(residuals_fc)))

  kset <- c(12, 6, 4, 3, 2, 1)
  h <- 1
  colnames(base) <- paste("k", rep(kset, h * rev(kset)), "_h",
                          do.call("c", as.list(sapply(
                            rev(kset) * h,
                            function(x) seq(1:x)))),
                          sep = "")
  h <- nrow(data$k12)
  colnames(res) <- paste("k", rep(kset, h * rev(kset)), "_h",
                         do.call("c", as.list(sapply(
                           rev(kset) * h,
                           function(x) seq(1:x)))),
                         sep = "")

  C <- matrix(c(rep(1,4),
                rep(1,3), 0), byrow = TRUE, nrow = 2)
  colnames(C) <- c("Comm", "Ind", "Other", "Res")
  rownames(C) <- c("Total", "NonRes")

  FoReco_data <- list(base = base,
                      res = res,
                      C = C)

  # Cross-sectional (contemporaneous) matrix
  cs_info <- hts_tools(C = C)

  ut <- cs_info$Ut

  # Temporal matrix
  te_info <- thf_tools(m = 12)

  Zt <- te_info$Zt

  # Reconciliation

  ## cross-sec
  K <- c(1,2,3,4,6,12)
  hts_recf_list <- NULL
  for(h in 1:length(K)){
    # base forecasts
    id <- which(simplify2array(strsplit(colnames(FoReco_data$base),
                                        split = "_"))[1, ] == paste("k", K[h], sep=""))
    mbase <- t(FoReco_data$base[, id])
    # residuals
    id <- which(simplify2array(strsplit(colnames(FoReco_data$res),
                                        split = "_"))[1, ] == "k1")
    mres <- t(FoReco_data$res[, id])
    hts_recf_list[[h]] <- htsrec(mbase, C = FoReco_data$C, comb = "bu",
                                 res = mres, keep = "recf")
  }
  names(hts_recf_list) <- paste("k", K, sep="")
  hts_recf <- t(do.call(rbind, hts_recf_list[rev(names(hts_recf_list))]))
  colnames(hts_recf) <- paste("k", 
                              rep(K, sapply(hts_recf_list[rev(names(hts_recf_list))], NROW)),
                              colnames(hts_recf), sep="")
  
  # temp
 
  n <- NROW(FoReco_data$base)
  thf_recf <- matrix(NA, n, NCOL(FoReco_data$base))
  dimnames(thf_recf) <- dimnames(FoReco_data$base)
  for(l in 1:n){
    # ts base forecasts ([lowest_freq' ...  highest_freq']')
    tsbase <- FoReco_data$base[l, ]
    # ts residuals ([lowest_freq' ...  highest_freq']')
    tsres <- FoReco_data$res[l, ]
    thf_recf[l,] <- thfrec(tsbase, m = 12, comb = "struc",
                           res = tsres, keep = "recf")
  }
  
  
  ## cross-temp
  oct_recf_struc <- octrec(FoReco_data$base, m = 12, C = FoReco_data$C,
                           comb = "struc", res = FoReco_data$res, keep = "recf")

  discrepancy <- function(x, tol = sqrt(.Machine$double.eps)) {
    cs <- max(abs(cs_info$Ut %*% x))
    te <- max(abs(te_info$Zt %*% t(x)))
    cat("cs discrepancy:", ifelse(cs>tol, sprintf("%.8f", cs), 0),
        "\nte discrepancy:",ifelse(te>tol, sprintf("%.8f", te), 0))
  }
  discrepancy(oct_recf_struc)


  for (j in 1:nrow(base)){
    base_arima_forecast[j, ,t] <- t(as.matrix(base[j, -c(1:16)]))
    reconciled_arima[j, , t] <- t(as.matrix(oct_recf_struc[j, -c(1:16)]))
    cross_sec_arima[j, , t] <- t(as.matrix(hts_recf[j, -c(1:16)]))
    temp_rec_arima[j, , t] <- t(as.matrix(thf_recf[j, -c(1:16)]))
  }
}

save(base_arima_forecast, file = "data/base_arima_fc.RData")
save(reconciled_arima, file = "data/rec_arima.RData")
save(temp_rec_arima, file = "data/temp_rec_arima.RData")
save(cross_sec_arima, file = "data/cross_sec_rec_arima.RData")
save(test_set, file = "data/test_set.RData")

library(FoReco)
library(thief)
library(tidyverse)
library(readxl)

ltd_unit <- read_excel("data/LTD unit records summary.xlsx") |>
  select(dd, ltd_total, ltd_nonres, ltd_comm, ltd_ind, ltd_other, ltd_res)

names(ltd_unit) <- c("Date", "Total", "NonRes", "Comm", "Ind", "Other", "Res")

month <- matrix(NA, nrow = nrow(ltd_unit), ncol = 6)

for (i in 2:ncol(ltd_unit)){
  month[ ,i-1] = as.numeric(unlist(ltd_unit[,i]))
}

colnames(month) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")

data <- NULL
base_fc <- NULL
residuals_fc <- NULL
test_fc <- NULL

# Monthly
data$k1 <- ts(month[1:120,], frequency = 12)
colnames(data$k1) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")

# BI-MONTHLY SERIES
data$k2 <- ts(apply(data$k1, 2,
                      function(x) colSums(matrix(x, nrow = 2))),
                frequency = 6)

# QUARTERLY SERIES
data$k3 <- ts(apply(data$k1, 2,
                      function(x) colSums(matrix(x, nrow = 3))),
                frequency = 4)

# FOUR-MONTHLY SERIES
data$k4 <- ts(apply(data$k1, 2,
                      function(x) colSums(matrix(x, nrow = 4))),
                frequency = 3)

# SEMI-ANNUAL SERIES
data$k6 <- ts(apply(data$k1, 2,
                      function(x) colSums(matrix(x, nrow = 6))),
                frequency = 2)

# ANNUAL SERIES
data$k12 <- ts(apply(data$k1, 2,
                       function(x) colSums(matrix(x, nrow = 12))),
                 frequency = 1)


# MONTHLY FORECASTS
base_fc$k1 <- matrix(NA, nrow = 12, ncol = ncol(data$k1))
residuals_fc$k1 <- matrix(NA, nrow = 108, ncol = ncol(data$k1))
for (i in 1:ncol(data$k1)) {
  train <- data$k1[1:108, i]
  forecast_arima <- forecast(ets(train), h = 12)
  base_fc$k1[, i] <- forecast_arima$mean
  residuals_fc$k1[, i] <- forecast_arima$residuals
}
base_fc$k1 <- ts(base_fc$k1, frequency = 12, start = c(10, 1))
colnames(base_fc$k1) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
residuals_fc$k1 <- ts(residuals_fc$k1, frequency = 12)
colnames(residuals_fc$k1) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
test_fc$k1 <- data$k1[-c(1:108), ]


# BI-MONTHLY FORECASTS
base_fc$k2 <- matrix(NA, nrow = 6, ncol = ncol(data$k2))
residuals_fc$k2 <- matrix(NA, nrow = 54, ncol = ncol(data$k2))
for (i in 1:ncol(data$k2)) {
  train <- data$k2[1:54, i]
  forecast_arima <- forecast(ets(train), h = 6)
  base_fc$k2[, i] <- forecast_arima$mean
  residuals_fc$k2[, i] <- forecast_arima$residuals
}
base_fc$k2 <- ts(base_fc$k2, frequency = 6, start = c(10, 1))
colnames(base_fc$k2) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
residuals_fc$k2 <- ts(residuals_fc$k2, frequency = 6)
colnames(residuals_fc$k2) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
test_fc$k2 <- data$k2[-c(1:54), ]

# Quarterly
base_fc$k3 <- matrix(NA, nrow = 4, ncol = ncol(data$k3))
residuals_fc$k3 <- matrix(NA, nrow = 36, ncol = ncol(data$k3))
for (i in 1:ncol(data$k3)) {
  train <- data$k3[1:36, i]
  forecast_arima <- forecast(ets(train), h = 4)
  base_fc$k3[,i] <- forecast_arima$mean
  residuals_fc$k3[,i] <- forecast_arima$residuals
}
base_fc$k3 <- ts(base_fc$k3, frequency = 4, start = c(10,1))
colnames(base_fc$k3) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
residuals_fc$k3 <- ts(residuals_fc$k3, frequency = 4)
colnames(residuals_fc$k3) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
test_fc$k3 <- data$k3[-c(1:36), ]

# Four-monthly
base_fc$k4 <- matrix(NA, nrow = 3, ncol = ncol(data$k4))
residuals_fc$k4 <- matrix(NA, nrow = 27, ncol = ncol(data$k4))
for (i in 1:ncol(data$k4)) {
  train <- data$k4[1:27, i]
  forecast_arima <- forecast(ets(train), h = 3)
  base_fc$k4[,i] <- forecast_arima$mean
  residuals_fc$k4[,i] <- forecast_arima$residuals
}
base_fc$k4 <- ts(base_fc$k4, frequency = 3, start = c(10,1))
colnames(base_fc$k4) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
residuals_fc$k4 <- ts(residuals_fc$k4, frequency = 3)
colnames(residuals_fc$k4) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
test_fc$k4 <- data$k4[-c(1:27), ]


# Semi-annual
base_fc$k6 <- matrix(NA, nrow = 2, ncol = ncol(data$k6))
residuals_fc$k6 <- matrix(NA, nrow = 18, ncol = ncol(data$k6))
for (i in 1:ncol(data$k6)) {
  train <- data$k6[1:18, i]
  forecast_arima <- forecast(ets(train), h = 2)
  base_fc$k6[,i] <- forecast_arima$mean
  residuals_fc$k6[,i] <- forecast_arima$residuals
}
base_fc$k6 <- ts(base_fc$k6, frequency = 2, start = c(10,1))
colnames(base_fc$k6) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
residuals_fc$k6 <- ts(residuals_fc$k6, frequency = 2)
colnames(residuals_fc$k6) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
test_fc$k6 <- data$k6[-c(1:18), ]


# Annual
base_fc$k12 <- matrix(NA, nrow = 1, ncol = ncol(data$k12))
residuals_fc$k12 <- matrix(NA, nrow = 9, ncol = ncol(data$k12))
for (i in 1:ncol(data$k12)) {
  train <- data$k12[1:9, i]
  forecast_arima <- forecast(ets(train), h = 1)
  base_fc$k12[,i] <- forecast_arima$mean
  residuals_fc$k12[,i] <- forecast_arima$residuals
}
base_fc$k12 <- ts(base_fc$k12, frequency = 1, start = c(10,1))
colnames(base_fc$k12) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
residuals_fc$k12 <- ts(residuals_fc$k12, frequency = 1)
colnames(residuals_fc$k12) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
test_fc$k12 <- data$k12[-c(1:9), ]


base <- t(do.call(rbind, rev(base_fc)))
res <- t(do.call(rbind, rev(residuals_fc)))
test <- t(do.call(rbind, rev(test_fc)))

kset <- c(12, 6, 4, 3, 2, 1)
h <- 1
colnames(base) <- paste("k", rep(kset, h * rev(kset)), "_h",
                        do.call("c", as.list(sapply(
                          rev(kset) * h,
                          function(x) seq(1:x)))),
                        sep = "")


colnames(test) <- paste("k", rep(kset, h * rev(kset)), "_h",
                        do.call("c", as.list(sapply(
                          rev(kset) * h,
                          function(x) seq(1:x)))),
                        sep = "")

h <- 9
colnames(res) <- paste("k", rep(kset, h * rev(kset)), "_h",
                       do.call("c", as.list(sapply(
                         rev(kset) * h,
                         function(x) seq(1:x)))),
                       sep = "")

C <- matrix(c(rep(1,4),
              rep(1,3), 0), byrow = TRUE, nrow = 2)
colnames(C) <- c("Comm", "Ind", "Other", "Res")
rownames(C) <- c("Total", "NonRes")
obs <- data
FoReco_data <- list(base = base,
                    test = test,
                    res = res,
                    C = C,
                    obs = obs)


# Reconciliation
oct_recf <- octrec(FoReco_data$base, m = 12, C = FoReco_data$C,
                   comb = "bdshr", res = FoReco_data$res, keep = "recf")

starting_time <- as.POSIXct("2001-01-01 00:00:00", tz="UTC")

data_ct <- tibble(oct = as.numeric(oct_recf[1, -c(1:16)]),
                  base = as.numeric(base[1, -c(1:16)]),
                  obs = as.numeric(obs$k1[c(109:120), 1]),
                  time = seq(from=starting_time, by="month", length.out = 12))

score_ct <- data_ct |>
  pivot_longer(-c(time, obs), names_to = "Approach") |>
  group_by(Approach) |>
  summarise(RMSE = sqrt(mean((value-obs)^2)))
score_ct

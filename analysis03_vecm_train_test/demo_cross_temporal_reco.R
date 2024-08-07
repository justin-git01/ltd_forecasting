library(FoReco)
library(tidyverse)
library(readxl)
library(fpp3)
library(urca)

source("Function/var_function.R")
source("Function/vecm_function.R")

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


names(ltd_unit) <- c("Date", "Total", "NonRes", "Comm", "Ind", "Other", "Res", "Sales", "hvi")

month <- matrix(NA, nrow = nrow(ltd_unit), ncol = 8)

for (i in 2:ncol(ltd_unit)){
  month[ ,i-1] = as.numeric(unlist(ltd_unit[,i]))
}

colnames(month) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res", "Sales", "hvi")

data <- NULL
base_fc <- NULL
residuals_fc <- NULL
test_fc <- NULL

# Monthly
data$k1 <- ts(month[10:129,], frequency = 12, start = c(13, 7))
colnames(data$k1) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res", "Sales", "hvi")

# BI-MONTHLY SERIES
data$k2 <- ts(apply(data$k1, 2,
                      function(x) colSums(matrix(x, nrow = 2))),
                frequency = 6, start = c(13, 7))

# QUARTERLY SERIES
data$k3 <- ts(apply(data$k1, 2,
                      function(x) colSums(matrix(x, nrow = 3))),
                frequency = 4, start = c(13, 7))

# FOUR-MONTHLY SERIES
data$k4 <- ts(apply(data$k1, 2,
                      function(x) colSums(matrix(x, nrow = 4))),
                frequency = 3, start = c(13, 7))

# SEMI-ANNUAL SERIES
data$k6 <- ts(apply(data$k1, 2,
                      function(x) colSums(matrix(x, nrow = 6))),
                frequency = 2, start = c(13, 7))

# ANNUAL SERIES
data$k12 <- ts(apply(data$k1, 2,
                       function(x) colSums(matrix(x, nrow = 12))),
                 frequency = 1, start = c(13, 7))

# MONTHLY FORECASTS
base_fc$k1 <- matrix(NA, nrow = 12, ncol = ncol(data$k1)-2)
residuals_fc$k1 <- matrix(NA, nrow = 108, ncol = ncol(data$k1)-2)
for (i in 1:6) {
  train <- data$k1[1:108, i]
  sales <- data$k1[1:108, 7]
  hvi <- data$k1[1:108, 8]
  forecast_res <- vecm_forecast_fun(train, sales, hvi, "month", 108, 12, 20)
  base_fc$k1[, i] <- forecast_res[[1]]
  residuals_fc$k1[, i] <- forecast_res[[2]]
}
base_fc$k1 <- ts(base_fc$k1, frequency = 12, start = c(10, 1))
colnames(base_fc$k1) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
residuals_fc$k1 <- ts(residuals_fc$k1, frequency = 12)
colnames(residuals_fc$k1) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
test_fc$k1 <- data$k1[-c(1:108), c(1:6)]


# BI-MONTHLY FORECASTS
base_fc$k2 <- matrix(NA, nrow = 6, ncol = ncol(data$k2)-2)
residuals_fc$k2 <- matrix(NA, nrow = 54, ncol = ncol(data$k2)-2)
for (i in 1:6) {
  train <- data$k2[1:54, i]
  sales <- data$k2[1:54, 7]
  hvi <- data$k2[1:54, 8]
  #forecast_res <- forecast_fun(train, sales, hvi, "2 months", 54, 6)
  forecast_res <- vecm_forecast_fun(train, sales, hvi, "2 months", 54, 6, 5)
  base_fc$k2[, i] <- forecast_res[[1]]
  residuals_fc$k2[, i] <- forecast_res[[2]]
}
base_fc$k2 <- ts(base_fc$k2, frequency = 6, start = c(10, 1))
colnames(base_fc$k2) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
residuals_fc$k2 <- ts(residuals_fc$k2, frequency = 6)
colnames(residuals_fc$k2) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
test_fc$k2 <- data$k2[-c(1:54), c(1:6)]

# Quarterly
base_fc$k3 <- matrix(NA, nrow = 4, ncol = ncol(data$k3)-2)
residuals_fc$k3 <- matrix(NA, nrow = 36, ncol = ncol(data$k3)-2)
for (i in 1:6) {
  train <- data$k3[1:36, i]
  sales <- data$k3[1:36, 7]
  hvi <- data$k3[1:36, 8]
  forecast_res <- vecm_forecast_fun(train, sales, hvi, "quarter", 36, 4, 3)
  base_fc$k3[,i] <- forecast_res[[1]]
  residuals_fc$k3[,i] <- forecast_res[[2]]
}
base_fc$k3 <- ts(base_fc$k3, frequency = 4, start = c(10,1))
colnames(base_fc$k3) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
residuals_fc$k3 <- ts(residuals_fc$k3, frequency = 4)
colnames(residuals_fc$k3) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
test_fc$k3 <- data$k3[-c(1:36), c(1:6) ]

# Four-monthly
base_fc$k4 <- matrix(NA, nrow = 3, ncol = ncol(data$k4)-2)
residuals_fc$k4 <- matrix(NA, nrow = 27, ncol = ncol(data$k4)-2)
for (i in 1:6) {
  train <- data$k4[1:27, i]
  sales <- data$k4[1:27, 7]
  hvi <- data$k4[1:27, 8]
  forecast_res <- vecm_forecast_fun(train, sales, hvi, "4 months", 27, 3, 2)
  base_fc$k4[,i] <- forecast_res[[1]]
  residuals_fc$k4[,i] <- forecast_res[[2]]
}
base_fc$k4 <- ts(base_fc$k4, frequency = 3, start = c(10,1))
colnames(base_fc$k4) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
residuals_fc$k4 <- ts(residuals_fc$k4, frequency = 3)
colnames(residuals_fc$k4) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
test_fc$k4 <- data$k4[-c(1:27), c(1:6) ]


# Semi-annual
base_fc$k6 <- matrix(NA, nrow = 2, ncol = ncol(data$k6)-2)
residuals_fc$k6 <- matrix(NA, nrow = 18, ncol = ncol(data$k6)-2)
for (i in 1:6) {
  train <- data$k6[1:18, i]
  sales <- data$k6[1:18, 7]
  hvi <- data$k6[1:18, 8]
  forecast_res <- vecm_forecast_fun(train, sales, hvi, "6 months", 18, 2, 2)
  base_fc$k6[,i] <- forecast_res[[1]]
  residuals_fc$k6[,i] <- forecast_res[[2]]
}
base_fc$k6 <- ts(base_fc$k6, frequency = 2, start = c(10,1))
colnames(base_fc$k6) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
residuals_fc$k6 <- ts(residuals_fc$k6, frequency = 2)
colnames(residuals_fc$k6) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
test_fc$k6 <- data$k6[-c(1:18), c(1:6)]


# Annual
base_fc$k12 <- matrix(NA, nrow = 1, ncol = ncol(data$k12)-2)
residuals_fc$k12 <- matrix(NA, nrow = 9, ncol = ncol(data$k12)-2)
for (i in 1:6) {
  train <- data$k12[1:9, i]
  sales <- data$k12[1:9, 7]
  hvi <- data$k12[1:9, 8]
  forecast_res <- var_forecast_fun(train, sales, hvi, "year", 9, 1)
  base_fc$k12[,i] <- forecast_res[[1]]
  residuals_fc$k12[,i] <- forecast_res[[2]]
}
base_fc$k12 <- ts(base_fc$k12, frequency = 1, start = c(10,1))
colnames(base_fc$k12) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
residuals_fc$k12 <- ts(residuals_fc$k12, frequency = 1)
colnames(residuals_fc$k12) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
test_fc$k12 <- data$k12[-c(1:9), c(1:6)]


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

# Cross-sectional (contemporaneous) matrix
cs_info <- hts_tools(C = C)

ut <- cs_info$Ut

cs_info$Ut %*% FoReco_data$test

# Temporal matrix
te_info <- thf_tools(m = 12)

Zt <- te_info$Zt

FoReco_data$test %*% t(te_info$Zt)

# Reconciliation

## cross-sec
K <- c(1,2,3,4,6,12)
hts_recf_list <- NULL
for(i in 1:length(K)){
  # base forecasts
  id <- which(simplify2array(strsplit(colnames(FoReco_data$base),
                                      split = "_"))[1, ] == paste("k", K[i], sep=""))
  mbase <- t(FoReco_data$base[, id])
  # residuals
  id <- which(simplify2array(strsplit(colnames(FoReco_data$res),
                                      split = "_"))[1, ] == "k1")
  mres <- t(FoReco_data$res[, id])
  hts_recf_list[[i]] <- htsrec(mbase, C = FoReco_data$C, comb = "shr",
                               res = mres, keep = "recf")
}
names(hts_recf_list) <- paste("k", K, sep="")
hts_recf <- t(do.call(rbind, hts_recf_list[rev(names(hts_recf_list))]))
colnames(hts_recf) <- paste("k", 
                            rep(K, sapply(hts_recf_list[rev(names(hts_recf_list))], NROW)),
                            colnames(hts_recf), sep="")

# monthly base forecasts
mbase <- FoReco2matrix(FoReco_data$base, m = 12)$k1
# monthly test set
mtest <- FoReco2matrix(FoReco_data$test, m = 12)$k1
# monthly residuals
mres <- FoReco2matrix(FoReco_data$res, m = 12)$k1
# monthly reconciled forecasts
mrecf <- htsrec(mbase, C = FoReco_data$C, comb = "shr", res = mres)$recf
# score
hts_score <- score_index(recf = mrecf, base = mbase, test = mtest, nb = 4)

## temp
te_info$R

n <- NROW(FoReco_data$base)
thf_recf <- matrix(NA, n, NCOL(FoReco_data$base))
dimnames(thf_recf) <- dimnames(FoReco_data$base)
for(i in 1:n){
  # ts base forecasts ([lowest_freq' ...  highest_freq']')
  tsbase <- FoReco_data$base[i, ]
  # ts residuals ([lowest_freq' ...  highest_freq']')
  tsres <- FoReco_data$res[i, ]
  thf_recf[i,] <- thfrec(tsbase, m = 12, comb = "struc",
                         res = tsres, keep = "recf")
}

## cross-temp
oct_recf_t_struc <- octrec(FoReco_data$base, m = 12, C = FoReco_data$C,
                         comb = "t_struc", res = FoReco_data$res, keep = "recf")

oct_recf_struc <- octrec(FoReco_data$base, m = 12, C = FoReco_data$C,
                         comb = "struc", res = FoReco_data$res, keep = "recf")

discrepancy <- function(x, tol = sqrt(.Machine$double.eps)) {
  cs <- max(abs(cs_info$Ut %*% x))
  te <- max(abs(te_info$Zt %*% t(x)))
  cat("cs discrepancy:", ifelse(cs>tol, sprintf("%.8f", cs), 0),
      "\nte discrepancy:",ifelse(te>tol, sprintf("%.8f", te), 0))
}
discrepancy(oct_recf_t_struc)

# oct_score <- score_index(recf = oct_recf,
#                          base = FoReco_data$base,
#                          test = FoReco_data$test, m = 12, nb = 4, type = "rmse")
# 
# oct_score

#### 
# Heuristic first-temporal-then-cross-sectional cross-temporal reconciliation
tcs_recf <- tcsrec(FoReco_data$base, m = 12, C = FoReco_data$C,
                   thf_comb = "struc", hts_comb = "bu",
                   res = FoReco_data$res)$recf

# Iterative cross-temporal reconciliation 
ite_recf <- iterec(FoReco_data$base,
                   m = 12, C = FoReco_data$C,
                   thf_comb = "struc", hts_comb = "bu",
                   res = FoReco_data$res, start_rec = "thf")$recf
# Visualise result

data_ct <- tibble(cross_sec = as.numeric(hts_recf[1, -c(1:16)]),
                  temp = as.numeric(thf_recf[1, -c(1:16)]),
                  #cross_temp_t_struc = as.numeric(oct_recf_t_struc[1, -c(1:16)]),
                  cross_temp_struc = as.numeric(oct_recf_struc[1, -c(1:16)]),
                  tcs_recf = as.numeric(tcs_recf[1, -c(1:16)]),
                  #ite_recf = as.numeric(ite_recf[1, -c(1:16)]),
                  base = as.numeric(base[1, -c(1:16)]),
                  obs = as.numeric(obs$k1[c(109:120), 1]),
                  time = seq(from=as.POSIXct("2023-07-01 00:00:00", tz="UTC"), by="month", length.out = 12))

plot_ct <- data_ct |>
  pivot_longer(-time, names_to = "Approach") |>
  ggplot(aes(x = time, y = value, col = Approach)) +
  geom_line()+
  labs(x = NULL, y = NULL, title = "Total LTD monthly forecast vs. Observation") +
  theme_minimal() +
  theme(legend.title = element_blank())
plotly::ggplotly(plot_ct)

score_ct1 <- data_ct |>
  pivot_longer(-c(time, obs), names_to = "Approach") |>
  group_by(Approach) |>
  summarise(RMSE = sqrt(mean((value-obs)^2)))

score_ct1


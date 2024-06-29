# Load packages
library(FoReco)
library(tidyverse)
library(readxl)
library(fpp3)
library(urca)
library(plotly)

# Import UDF
source("Function/var_function.R")
source("Function/vecm_function.R")
source("Function/adjust_series_fun.R")
source("Function/test_extract_fun.R")
source("Function/select_optimal_lag.R")

# Load ltd aggregate date
ltd_agg <- read_excel("data/LTD_new.xlsx", sheet = 1) |>
  rename(Date = ...1,
         ltd = LTD,
         sales = SALES,
         hvi = HVI,
         lending = LENDING) |>
  dplyr::select(c(Date, ltd, sales, hvi, lending))

# Load ltd unit data and join with aggregate data
ltd_unit <- read_excel("data/LTD_new.xlsx", sheet = 2) |>
  rename(Date = ...1) |>
  dplyr::select(Date, ltd_total, ltd_nonres, ltd_comm, ltd_ind, ltd_other, ltd_res) |>
  # Join with aggregate data to get sales and hvi
  left_join(ltd_agg, by = c("Date")) |>
  dplyr::select(-ltd)

# Rename ltd unit data
names(ltd_unit) <- c("Date", "Total", "NonRes", "Comm", "Ind", "Other", "Res", "Sales", "hvi", "lending")

# Convert ltd unit data to tsibble object
ltd_unit <- ltd_unit %>%
  mutate(Month = yearmonth(Date)) %>%
  select(-Date) %>%
  as_tsibble(index = Month) %>%
  relocate(Month)

# Decompositions
dcmp <- ltd_unit |>
  model(stl = STL(Sales))

# Filter out sales trend
sales_trend <- components(dcmp) |>
  select(trend) |>
  relocate(Month)

# Join sales trend to ltd_unit
ltd_unit <- ltd_unit |>
  left_join(sales_trend, by = c("Month")) 

# Perform cross-validation with 8 years initially
ltd_cv <- ltd_unit |>
  stretch_tsibble(.init = 96, .step = 1)

# Number of cross-validation folds
folds <- length(unique(ltd_cv$.id))

# Define array of data
base_vecm_forecast <- hts_reconciled_vecm <- thf_reconciled_vecm <- reconciled_vecm_tcs <- test_set <- array(, dim = c(6, 12, folds))

# Add row names for the array
rownames(base_vecm_forecast) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
rownames(reconciled_vecm_tcs) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
rownames(hts_reconciled_vecm) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
rownames(thf_reconciled_vecm) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
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
  
  # Convert each folds into data in different temporal frequencies
  ## MONTHLY SERIES
  data$k1 <- adjust_series(ltd_filtered[, -1], freq = 12)
  colnames(data$k1) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res", "Sales", "hvi", "lending", "sales_trend")
  
  ## BI-MONTHLY SERIES
  data$k2 <- adjust_series(data$k1, freq = 6)
  
  ## QUARTERLY SERIES
  data$k3 <- adjust_series(data$k1, freq = 4)
  
  ## FOUR-MONTHLY SERIES
  data$k4 <- adjust_series(data$k1, freq = 3)
  
  ## SEMI-ANNUAL SERIES
  data$k6 <- adjust_series(data$k1, freq = 2)
  
  ## ANNUAL SERIES
  data$k12 <- adjust_series(data$k1, freq = 1)
  
  # FORECASTS
  ## MONTHLY FORECASTS
  base_fc$k1 <- matrix(NA, nrow = 12, ncol = ncol(data$k1)-4)
  residuals_fc$k1 <- matrix(NA, nrow = nrow(data$k1), ncol = ncol(data$k1)-4)
  
  ## VECM fitting and forecast loop over the cross-sectional level
  for (i in 1:6) {
    train <- data$k1[, i]
    sales <- data$k1[, 7]
    hvi <- data$k1[, 8]
    lending <- data$k1[, 9]
    sales_trend <- data$k1[,10]
    
    include_lending <- i %in% 2:5
    
    if (include_lending){
      forecast_res <- vecm_forecast_fun(train, sales_trend, hvi, lending, "month", nrow(data$k1), 12, include_lending)
    }
    else {
      forecast_res <- vecm_forecast_fun(train, sales, hvi, lending, "month", nrow(data$k1), 12, include_lending)
    }
    
    base_fc$k1[, i] <- forecast_res[[1]]
    residuals_fc$k1[, i] <- forecast_res[[2]]
  }
  # Convert base forecast and residuals data into ts object
  base_fc$k1 <- ts(base_fc$k1, frequency = 12)
  colnames(base_fc$k1) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
  residuals_fc$k1 <- ts(residuals_fc$k1, frequency = 12)
  colnames(residuals_fc$k1) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
  
  # BI-MONTHLY FORECASTS
  base_fc$k2 <- matrix(NA, nrow = 6, ncol = ncol(data$k2)-4)
  residuals_fc$k2 <- matrix(NA, nrow = nrow(data$k2), ncol = ncol(data$k2)-4)
  
  ## VECM fitting and forecast loop over the cross-sectional level
  for (i in 1:6) {
    train <- data$k2[, i]
    sales <- data$k2[, 7]
    hvi <- data$k2[, 8]
    lending <- data$k2[, 9]
    sales_trend <- data$k2[,10]
    
    include_lending <- i %in% 2:5
    
    if (include_lending){
      forecast_res <- vecm_forecast_fun(train, sales_trend, hvi, lending, "2 months", nrow(data$k2), 6, include_lending)
    }
    else {
      forecast_res <- vecm_forecast_fun(train, sales, hvi, lending, "2 months", nrow(data$k2), 6, include_lending)
    }
    base_fc$k2[, i] <- forecast_res[[1]]
    residuals_fc$k2[, i] <- forecast_res[[2]]
  }
  # Convert base forecast and residuals data into ts object
  base_fc$k2 <- ts(base_fc$k2, frequency = 6)
  colnames(base_fc$k2) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
  residuals_fc$k2 <- ts(residuals_fc$k2, frequency = 6)
  colnames(residuals_fc$k2) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
  
  # QUARTERLY FORECASTS
  base_fc$k3 <- matrix(NA, nrow = 4, ncol = ncol(data$k3)-4)
  residuals_fc$k3 <- matrix(NA, nrow = nrow(data$k3), ncol = ncol(data$k3)-4)
  
  ## VECM fitting and forecast loop over the cross-sectional level
  for (i in 1:6) {
    train <- data$k3[, i]
    sales <- data$k3[, 7]
    hvi <- data$k3[, 8]
    lending <- data$k3[, 9]
    sales_trend <- data$k3[,10]
    
    include_lending <- i %in% 2:5
    
    if (include_lending){
      forecast_res <- vecm_forecast_fun(train, sales_trend, hvi, lending, "quarter", nrow(data$k3), 4, include_lending)
    }
    else {
      forecast_res <- vecm_forecast_fun(train, sales, hvi, lending, "quarter", nrow(data$k3), 4, include_lending)
    }
    
    base_fc$k3[,i] <- forecast_res[[1]]
    residuals_fc$k3[,i] <- forecast_res[[2]]
  }
  # Convert base forecast and residuals data into ts object
  base_fc$k3 <- ts(base_fc$k3, frequency = 4)
  colnames(base_fc$k3) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
  residuals_fc$k3 <- ts(residuals_fc$k3, frequency = 4)
  colnames(residuals_fc$k3) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
  
  # FOUR_MONTHLY FORECASTS
  base_fc$k4 <- matrix(NA, nrow = 3, ncol = ncol(data$k4)-4)
  residuals_fc$k4 <- matrix(NA, nrow = nrow(data$k4), ncol = ncol(data$k4)-4)
  
  ## VECM fitting and forecast loop over the cross-sectional level
  for (i in 1:6) {
    train <- data$k4[, i]
    sales <- data$k4[, 7]
    hvi <- data$k4[, 8]
    lending <- data$k4[, 9]
    sales_trend <- data$k4[,10]
    
    include_lending <- i %in% 2:5
    
    if (include_lending){
      forecast_res <- vecm_forecast_fun(train, sales_trend, hvi, lending, "4 months", nrow(data$k4), 3, include_lending)
    }
    else {
      forecast_res <- vecm_forecast_fun(train, sales, hvi, lending, "4 months", nrow(data$k4), 3, include_lending)
    }
    base_fc$k4[,i] <- forecast_res[[1]]
    residuals_fc$k4[,i] <- forecast_res[[2]]
  }
  
  # Convert base forecast and residuals data into ts object
  base_fc$k4 <- ts(base_fc$k4, frequency = 3)
  colnames(base_fc$k4) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
  residuals_fc$k4 <- ts(residuals_fc$k4, frequency = 3)
  colnames(residuals_fc$k4) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
  
  
  # SEMI_ANNUALLY FORECASTS
  base_fc$k6 <- matrix(NA, nrow = 2, ncol = ncol(data$k6)-4)
  residuals_fc$k6 <- matrix(NA, nrow = nrow(data$k6), ncol = ncol(data$k6)-4)
  for (i in 1:6) {
    train <- data$k6[, i]
    sales <- data$k6[, 7]
    hvi <- data$k6[, 8]
    lending <- data$k6[, 9]
    sales_trend <- data$k6[,10]
    
    include_lending <- i %in% 2:5
    
    if (include_lending){
      forecast_res <- vecm_forecast_fun(train, sales_trend, hvi, lending, "6 months", nrow(data$k6), 2, include_lending)
    }
    else {
      forecast_res <- vecm_forecast_fun(train, sales, hvi, lending, "6 months", nrow(data$k6), 2, include_lending)
    }
    base_fc$k6[,i] <- forecast_res[[1]]
    residuals_fc$k6[,i] <- forecast_res[[2]]
  }
  
  # Convert base forecast and residuals data into ts object
  base_fc$k6 <- ts(base_fc$k6, frequency = 2)
  colnames(base_fc$k6) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
  residuals_fc$k6 <- ts(residuals_fc$k6, frequency = 2)
  colnames(residuals_fc$k6) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
  
  # ANNUALLY FORECASTS
  base_fc$k12 <- matrix(NA, nrow = 1, ncol = ncol(data$k12)-4)
  residuals_fc$k12 <- matrix(NA, nrow = nrow(data$k12), ncol = ncol(data$k12)-4)
  for (i in 1:6) {
    train <- data$k12[, i]
    sales <- data$k12[, 7]
    hvi <- data$k12[, 8]
    lending <- data$k12[, 9]
    sales_trend <- data$k12[,10]
    
    include_lending <- i %in% 2:5
    
    # Fit VAR
    if (include_lending){
      forecast_res <- var_forecast_fun(train, sales_trend, hvi, lending, "year", nrow(data$k12), 1, include_lending)
    }
    else {
      forecast_res <- var_forecast_fun(train, sales, hvi, lending, "year", nrow(data$k12), 1, include_lending)
    }
    base_fc$k12[,i] <- forecast_res[[1]]
    residuals_fc$k12[,i] <- forecast_res[[2]]
  }
  base_fc$k12 <- ts(base_fc$k12, frequency = 1)
  colnames(base_fc$k12) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
  residuals_fc$k12 <- ts(residuals_fc$k12, frequency = 1)
  colnames(residuals_fc$k12) <- c("Total", "NonRes", "Comm", "Ind", "Other", "Res")
  
  ## Pre-process base forecast and residuals matrices into appropriate format for reconciliation
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
  
  ## Cross-sectionally reconciliation
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
    hts_recf_list[[h]] <- htsrec(mbase, C = FoReco_data$C, comb = "shr",
                                 res = mres, keep = "recf")
  }
  names(hts_recf_list) <- paste("k", K, sep="")
  hts_recf <- t(do.call(rbind, hts_recf_list[rev(names(hts_recf_list))]))
  colnames(hts_recf) <- paste("k", 
                              rep(K, sapply(hts_recf_list[rev(names(hts_recf_list))], NROW)),
                              colnames(hts_recf), sep="")
  
  ## Temporally reconciliation
  n <- NROW(FoReco_data$base)
  thf_recf <- matrix(NA, n, NCOL(FoReco_data$base))
  dimnames(thf_recf) <- dimnames(FoReco_data$base)
  for(l in 1:n){
    # ts base forecasts ([lowest_freq' ...  highest_freq']')
    tsbase <- FoReco_data$base[l, ]
    # ts residuals ([lowest_freq' ...  highest_freq']')
    tsres <- FoReco_data$res[l, ]
    thf_recf[l,] <- thfrec(tsbase, m = 12, comb = "wlsv",
                           res = tsres, keep = "recf")
  }
  
  ## Cross-temporally reconciliation
  ### Heuristic first-temporal-then-cross-sectional cross-temporal reconciliation
  tcs_recf <- tcsrec(FoReco_data$base, m = 12, C = FoReco_data$C,
                     thf_comb = "wlsv", hts_comb = "shr",
                     res = FoReco_data$res)$recf
  
  discrepancy <- function(x, tol = sqrt(.Machine$double.eps)) {
    cs <- max(abs(cs_info$Ut %*% x))
    te <- max(abs(te_info$Zt %*% t(x)))
    cat("cs discrepancy:", ifelse(cs>tol, sprintf("%.8f", cs), 0),
        "\nte discrepancy:",ifelse(te>tol, sprintf("%.8f", te), 0))
  }
  discrepancy(tcs_recf)
  
  # Extract and assigned forecasts into pre-defined array
  for (j in 1:nrow(base)){
    base_vecm_forecast[j, ,t] <- t(as.matrix(base[j, -c(1:16)]))
    hts_reconciled_vecm[j, , t] <- t(as.matrix(hts_recf[j, -c(1:16)]))
    reconciled_vecm_tcs[j, , t] <- t(as.matrix(tcs_recf[j, -c(1:16)]))
    thf_reconciled_vecm[j, , t] <- t(as.matrix(thf_recf[j, -c(1:16)]))
  }
}

# Loop through each .id to create forecasts dataframe for each folds
df <- NULL
for (i in 1:folds) {
  # Calculate the starting date for the current .id
  start_date <- as.POSIXct("2021-07-01", tz="UTC") + months(i - 1)
  
  # Extract Total values for each .id and each array
  vecm_base <- base_vecm_forecast["Total", , i]            # base forecasts
  vecm_hts_reconciled <- hts_reconciled_vecm["Total", , i] # cross-sectionally reconciled forecasts
  vecm_tcs_reconciled <- reconciled_vecm_tcs["Total", , i] # temporally reconciled forecasts
  vecm_thf_reconciled <- thf_reconciled_vecm["Total", , i] # cross-temporally reconciled forecasts
  total_observed <- test_set["Total", , i]                 # True values or observations
  
  # Create a dataframe for the current .id
  df1 <- data.frame(
    Date = seq(from=start_date, by="month", length.out = 12),
    vecm_base = as.numeric(vecm_base),
    vecm_hts_reconciled = as.numeric(vecm_hts_reconciled),
    vecm_tcs_reconciled = as.numeric(vecm_tcs_reconciled),
    vecm_thf_reconciled = as.numeric(vecm_thf_reconciled),
    observations = as.numeric(total_observed),
    id = as.factor(i)
  )
  
  df <- rbind(df, df1)
  
  # Convert Yearmonth to a Date object
  df$Date <- as.Date(df$Date)
}

# Plotting forecasts for one fold
## Initialise the fold number we want to plot
fold_num = 25

## Plotting
plot_ct <- df |>
  filter(id == fold_num) |>
  select(-id) |>
  pivot_longer(-Date, names_to = "Approach") |>
  ggplot(aes(x = Date, y = value, col = Approach)) +
  geom_line()+
  labs(x = NULL, y = NULL, title = "Total LTD monthly forecast vs. Observation") +
  theme_minimal() +
  theme(legend.title = element_blank())
plotly::ggplotly(plot_ct)

# RMSE and MAPE of all forecasts against actual observations for a fold
score_ct1 <- df |>
  filter(id == fold_num) |>
  select(-id) |>
  pivot_longer(-c(Date, observations), names_to = "Approach") |>
  group_by(Approach) |>
  summarise(
    RMSE = sqrt(mean((value - observations)^2, na.rm = TRUE)),
    MAPE = mean(abs((value - observations) / observations) * 100, na.rm = TRUE)
  )

score_ct1

# save(base_vecm_forecast, file = "data/base_vecm_fc.RData")
# save(reconciled_vecm_tcs, file = "data/rec_vecm.RData")
# save(thf_reconciled_vecm, file = "data/temp_rec_vecm.RData")
# save(hts_reconciled_vecm, file = "data/cross_sec_rec_vecm.RData")
# save(test_set, file = "data/test_set.RData")
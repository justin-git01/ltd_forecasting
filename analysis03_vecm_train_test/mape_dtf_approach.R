# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)

# Load ltd unit data
ltd_dtf <- read_excel(here::here("data/ltd_d11.xlsx")) %>%
  rename(Date = `_date_`)

# Create a data frame to store MAPE values for each fold
fold_mape <- data.frame(matrix(ncol = 11, nrow = 12))
colnames(fold_mape) <- c("h", paste0("fold", 1:10))
fold_mape$h <- 1:12

# Function to compute MAPE
compute_mape <- function(actual, forecast) {
  return(mean(abs((actual - forecast) / actual)) * 100)
}

# Manually computed MAPE for fold 1
fold1 <- ltd_dtf %>%
  select(c(Date, ltd_d11, ltd_d11_f1)) %>%
  filter(Date >= ymd("2022-06-01")) %>%
  filter(Date <= ymd("2023-05-01"))

fold_mape$fold1 <- abs((fold1$ltd_d11-fold1$ltd_d11_f1)/fold1$ltd_d11)*100

# Manually computed MAPE for fold 2
fold2 <- ltd_dtf %>%
  select(c(Date, ltd_d11, ltd_d11_f2)) %>%
  filter(Date >= ymd("2022-07-01")) %>%
  filter(Date <= ymd("2023-06-01"))

fold_mape$fold2 <- abs((fold2$ltd_d11-fold2$ltd_d11_f2)/fold2$ltd_d11)*100

# Manually computed MAPE for fold 3
fold3 <- ltd_dtf %>%
  select(c(Date, ltd_d11, ltd_d11_f3)) %>%
  filter(Date >= ymd("2022-08-01")) %>%
  filter(Date <= ymd("2023-07-01"))

fold_mape$fold3 <- abs((fold3$ltd_d11-fold3$ltd_d11_f3)/fold3$ltd_d11)*100

# Manually computed MAPE for fold 4
fold4 <- ltd_dtf %>%
  select(c(Date, ltd_d11, ltd_d11_f4)) %>%
  filter(Date >= ymd("2022-09-01")) %>%
  filter(Date <= ymd("2023-08-01"))

fold_mape$fold4 <- abs((fold4$ltd_d11-fold4$ltd_d11_f4)/fold4$ltd_d11)*100

# Manually computed MAPE for fold 5
fold5 <- ltd_dtf %>%
  select(c(Date, ltd_d11, ltd_d11_f5)) %>%
  filter(Date >= ymd("2022-10-01")) %>%
  filter(Date <= ymd("2023-09-01"))

fold_mape$fold5 <- abs((fold5$ltd_d11-fold5$ltd_d11_f5)/fold5$ltd_d11)*100

# Manually computed MAPE for fold 6
fold6 <- ltd_dtf %>%
  select(c(Date, ltd_d11, ltd_d11_f6)) %>%
  filter(Date >= ymd("2022-11-01")) %>%
  filter(Date <= ymd("2023-10-01"))

fold_mape$fold6 <- abs((fold6$ltd_d11-fold6$ltd_d11_f6)/fold6$ltd_d11)*100

# Manually computed MAPE for fold 7
fold7 <- ltd_dtf %>%
  select(c(Date, ltd_d11, ltd_d11_f7)) %>%
  filter(Date >= ymd("2022-12-01")) %>%
  filter(Date <= ymd("2023-11-01"))

fold_mape$fold7 <- abs((fold7$ltd_d11-fold7$ltd_d11_f7)/fold7$ltd_d11)*100

# Manually computed MAPE for fold 8
fold8 <- ltd_dtf %>%
  select(c(Date, ltd_d11, ltd_d11_f8)) %>%
  filter(Date >= ymd("2023-01-01")) %>%
  filter(Date <= ymd("2023-12-01"))

fold_mape$fold8 <- abs((fold8$ltd_d11-fold8$ltd_d11_f8)/fold8$ltd_d11)*100

# Manually computed MAPE for fold 9
fold9 <- ltd_dtf %>%
  select(c(Date, ltd_d11, ltd_d11_f9)) %>%
  filter(Date >= ymd("2023-02-01")) %>%
  filter(Date <= ymd("2024-01-01"))

fold_mape$fold9 <- abs((fold9$ltd_d11-fold9$ltd_d11_f9)/fold9$ltd_d11)*100

# Manually computed MAPE for fold 10
fold10 <- ltd_dtf %>%
  select(c(Date, ltd_d11, ltd_d11_f10)) %>%
  filter(Date >= ymd("2023-03-01")) %>%
  filter(Date <= ymd("2024-02-01"))

fold_mape$fold10 <- abs((fold10$ltd_d11-fold10$ltd_d11_f10)/fold10$ltd_d11)*100

# Compute the average MAPE across all folds
fold_mape$average_mape <- rowMeans(fold_mape[, 2:11])

# Save the MAPE results
save(fold_mape, file = "data/dtf_mape.RData")

library(readxl)
library(dplyr)
library(lubridate)

# Load ltd unit data
ltd_dtf <- read_excel(here::here("data/ltd_d11.xlsx")) |>
  rename(Date = `_date_`) 


fold_rmse <- data.frame(matrix(ncol = 11, nrow = 12))
colnames(fold_rmse) <- c("h", paste0("fold", 1:10))
fold_rmse$h <- 1:12

# Manually computed RMSE for fold 1
fold1 <- ltd_dtf %>%
  select(c(Date, ltd_d11, ltd_d11_f1)) %>%
  filter(Date >= ymd("2022-06-01")) %>%
  filter(Date <= ymd("2023-05-01"))

fold_rmse$fold1 <- (fold1$ltd_d11_f1 - fold1$ltd_d11)^2

# Manually computed RMSE for fold 2
fold2 <- ltd_dtf %>%
  select(c(Date, ltd_d11, ltd_d11_f2)) %>%
  filter(Date >= ymd("2022-07-01")) %>%
  filter(Date <= ymd("2023-06-01"))

fold_rmse$fold2 <- (fold2$ltd_d11 - fold2$ltd_d11_f2)^2

# Manually computed RMSE for fold 3
fold3 <- ltd_dtf %>%
  select(c(Date, ltd_d11, ltd_d11_f3)) %>%
  filter(Date >= ymd("2022-08-01")) %>%
  filter(Date <= ymd("2023-07-01"))

fold_rmse$fold3 <- (fold3$ltd_d11 - fold3$ltd_d11_f3)^2

# Manually computed RMSE for fold 4
fold4 <- ltd_dtf %>%
  select(c(Date, ltd_d11, ltd_d11_f4)) %>%
  filter(Date >= ymd("2022-09-01")) %>%
  filter(Date <= ymd("2023-08-01"))

fold_rmse$fold4 <- (fold4$ltd_d11 - fold4$ltd_d11_f4)^2

# Manually computed RMSE for fold 5
fold5 <- ltd_dtf %>%
  select(c(Date, ltd_d11, ltd_d11_f5)) %>%
  filter(Date >= ymd("2022-10-01")) %>%
  filter(Date <= ymd("2023-09-01"))

fold_rmse$fold5 <- (fold5$ltd_d11 - fold5$ltd_d11_f5)^2

# Manually computed RMSE for fold 6
fold6 <- ltd_dtf %>%
  select(c(Date, ltd_d11, ltd_d11_f6)) %>%
  filter(Date >= ymd("2022-11-01")) %>%
  filter(Date <= ymd("2023-10-01"))

fold_rmse$fold6 <- (fold6$ltd_d11 - fold6$ltd_d11_f6)^2

# Manually computed RMSE for fold 7
fold7 <- ltd_dtf %>%
  select(c(Date, ltd_d11, ltd_d11_f7)) %>%
  filter(Date >= ymd("2022-12-01")) %>%
  filter(Date <= ymd("2023-11-01"))

fold_rmse$fold7 <- (fold7$ltd_d11 - fold7$ltd_d11_f7)^2

# Manually computed RMSE for fold 8
fold8 <- ltd_dtf %>%
  select(c(Date, ltd_d11, ltd_d11_f8)) %>%
  filter(Date >= ymd("2023-01-01")) %>%
  filter(Date <= ymd("2023-12-01"))

fold_rmse$fold8 <- (fold8$ltd_d11 - fold8$ltd_d11_f8)^2

# Manually computed RMSE for fold 9
fold9 <- ltd_dtf %>%
  select(c(Date, ltd_d11, ltd_d11_f9)) %>%
  filter(Date >= ymd("2023-02-01")) %>%
  filter(Date <= ymd("2024-01-01"))

fold_rmse$fold9 <- (fold9$ltd_d11 - fold9$ltd_d11_f9)^2

# Manually computed RMSE for fold 10
fold10 <- ltd_dtf %>%
  select(c(Date, ltd_d11, ltd_d11_f10)) %>%
  filter(Date >= ymd("2023-03-01")) %>%
  filter(Date <= ymd("2024-02-01"))

fold_rmse$fold10 <- (fold10$ltd_d11 - fold10$ltd_d11_f10)^2


fold_rmse$average_rmse <- sqrt(rowMeans(fold_rmse[, 2:11]))

save(fold_rmse, file = "data/dtf_rmse.RData")

###VECM 
train <- data$k6[1:18, 1]
sales <- data$k6[1:18, 7]
hvi <- data$k6[1:18, 8]

dat <- data.frame(cbind(train, sales, hvi))
start_date <- as.Date("2013-07-01")
end_date <- seq.Date(start_date, by = "6 months", length.out = 18)
dat$Date <- end_date

dat <- dat %>%
  mutate(Month = yearmonth(Date)) |>
  dplyr::select(-Date)  |>
  as_tsibble(index = Month) |>
  relocate(Month)

dat1 <- ts(dat[,-1], frequency = 2)

dat1

# Johansen cointegration test to determine the rank
coint_test <- ca.jo(dat1, spec = "longrun", K = 1)  

qsummary(coint_test)
# Fit the VECM model using the rank from Johansen test
vecm_fit <- vars::vec2var(coint_test, r = 1) 

res <- residuals(vecm_fit)
res1 <- c(mean(res, na.rm = TRUE), mean(res, na.rm = TRUE), as.numeric(residuals(vecm_fit)[,1]))

# Forecast
forecast_res <- as.numeric(predict(vecm_fit, n.ahead = 2)$fcst$train[,1])
 
###VECM 
dat <- data.frame(train = data$k1[1:108, 1],
                  sales = data$k1[1:108, 7],
                  hvi = data$k1[1:108, 8],
                  Date = seq(as.Date("2013-07-01"), by = "month", length.out = 108))

dat_tsibble <- dat %>%
  mutate(Month = yearmonth(Date)) %>%
  select(-Date) %>%
  as_tsibble(index = Month) %>%
  relocate(Month)

#dat1 <- ts(dat_cv[,-1] %>% filter(.id == 1) %>% select(-.id), frequency = 12)
dat1 <- ts(dat_tsibble[,-1], frequency = 12)

# Johansen cointegration test to determine the rank
library(urca)
coint_test <- ca.jo(dat1, spec = "longrun", K = 2)  

# Fit the VECM model using the rank from Johansen test
vecm_fit <- vars::vec2var(coint_test, r = 2) 

# Access test statistics and critical values
test_stats <- coint_test@teststat
critical_values_5pct <- coint_test@cval[, "5pct"]

# Print the test statistics and critical values
print(test_stats)
print(critical_values_5pct)

# Determine the rank (number of cointegrating relations)
# Hypothesis r = 0, r <= 1, r <= 2, etc., where r is the rank.
rank <- sum(test_stats > coint_test@cval[, "5pct"])
print(paste("Estimated number of cointegrating vectors:", rank))



res <- residuals(vecm_fit)
res1 <- c(mean(res, na.rm = TRUE), mean(res, na.rm = TRUE), as.numeric(residuals(vecm_fit)[,1]))

# Forecast
forecast_res <- as.numeric(predict(vecm_fit, n.ahead = 12)$fcst$train[,1])
 
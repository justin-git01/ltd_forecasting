library(fable)
library(fabletools)
library(tsibble)
library(feasts)
library(dplyr)
library(urca)  

dat <- data.frame(train = data$k1[1:108, 1],
                  sales = data$k1[1:108, 7],
                  hvi = data$k1[1:108, 8],
                  Date = seq(as.Date("2013-07-01"), by = "month", length.out = 108))

dat_tsibble <- dat %>%
  mutate(Month = yearmonth(Date)) %>%
  select(-Date) %>%
  as_tsibble(index = Month) %>%
  relocate(Month)



dat_cv <- dat_tsibble |>
  stretch_tsibble(.init = 60, .step = 1)

fit_models <- function(data_slice) {
  # Fit each model
  models <- model(
    data_slice,
    VAR = VAR(vars(train, sales, hvi)),  # You might adjust based on your `forecast_fun1`
    VECM = { # Fitting VECM model might require the Johansen test results to specify ranks
      jtest <- ca.jo(ts(data_slice, frequency = 12), spec = "longrun", K = 2)
      r <- sum(test_stats > jtest@cval[, "5pct"])  # Example: Using r=1
      vars::vec2var(jtest, r = r)
    },
    ARIMA = ARIMA(train),
    ETS = ETS(train)
  )
  
  # Forecast using the fitted models
  forecasts <- models %>%
    forecast(h = "1 year")
  
  # Calculate accuracy metrics
  accuracy_metrics <- accuracy(forecasts, data_slice)
  return(accuracy_metrics)
}

# Apply the function over each slice of the data
results <- dat_cv %>%
  group_by(.id) %>%
  summarise(accuracy_metrics = fit_models(.), .groups = "drop")

results_summary <- results %>%
  group_by(model) %>%
  summarise(across(c(RMSE, MAPE, MASE), mean, na.rm = TRUE))



forecast_fun1 <- function(dat, period){
  if (period %in% c("6 months", "year")){
    model(dat, VAR = VAR(vars(train, sales, hvi) ~ AR(p = 1)))
  } else {
    model(dat, VAR = VAR(vars(train, sales, hvi)))
  }
}


base_fc$k1 <- matrix(NA, nrow = 12, ncol = ncol(data$k1)-2)
residuals_fc$k1 <- matrix(NA, nrow = 108, ncol = ncol(data$k1)-2)
for (i in 1:6) {
  train <- data$k1[1:108, i]
  sales <- data$k1[1:108, 7]
  hvi <- data$k1[1:108, 8]
  forecast_res <- forecast_fun(train, sales, hvi, "month", 108, 12)
  base_fc$k1[, i] <- forecast_res[[1]]
  residuals_fc$k1[, i] <- forecast_res[[2]]
}



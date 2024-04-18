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
      jtest <- ca.jo(ts(data_slice[,-1] %>% select(-.id), frequency = 12), spec = "longrun", K = 2)
      r <- sum(jtest@teststat > jtest@cval[, "5pct"])  # Example: Using r=1
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



############

library(dplyr)
library(dplyr)
library(tsibble)
library(fable)
library(vars)
library(forecast)
library(urca)

fit_models <- function(data_slice) {
  # Assuming data_slice is already a tsibble
  # Fit models using the fable framework
  models <- data_slice %>%
    model(
      VAR = VAR(vars(train, sales, hvi)),
      VECM = {
        jtest <- ca.jo(as.ts(data_slice %>% select(train, sales, hvi)), spec = "longrun", K = 2)
        r <- sum(jtest@teststat > jtest@cval[, "5pct"])
        vec2var(jtest, r = r)
      },
      ARIMA = ARIMA(train),
      ETS = ETS(train)
    )
  
  # Forecast using the fitted models
  forecasts <- models %>%
    forecast(h = "12 months")  # specifying 12 months explicitly
  
  # Calculate accuracy metrics
  accuracy_metrics <- accuracy(forecasts, data_slice)
  
  return(accuracy_metrics)
}

# Apply the function over each slice of the data
results <- dat_cv %>%
  group_by(.id) %>%
  summarise(accuracy_metrics = list(fit_models(cur_data())), .groups = "drop") %>%
  unnest(cols = c(accuracy_metrics))

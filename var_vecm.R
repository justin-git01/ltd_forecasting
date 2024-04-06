library(FoReco)
library(thief)
library(tidyverse)
library(readxl)
library(vars)  

ltd_agg <- read_excel("data/LTD data aggregate.xlsx") %>%
  mutate(log_ltd = log(ltd),
         log_sales = log(sales),
         log_hvi = log(hvi)) %>%
  dplyr::select(c(log_ltd, log_sales, log_hvi))

D_ltd <- diff(as.numeric(unlist(ltd_agg |> dplyr::select(log_ltd))))
D_sales <- diff(as.numeric(unlist(ltd_agg |> dplyr::select(log_sales))))
D_hvi <- diff(as.numeric(unlist(ltd_agg |> dplyr::select(log_hvi))))
varmat <- as.matrix(cbind(D_ltd,D_sales, D_hvi))
varfit <- VAR(varmat) # `VAR()` from package `vars`
summary(varfit)

# Get the forecasts from the VECM model for ltd
forecast_var <- predict(varfit, n.ahead = 12) # Adjust the number of periods as needed

# Extract the forecasts for ltd
forecast_var$fcst$D_ltd[,1]

# Combine the VECM forecast for ltd with your existing forecast data structure (e.g., 'base_fc')
# Assuming 'base_fc' is your existing forecast data structure

base_fc$k1 <- cbind(base_fc$k1, vecm_forecasts_ltd)

# Continue with the rest of your code...


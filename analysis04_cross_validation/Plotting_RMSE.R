library(dplyr)
library(plotly)
library(ggplot2)

# Plotting RMSE of base forecast to choose the benchmark 
## Node index (1: Total, 2: Non-residential, 3: Commercial, 4: Industrial, 5: Other, 5: Residential)
i = 1
## Create a data frame for plotting
rmse_base_data <- data.frame(
  h_step = 1:12,
  RMSE_ARIMA_base_total = RMSE_arima_h_base[i,],
  RMSE_VECM_base_total = RMSE_vecm_h_base[i,]
)

## Convert to long format
rmse_data_long <- rmse_base_data %>%
  pivot_longer(cols = -h_step, names_to = "Method", values_to = "RMSE")


## Create the plot
ggplot_rmse <- ggplot(rmse_data_long, aes(x = h_step, y = RMSE, colour = Method)) +
  geom_line(linewidth = 1.1) +
  labs(x = "h-step Forecast", y = "RMSE Value", title = "Base Total forecast RMSE by Method") +
  scale_x_continuous(breaks = 1:12) +
  theme_minimal()

## Convert ggplot to plotly
ggplotly(ggplot_rmse)


# Plotting all reconciled forecasts proportion against vecm based forecast for comparison
prop_arima_vs_vecm_base <- (RMSE_arima_h_rec / RMSE_vecm_h_base)[i,]

# Create a data frame for plotting
prop_data <- data.frame(
  h_step = 1:12,
  Structural = RMSE_vecm_struc_prop[i,],
  TCS = RMSE_vecm_tcs_prop[i,],
  ARIMA_vs_VECM_Base = prop_arima_vs_vecm_base
)

# Reshape for ggplot
prop_data_long <- prop_data %>%
  pivot_longer(cols = -h_step, names_to = "Method", values_to = "Proportion")

# Create the plot
ggplot_prop <- ggplot(prop_data_long, aes(x = h_step, y = Proportion, colour = Method)) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = 1) +
  labs(x = "H-step Forecast", y = "Proportion (Reconciled/Base Forecast)", title = "Proportion of Reconciled Forecast to Base VECM Forecast") +
  scale_x_continuous(breaks = 1:12) +
  theme_minimal()

# Display the plot
ggplotly(ggplot_prop)

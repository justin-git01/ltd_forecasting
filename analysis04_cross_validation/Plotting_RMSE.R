# Load necessary library
library(dplyr)
library(plotly)
library(ggplot2)

# Create a data frame for plotting
rmse_base_data <- data.frame(
  h_step = 1:12,
  RMSE_ARIMA = RMSE_arima_h_base[1,],
  RMSE_VECM = RMSE_vecm_h_base[1,]
)

# Reshape for ggplot
rmse_data_long <- rmse_base_data %>%
  pivot_longer(cols = -h_step, names_to = "Method", values_to = "RMSE")


# Create the plot
ggplot_rmse <- ggplot(rmse_data_long, aes(x = h_step, y = RMSE, colour = Method)) +
  geom_line(linewidth = 1.2) +
  labs(x = "H-step Forecast", y = "RMSE Value", title = "Forecast RMSE by Method") +
  theme_minimal()

# Convert ggplot to plotly
ggplotly(ggplot_rmse)


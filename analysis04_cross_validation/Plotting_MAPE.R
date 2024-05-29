library(dplyr)
library(plotly)
library(ggplot2)

load(here::here("data/dtf_mape.RData"))


# Plotting RMSE of base forecast to choose the benchmark 
## Node index (1: Total, 2: Non-residential, 3: Commercial, 4: Industrial, 5: Other, 5: Residential)
i = 1
## Create a data frame for plotting
mape_base_data <- data.frame(
  h_step = 1:12,
  mape_arima_base_total = mape_arima_base[i,],
  mape_vecm_base_total = mape_vecm_base[i,]
)

## Convert to long format
mape_data_long <- mape_base_data %>%
  pivot_longer(cols = -h_step, names_to = "Method", values_to = "MAPE")


## Create the plot
ggplot_mape <- ggplot(mape_data_long, aes(x = h_step, y = MAPE, colour = Method)) +
  geom_line(linewidth = 1.1) +
  labs(x = "h-step Forecast", y = "MAPE Value", title = "Base Total forecast MAPE by Method") +
  scale_x_continuous(breaks = 1:12) +
  theme_minimal()

## Convert ggplot to plotly
ggplotly(ggplot_mape)

#######################################

# Plotting RMSE of base forecast to choose the benchmark 
## Node index (1: Total, 2: Non-residential, 3: Commercial, 4: Industrial, 5: Other, 5: Residential)
i = 1
## Create a data frame for plotting
mape_base_data <- data.frame(
  h_step = 1:12,
  mape_arima_base = mape_arima_base[i,],
  mape_vecm_base = mape_vecm_base[i,],
  mape_arima_rec = mape_arima_cross_temp[i,],
  mape_vecm_rec = mape_vecm_cross_temp[i,],
  mape_DTF = fold_mape$average_mape
)

## Convert to long format
mape_data_long <- mape_base_data %>% 
  pivot_longer(cols = -h_step, names_to = "Method", values_to = "MAPE")

## Add columns for line type and method
mape_data_long <- mape_data_long %>%
  mutate(
    LineType = ifelse(grepl("rec", Method), "Reconciled", "Base"),
    MethodGroup = case_when(
      grepl("arima", Method) ~ "ARIMA",
      grepl("vecm", Method) ~ "VECM",
      Method == "mape_DTF" ~ "DTF"
    )
  )

## Create the plot with specified colors and line types
ggplot_mape <- ggplot(mape_data_long, aes(x = h_step, y = MAPE, colour = MethodGroup, linetype = LineType)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = c(
    "ARIMA" = "blue",
    "VECM" = "red",
    "DTF" = "green"
  )) +
  scale_linetype_manual(values = c(
    "Base" = "solid",
    "Reconciled" = "dashed"
  )) +
  labs(x = "h-step Forecast", y = "MAPE Value", title = "Monthly total LTD base and reconciled forecast MAPE by Method") +
  scale_x_continuous(breaks = 1:12) +
  theme_minimal() +
  guides(
    colour = guide_legend(title = "Colour: Model", override.aes = list(linetype = "solid")),
    linetype = guide_legend(title = "Line type: Reconciled/Base")
  )

## Convert ggplot to plotly
ggplotly(ggplot_mape)

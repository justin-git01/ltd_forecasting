library(patchwork)

testing <- tibble(base = exp(as.numeric(base_fc$k1[,1])),
                  base_vecm = exp(forecast_res),
                  obs = exp(as.numeric(test_fc$k1[,1])),
                  time = seq(from=as.Date("2013-07-01"), by="month", length.out = 12))

plot_test <- testing |>
  pivot_longer(-time, names_to = "Approach") |>
  ggplot(aes(x = time, y = value, col = Approach)) +
  geom_line()+
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.title = element_blank())
plot <- plotly::ggplotly(plot_test)

# 2months
testing <- tibble(base = exp(as.numeric(base_fc$k2[,1])),
                  obs = exp(as.numeric(test_fc$k2[,1])),
                  time = seq(from=as.Date("2013-07-01"), by="2 months", length.out = 6))

plot_test <- testing |>
  pivot_longer(-time, names_to = "Approach") |>
  ggplot(aes(x = time, y = value, col = Approach)) +
  geom_line()+
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.title = element_blank())

plot2 <- plotly::ggplotly(plot_test)

# quarter
testing <- tibble(base = exp(as.numeric(base_fc$k3[,1])),
                  obs = exp(as.numeric(test_fc$k3[,1])),
                  time = seq(from=as.Date("2013-07-01"), by="quarter", length.out = 4))

plot_test <- testing |>
  pivot_longer(-time, names_to = "Approach") |>
  ggplot(aes(x = time, y = value, col = Approach)) +
  geom_line()+
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.title = element_blank())

plot3 <- plotly::ggplotly(plot_test)

# 4months
testing <- tibble(base = exp(as.numeric(base_fc$k4[,1])),
                  obs = exp(as.numeric(test_fc$k4[,1])),
                  time = seq(from=as.Date("2013-07-01"), by="4 months", length.out = 3))

plot_test <- testing |>
  pivot_longer(-time, names_to = "Approach") |>
  ggplot(aes(x = time, y = value, col = Approach)) +
  geom_line()+
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.title = element_blank())

plot4 <- plotly::ggplotly(plot_test)

# semi-annual
testing <- tibble(base = exp(as.numeric(base_fc$k6[,1])),
                  base_vecm = exp(forecast_res),
                  obs = exp(as.numeric(test_fc$k6[,1])),
                  time = seq(from=as.Date("2013-07-01"), by="6 months", length.out = 2))

plot_test <- testing |>
  pivot_longer(-time, names_to = "Approach") |>
  ggplot(aes(x = time, y = value, col = Approach)) +
  geom_line()+
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.title = element_blank())

plot6 <- plotly::ggplotly(plot_test)

# annual
testing <- tibble(base = exp(as.numeric(base_fc$k12[,1])),
                  obs = exp(as.numeric(test_fc$k12[1])),
                  time = seq(from=as.Date("2013-07-01"), by="year", length.out = 1))

plot_test <- testing |>
  pivot_longer(-time, names_to = "Approach") |>
  ggplot(aes(x = time, y = value, col = Approach)) +
  geom_point()+
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(legend.title = element_blank())

plot12 <- plotly::ggplotly(plot_test)

# Combine plots using subplot
library(plotly)
combined_plot <- subplot(plot, plot2, plot3, plot4, plot6,
                         plot12, nrows = 3, shareX = TRUE, titleX = FALSE)

# Display the combined plot
combined_plot

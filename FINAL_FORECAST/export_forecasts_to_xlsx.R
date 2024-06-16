# Load necessary libraries
library(openxlsx)
library(lubridate)

# Create a workbook
wb <- createWorkbook()

# Define the number of forecasts
num_forecasts <- 12

# Initialize empty lists to store forecast data
hts_reconciled_forecasts <- list()
thf_reconciled_forecasts <- list()
tcs_reconciled_forecasts <- list()

# Loop for Cross-temporally reconciled forecasts
for (i in 1:dim(hts_reconciled_vecm)[3]) {
  start_date <- as.Date("2021-07-01") %m+% months(i - 1)
  forecasts <- hts_reconciled_vecm[1, , i]
  hts_reconciled_forecasts[[i]] <- c(format(start_date, "%b-%y"), forecasts)
}

# Loop for Temporally reconciled forecasts
for (i in 1:dim(thf_reconciled_vecm)[3]) {
  start_date <- as.Date("2021-07-01") %m+% months(i - 1)
  forecasts <- thf_reconciled_vecm[1, , i]
  thf_reconciled_forecasts[[i]] <- c(format(start_date, "%b-%y"), forecasts)
}

# Loop for Cross-temporally reconciled forecasts
for (i in 1:dim(reconciled_vecm_tcs)[3]) {
  start_date <- as.Date("2021-07-01") %m+% months(i - 1)
  forecasts <- reconciled_vecm_tcs[1, , i]
  tcs_reconciled_forecasts[[i]] <- c(format(start_date, "%b-%y"), forecasts)
}

# Convert lists to data frames
hts_reconciled_forecasts <- do.call(rbind, hts_reconciled_forecasts)
thf_reconciled_forecasts <- do.call(rbind, thf_reconciled_forecasts)
tcs_reconciled_forecasts <- do.call(rbind, tcs_reconciled_forecasts)

# Convert to data frames with appropriate column names
hts_reconciled_forecasts <- as.data.frame(hts_reconciled_forecasts, stringsAsFactors = FALSE)
colnames(hts_reconciled_forecasts) <- c("Date", as.character(1:num_forecasts))

thf_reconciled_forecasts <- as.data.frame(thf_reconciled_forecasts, stringsAsFactors = FALSE)
colnames(thf_reconciled_forecasts) <- c("Date", as.character(1:num_forecasts))

tcs_reconciled_forecasts <- as.data.frame(tcs_reconciled_forecasts, stringsAsFactors = FALSE)
colnames(tcs_reconciled_forecasts) <- c("Date", as.character(1:num_forecasts))

# Add data frames to workbook
addWorksheet(wb, "Cross-Temporally Reconciled")
writeData(wb, sheet = "Cross-Temporally Reconciled", x = hts_reconciled_forecasts)

addWorksheet(wb, "Temporally Reconciled")
writeData(wb, sheet = "Temporally Reconciled", x = thf_reconciled_forecasts)

addWorksheet(wb, "Cross-Sectionally Reconciled")
writeData(wb, sheet = "Cross-Sectionally Reconciled", x = tcs_reconciled_forecasts)

# Save the workbook
saveWorkbook(wb, "FINAL_FORECAST/final_forecast_data/forecasts.xlsx", overwrite = TRUE)

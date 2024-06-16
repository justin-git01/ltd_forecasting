test_extract <- function(data){
  # Get the last month available in ltd_filtered
  last_month <- max(data$Month)
  
  # Calculate the range for the next 12 months
  start_ym <- yearmonth(as.Date(last_month) + days(1))
  end_ym <- yearmonth(as.Date(last_month) + months(12) + days(1) - days(1))
  
  # Create a complete sequence of months for the next 12 months
  full_month_seq <- seq(as.Date(start_ym), as.Date(end_ym), by = "month")
  full_month_seq <- yearmonth(full_month_seq)
  full_month_seq <- as_tsibble(data.frame(Month = full_month_seq), index = Month)
  
  # Filter using 'yearmonth' range
  required_data <- ltd_unit %>%
    filter(Month > start_ym & Month <= end_ym) %>%
    select(Month, Total, NonRes, Comm, Ind, Other, Res)
  
  # Join with the full sequence to fill missing months with NA
  complete_data <- full_month_seq %>%
    left_join(required_data, by = "Month")
  
  # Return the transposed data excluding the 'Month' column
  return(t(complete_data[-1,-1]))
}


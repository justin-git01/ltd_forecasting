test_extract <- function(data){
  # Get the last month available in ltd_filtered
  last_month <- max(data$Month)
  
  # Calculate the range for the next 12 months
  start_ym <- yearmonth(as.Date(last_month) + days(1))
  end_ym <- yearmonth(as.Date(last_month) + months(12) + days(1) - days(1))
  
  # Filter using 'yearmonth' range
  required_data <- ltd_unit %>%
    filter(Month > start_ym & Month <= end_ym) %>%
    select(Total, NonRes, Comm, Ind, Other, Res) %>%
    relocate(Month)
  
  return(t(required_data[,-1]))
}


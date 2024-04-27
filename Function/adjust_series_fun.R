# Function to adjust the time series based on the frequency
adjust_series <- function(data, freq) {
  rownum <- nrow(data)
  if (freq == 1) {
    # For annual frequency, make sure the data length is a multiple of 12
    usable_length <- rownum %/% 12 * 12
  } else {
    usable_length <- rownum %/% freq * freq
  }

  data_use <- tail(data, n = usable_length)  # Get the tail part of the data
  if (freq == 1) {
    # Sum up every 12 months to create annual data
    ts(apply(data_use, 2, function(x) colSums(matrix(x, nrow = 12))),
       frequency = 1)
  } else {
    # General case for other frequencies
    ts(apply(data_use, 2, function(x) colSums(matrix(x, nrow = 12/freq))),
       frequency = freq)
  }
}
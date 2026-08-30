sma <- function(data, period) {
  
  # Check if the length of data is less than the specified period
  if(length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  
  # Initialize a vector to store the SMA values
  sma_values <- numeric(length(data) - period + 1)
  
  # Calculate SMA for each window of 'period' data points
  for(i in 1:(length(data) - period + 1)) {
    
    current_window <- data[i:(i + period - 1)]
    
    mean_value <- sum(current_window) / period
    
    sma_values[i] <- mean_value
  }
  
  return(sma_values)
}

# Sample data

data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

# Calculate SMA with a period of 3

sma_result <- sma(data, period = 3)

print(sma_result)

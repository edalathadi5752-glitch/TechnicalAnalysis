# Exponential Moving Average (EMA) function

ema <- function(data, period) {
  
  multiplier <- 2 / (period + 1)
  
  ema_values <- numeric(length(data))
  
  for(i in 1:length(data)) {
    
    if(i == 1) {
      
      ema_values[i] <- data[i]
      
    } else {
      
      ema_values[i] <- (data[i] - ema_values[i - 1]) *
        multiplier + ema_values[i - 1]
      
    }
  }
  
  return(ema_values)
}

# Moving Average Convergence Divergence (MACD)

macd <- function(data,
                 short_period,
                 long_period,
                 signal_period) {
  
  # Calculate the short-term EMA
  short_ema <- ema(data, short_period)
  
  # Calculate the long-term EMA
  long_ema <- ema(data, long_period)
  
  # Calculate the MACD line
  macd_line <- short_ema - long_ema
  
  # Calculate the signal line
  signal_line <- ema(macd_line, signal_period)
  
  # Calculate the histogram
  histogram <- macd_line - signal_line
  
  # Return results
  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )
  
  return(result)
}

# Sample data

data <- c(100, 105, 110, 115, 120, 125, 130)

# Calculate MACD

macd_result <- macd(
  data,
  short_period = 3,
  long_period = 5,
  signal_period = 2
)

print(macd_result)


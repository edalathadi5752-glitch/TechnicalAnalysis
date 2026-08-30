rsi <- function(data, period) {
  
  # Calculate differences between consecutive data points
  diff_values <- diff(data)
  
  # Initialize gains and losses
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))
  
  # Calculate gains and losses
  for(i in 1:length(diff_values)) {
    
    if(diff_values[i] > 0) {
      
      gains[i] <- diff_values[i]
      
    } else {
      
      losses[i] <- abs(diff_values[i])
      
    }
    
  }
  
  # Calculate average gain and loss
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  
  # Initialize RSI vector
  rsi_values <- rep(NA, length(data))
  
  # Wilder smoothing
  for(i in (period + 1):length(data)) {
    
    avg_gain <- ((avg_gain * (period - 1)) +
                   gains[i - 1]) / period
    
    avg_loss <- ((avg_loss * (period - 1)) +
                   losses[i - 1]) / period
    
    rs <- avg_gain / avg_loss
    
    rsi_values[i] <- 100 - (100 / (1 + rs))
  }
  
  return(rsi_values)
}

# Sample data

data <- c(
  45, 50, 48, 55, 52,
  49, 58, 60, 65, 62
)

# Calculate RSI

rsi_result <- rsi(data, period = 5)

print(rsi_result)

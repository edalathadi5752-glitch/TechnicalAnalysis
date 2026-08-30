linreg <- function(regressionSource,
                   regressionLength,
                   regressionOffset) {
  
  # Calculate the total number of elements
  n <- length(regressionSource)
  
  # Check if regressionLength is greater than number of elements
  if(regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }
  
  # Check if regressionOffset is greater than or equal to regressionLength
  if(regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }
  
  # Calculate start and end indices
  start_index <- max(1, n - regressionLength + regressionOffset)
  
  end_index <- min(n, n - regressionOffset)
  
  # Extract source subset
  source_subset <- regressionSource[start_index:end_index]
  
  # Calculate index values
  index_values <- 1:length(source_subset)
  
  # Calculate means
  mean_index <- sum(index_values) / length(index_values)
  
  mean_source <- sum(source_subset) / length(source_subset)
  
  # Calculate numerator and denominator
  numerator <- sum(
    (index_values - mean_index) *
      (source_subset - mean_source)
  )
  
  denominator <- sum(
    (index_values - mean_index)^2
  )
  
  # Calculate slope and intercept
  slope <- numerator / denominator
  
  intercept <- mean_source - slope * mean_index
  
  # Calculate predicted values
  predicted_values <- slope * index_values + intercept
  
  # Return result
  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )
  
  return(result)
}

# Sample data

data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

# Calculate linear regression

linreg_result <- linreg(
  regressionSource = data,
  regressionLength = 5,
  regressionOffset = 0
)

print(linreg_result)


library(quantmod)
library(TTR)

load_stock_data <- function(file_name) {
  
  symbols <- readLines(file_name)
  
  stock_list <- list()
  
  for(symbol in symbols) {
    
    stock_data <- getSymbols(
      symbol,
      src = "yahoo",
      auto.assign = FALSE
    )
    
    stock_list[[symbol]] <- stock_data
  }
  
  return(stock_list)
}
get_mode <- function(x) {
  
  ux <- unique(x)
  
  ux[which.max(tabulate(match(x, ux)))]
}

calculate_statistics <- function(stock_data) {
  
  close_prices <- round(as.numeric(Cl(stock_data)), 2)
  
  stats <- list(
    Mean = mean(close_prices, na.rm = TRUE),
    Median = median(close_prices, na.rm = TRUE),
    Mode = get_mode(close_prices),
    SD = sd(close_prices, na.rm = TRUE),
    MovingAverage = SMA(close_prices, n = 20)
  )
  
  return(stats)
}

for(symbol in names(stocks)) {
  
  cat("\n====================\n")
  cat("Stock:", symbol, "\n")
  
  stats <- calculate_statistics(stocks[[symbol]])
  
  print(stats$Mean)
  print(stats$Median)
  print(stats$Mode)
  print(stats$SD)
}

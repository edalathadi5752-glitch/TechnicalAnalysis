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

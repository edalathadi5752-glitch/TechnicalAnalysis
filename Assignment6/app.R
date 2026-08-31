library(shiny)
library(ggplot2)
library(quantmod)

stock_symbol <- "AAPL"

start_date <- "2023-01-01"

end_date <- "2023-07-01"

stock_data <- getSymbols(
  stock_symbol,
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

ui <- fluidPage(
  
  titlePanel("Portfolio Dashboard"),
  
  dateRangeInput(
    "date_range",
    "Select Date Range:",
    start = "2023-01-01",
    end = "2023-07-01"
  ),
  
  selectInput(
    "time_frame",
    "Select Time Frame:",
    choices = c(
      "Daily",
      "Weekly",
      "Monthly"
    )
  ),
  
  checkboxGroupInput(
    "technical_indicators",
    "Technical Indicators:",
    choices = c(
      "Moving Averages",
      "RSI",
      "MACD"
    )
  ),
  
  plotOutput("stock_chart") 
  
)

server <- function(input, output) {
  
  output$stock_chart <- renderPlot({
    
    chart_data <- data.frame(
      
      Date = index(stock_data),
      
      Close = as.numeric(Cl(stock_data))
      
    )
    
    p <- ggplot(
      chart_data,
      aes(x = Date, y = Close)
    ) +
      geom_line(color = "blue")
    
    if("Moving Averages" %in%
       input$technical_indicators) {
      
      chart_data$MA20 <- as.numeric(
        stats::filter(
          chart_data$Close,
          rep(1/20, 20),
          sides = 1
        )
      )
      
     
      p <- p +
        geom_line(
          data = chart_data,
          aes(y = MA20),
          color = "red"
        )
    }
    
    # Trading Signals
    
    signal_data <- chart_data[!is.na(chart_data$MA20), ]
    
    signal_data$Signal <- ifelse(
      signal_data$Close > signal_data$MA20,
      "Buy",
      "Sell"
    )
    
    p <- p +
      geom_text(
        data = tail(signal_data, 10),
        aes(label = Signal),
        color = "darkgreen",
        vjust = -1,
        size = 3
      )
    
    
    print(p)
    
  })
  
}

shinyApp(ui, server)


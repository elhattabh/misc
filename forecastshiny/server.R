library(shiny)
library(ROCR)
library(dplyr)
library(prophet)
library(ggplot2)
library(dygraphs)
library(xts)


shinyServer(function(input, output) {
  
  topics <- read.csv("/Users/konafa/Downloads/squarespacetopics.csv")
  
  topics$ds<-as.Date(topics$date, format="%m/%d/%Y")    
  topics$y<-as.numeric(topics$count)
  email <- topics[topics$category=="email", c("ds","y"),]
  chat <- topics[topics$category=="chat", c("ds","y"),]
  data <- read.csv("/Users/konafa/Downloads/xts.csv")
    
    data$ds<-as.Date(data$date)
    data$y<-as.numeric(data$count)
    email <- data[data$category=="email", c("ds","y"),]
    chat <- data[data$category=="chat" & data$y >10, c("ds","y"),]
  
    drop <- data[, !(colnames(data) %in% c("y"))]
    xts <- as.xts(drop,order.by=drop$ds)
  
    #dygraph for the chat data we have already
    output$dygraph <- renderDygraph({
      xts <- subset(xts, category == input$category)

      d1 <-  dygraph(xts) %>%
        dyHighlight(highlightSeriesBackgroundAlpha = 0.5, highlightCircleSize = 3, highlightSeriesOpts = list(strokeWidth = 3),hideOnMouseOut = FALSE) %>%
        dyRangeSelector() %>% dyOptions(colors = c("purple"))

      d1
    })
  
  #ggplot(data=subset_data, aes(x=as.Date(datecalled), y=value, color=category)) + geom_line() + geom_point() +labs(x = "Date Called")`
  
  
  output$forecast <- renderPlot({
    
    
    m <- prophet(df = topics[topics$category==input$category & topics$chat_topic== input$chat_topic & topics$y >10, c("ds","y"),], growth = "linear", changepoints = NULL,
                 weekly.seasonality = "TRUE", holidays = NULL, yearly.seasonality=FALSE,
                 uncertainty.samples = 1000, fit = TRUE)
    future <- make_future_dataframe(m, periods=30, freq = "day", include_history = TRUE)
    forecast <- predict(m, future)
    plot(m, forecast)    
  })
})

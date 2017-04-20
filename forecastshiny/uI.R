library(shiny)
library(dygraphs)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Pick the category you want to see: (top for data we already have, bottom for future forecast):"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("category", "Category for Past and Forecast (Toggle Me):", 
                  choices = list("chat", 
                                 "email"), multiple=FALSE, selected = "chat"),
      selectInput("chat_topic", "Topic for Forecast (Toggle Me):", 
                  choices = list("All Topics",
                                 "Adding and editing pages and blocks", 
                                 "Billing, signup and payments",
                                 "Domains and email services",
                                 "Getting Started with Squarespace",
                                 "Commerce, and managing your store",
                                 "Designing your site and managing your template",
                                 "Galleries, images, and videos",
                                 "Logging in and managing your account",
                                 "SEO and metrics",
                                 "Squarespace Mobile Apps"), multiple=FALSE, selected = "All Topics")
      
    ),      
    mainPanel(
      #plotOutput("x"),      
      h4("Past Data by n of Unique Chats/Emails Tickets:"),
      dygraphOutput("dygraph"),
      h4("Forecast for n of Unique Chats/Email Tickets Per Day Per Topic for February:"),
      plotOutput('forecast')
    
      
    ))
))

library(shiny)
library(mongolite)

ui <- fluidPage(
  selectInput("docSelect", "Select Document:", choices = NULL),
  textOutput("selectedDoc")
)

server <- function(input, output, session) {
  mongoConn <- mongo(collection = "your_collection", db = "your_db", url = "your_mongo_url")
  
  # Populate drop-down with document names
  updateSelectInput(session, "docSelect", choices = mongoConn$find('{}')$name)
  
  output$selectedDoc <- renderText({
    paste("You selected:", input$docSelect)
  })
}

shinyApp(ui, server)

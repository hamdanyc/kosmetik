library(shiny)
library(DT)
library(mongolite)

# Sample data (replace with your actual data and MongoDB connection details)
data <- data.frame(
  ID = 1:3,
  Name = c("Alice", "Bob", "Charlie"),
  Age = c(25, 30, 28),
  stringsAsFactors = FALSE
)

mongo_uri <- "mongodb://user:password@host:27017/dbname"
collection_name <- "your_collection"

ui <- fluidPage(
  titlePanel("Editable datatable and MongoDB"),
  DT::dataTableOutput("editable_table"),
  verbatimTextOutput("updates"),
  actionButton("save_button", "Save Changes")
)

server <- function(input, output, session) {
  
  values <- reactiveValues(data = data)
  
  output$editable_table <- DT::renderDataTable({
    DT::datatable(values$data, editable = TRUE, rownames = FALSE)
  })
  
  observeEvent(input$editable_table_cell_edit, {
    info <- input$editable_table_cell_edit
    str(info) # Inspect the structure of info
    i <- info$row
    j <- info$col
    value <- info$value
    
    # Accessing and updating the reactive values
    values$data[i, j] <- value
    values$data <- values$data  # Trigger reactivity
  })
  
  output$updates <- renderPrint({
    req(input$editable_table_cell_edit)
    input$editable_table_cell_edit
  })
  
  observeEvent(input$save_button, {
    # Connect to MongoDB
    collection <- mongo(collection = collection_name, url = mongo_uri)
    
    # Get the edited data
    edited_data <- values$data
    
    # Iterate through the edited rows
    for (i in 1:nrow(edited_data)) {
      # Construct the query to find the document
      query <- list(ID = edited_data$ID[i])
      
      # Construct the update operation
      update_ops <- list(
        `$set` = list(
          Name = edited_data$Name[i],
          Age = edited_data$Age[i]
        )
      )
      
      # Perform the update
      collection$update(query, update_ops)
    }
    print("Data saved to MongoDB")
    # Optionally, you might want to reload the data from MongoDB to reflect the changes in the table
    # values$data <- collection$find() 
  })
}

shinyApp(ui = ui, server = server)
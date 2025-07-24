library(shiny)
library(DT)
library(mongolite)
library(jsonlite)

# MongoDB connection
con <- mongo(collection = "items", 
                          db = "oa", 
                          url = Sys.getenv("URI-1"))

ui <- fluidPage(
  DTOutput("data_table"),
  actionButton("update", "Update Table")
)

server <- function(input, output, session) {
  
  # Reactive expression to fetch data from MongoDB
  data = dplyr::as_tibble(con$find(fields = '{"_id": 1, "item": 1, "price": 1}')) # Fetch all documents
  values <- reactiveValues(data = data)

  # Render the DataTable
  output$data_table <- renderDT({
    datatable(values$data, editable = TRUE)
  })
  
  observeEvent(input$editable_table_cell_edit, {
    info <- input$editable_table_cell_edit
    i <- info$row
    j <- info$col
    value <- info$value
    
    # Accessing and updating the reactive values
    values$data[i, j] <- value
    values$data <- values$data  # Trigger reactivity
  })
  
  # Assume a dataframe 'df' with _id and other fields like 'name' and 'value'
  # ie:
  # for (i in 1:nrow(df)) {
  #   query <- paste0('{"_id": "', df$._id[i], '"}')
  #   update_data <- df[i,]
  #   update_data$._id <- NULL
  #   update_op <- paste0('{"$set":', jsonlite::toJSON(update_data, auto_unbox = TRUE), '}')
  #   
  #   collection$update(query, update_op, upsert = TRUE)
  # }
  
  # Update table on button click
  observeEvent(input$update, {
    
    # Get the edited data
    edited_data <- values$data
    
    for (i in 1:nrow(edited_data)) {
      row <- edited_data[i, ]
      
      # Construct the query (assuming '_id' is the unique identifier)
      query <- paste0('{"_id": "', row$`_id`, '"}')
      
      # Remove the '_id' field from the update document to avoid errors
      update_data <- list(item = row$item, price = row$price)
      update_data$`_id` <- NULL
      
      # Construct the update document
      update <- paste0('{"$set": ', toJSON(update_data, auto_unbox = TRUE), '}')
      
      # Update the document
     con$update(query, update)
    }
  })
}

shinyApp(ui, server)

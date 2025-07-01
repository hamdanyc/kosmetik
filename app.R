# dashboard for kosmetik

library(shiny)
library(dplyr)
library(mongolite)

# read sales data
uri <- Sys.getenv("URI")
db <- mongo(collection="sales", db="oa", url=uri)
df <- db$find('{}')
df_item <- df %>% distinct(item)

# Define UI 
ui <- fluidPage(
    titlePanel("Product Sales Dashboard"),  
    sidebarLayout(
        sidebarPanel(
            selectInput(  
                inputId = "product_choice",
                label = "Select Product",
                choices = df_item,
                selected = df_item[1]
            ),
        ),
            mainPanel(
                h3(tableOutput("sales"))
            )
    )
)

# server logic
server <- function(input, output) {
    output$total_amount <- renderText({
        selected <- input$product_choice
        
        filtered_data <- df %>% filter(item == selected) %>%
            mutate(total_sales = item_price * qty) %>%
            summarise(total = sum(total_sales))
        
        total <- filtered_data$total
        currency_format <-
            paste0("RM", format(
                round(total, 2),
                nsmall = 2,
                big.mark = ","
            ))
        currency_format
    })
    
    output$sales <- renderTable({
        selected <- input$product_choice
        
        filtered_data <- df %>% filter(item == selected) %>%
            mutate(total_sales = item_price * qty) %>%
            summarise("Sales total" = sum(total_sales), "Quantity" = sum(qty))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

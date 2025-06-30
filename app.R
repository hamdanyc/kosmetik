# dashboard for kosmetik

library(shiny)
library(dplyr)

# read sales data
df <- readr::read_csv("sales.csv")

# Define UI 
ui <- fluidPage(
    titlePanel("Product Sales Dashboard"),  
    sidebarLayout(
        sidebarPanel(
            selectInput(  
                inputId = "product_choice",
                label = "Select Product",
                choices = c("shampoo", "shower gel", "make up cleansing"),
                selected = "shampoo"
            ),
        ),
            mainPanel(
                h2("Total Sales for:"),
                h3(textOutput("total_amount"))
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
}

# Run the application 
shinyApp(ui = ui, server = server)

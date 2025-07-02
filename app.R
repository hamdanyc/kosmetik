# dashboard for Wawa kosmetik
# https://abi-posit-wawakosmetik.share.connect.posit.cloud/

library(shiny)
library(dplyr)
library(mongolite)
library(ggplot2)

# read sales data
# uri <- Sys.getenv("URI-1")
# db <- mongo(collection="sales", db="oa", url=uri)
# df <- db$find('{}')
# df_item <- df %>% distinct(item)

dt <- lubridate::today()
# Define UI 
ui <- bs4Dash::dashboardPage(
    title = "Shiny Application",
    bs4Dash::dashboardHeader(
        title = "Wawa Roadmap"
    ),
    bs4Dash::dashboardSidebar(
        bs4Dash::sidebarMenu(
            bs4Dash::menuItem(
                text = "Transaksi",
                tabName = "tab_j1p2rgcpnn"
            ),
            bs4Dash::menuItem(
                text = "Ringkasan",
                tabName = "tab_4dhx95c6et"
            )
        )
    ),
    bs4Dash::dashboardBody(
        bs4Dash::tabItem(
            tabName = "tab_j1p2rgcpnn",
            h1(
                "Transaksi"
            ),
            textInput(
                inputId = "input_dhnrhjc40z",
                label = "Tarikh",
                value = dt
            ),
            selectInput(
                inputId = "input_vp6vjageru",
                label = "Kategori",
                choices = c("Sales Retail","Sales Agent","Restock", "Lain-lain"),
                selected = "Sales Retail"
            ),
            selectInput(
                inputId = "input_tqvu7icwb7",
                label = "Item",
                choices = c("Sales","Restock FR"),
                selected = "Sales"
            ),
            numericInput(
                inputId = "input_l7ncx2dtfs",
                label = "Amaun",
                value = 300
            ),
            actionButton(
                inputId = "input_y48vyz6564",
                "Terima!",
                class = "btn-success"
            ),
            tableOutput(
                outputId = "df"
            )
        ),
        bs4Dash::tabItem(
            tabName = "tab_4dhx95c6et"
        )
    )
)

# server logic
server <- function(input, output) {
    observeEvent(input$input_y48vyz6564, {
        output$df <- renderTable({
            
            c(input$input_dhnrhjc40z, input$input_vp6vjageru,
              input$input_tqvu7icwb7, input$input_l7ncx2dtfs)
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

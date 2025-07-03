# dashboard for Wawa kosmetik
# https://abi-posit-wawakosmetik.share.connect.posit.cloud/

library(shiny)
library(dplyr)
library(mongolite)
library(ggplot2)
library(bs4Dash)

# read sales data
uri <- Sys.getenv("URI")
db <- mongo(collection="sales", db="oa", url=uri)
df <- db$find('{}')

dt <- lubridate::today()
dm <- lubridate::month(today())

# Define UI 
ui <- dashboardPage(
    title = "Shiny Application",
    dashboardHeader(
        title = "Wawa Roadmap"
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                text = "Transaksi",
                tabName = "tab_j1p2rgcpnn"
            ),
            menuItem(
                text = "Ringkasan Tahunan",
                tabName = "tab_inr00usq8d"
            ),
            menuItem(
                text = "Ringkasan Bulanan",
                tabName = "tab_ijylm47bld"
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
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
                    choices = c("sales retail","sales agent","restock", "misc costs"),
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
            tabItem(
                tabName = "tab_inr00usq8d",
                h1(
                    "Ringkasan Tahun 2025"
                ),
                box(
                    title = "Jualan:", textOutput(
                        outputId = "output_yr_sales",
                    ),
                    solidHeader = TRUE,
                    background = "indigo"
                ),
                box(
                    title = "Perbelanjaan:", textOutput(
                        outputId = "output_yr_exp",
                    ),
                    solidHeader = TRUE,
                    background = "info"
                ),
                box(
                    title = "Pendapatan:", textOutput(
                        outputId = "output_yr_net",
                    ),
                    solidHeader = TRUE,
                    backgroud = "gray"
                ),
                box(
                    title = "Margin:", textOutput(
                        outputId = "output_yr_margin",
                    ),
                    solidHeader = TRUE,
                    background = "warning"
                ),
                plotOutput(
                    outputId = "output_waybzvxb4v"
                )
            ),
            tabItem(
                tabName = "tab_ijylm47bld",
                h1(
                    "Ringkasan Mengikut Bulan"
                ),
                sliderInput(
                    inputId = "slider_jv8sv5rtdw",
                    label = "Bulan",
                    min = 0,
                    max = 12,
                    value = dm
                ),
                box(
                    title = "Jualan:", textOutput(
                        outputId = "output_mth_sales"
                    ),
                    solidHeader = TRUE,
                    background = "primary"
                ),
                box(
                    title = "Perbelanjaan:", textOutput(
                        outputId = "output_mth_exp"
                    ),
                    solidHeader = TRUE,
                    background = "maroon"
                ),
                box(
                    title = "Pendapatan:", textOutput(
                        outputId = "output_mth_net"
                    ),
                    solidHeader = TRUE,
                    background = "lime"
                ),
                box(
                    title = "Margin:", textOutput(
                        outputId = "output_mth_margin"
                    ), 
                    solidHeader = TRUE,
                    background = "warning"
                )
            )
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
    
    # Calculate total Sales - Exp
    total_sales <- df %>% 
        filter(item == "sales retail" | item == "sales agent") %>% 
        summarise("Total sale" = sum(amount)) %>% unlist()
    
    total_exp <- df %>% 
        filter(item == "restock RF" | item == "misc cost") %>% 
        summarise("Total sale" = sum(amount)) %>% unlist()
    
    net_profit <- total_sales - total_exp
    net_margin <- net_profit / total_sales * 100
    output$output_yr_sales <- renderText(total_sales)
    output$output_yr_exp <- renderText(total_exp)
    output$output_yr_net <- renderText(net_profit)
    output$output_yr_margin <- renderText(net_margin)
    
    # monthly aggregate
    observeEvent(input$slider_jv8sv5rtdw,{
        mth_sales <- df %>% 
            group_by(lubridate::month(date)) %>% 
            filter(lubridate::month(date) == input$slider_jv8sv5rtdw) %>% 
            filter(item == "sales retail" | item == "sales agent") %>% 
            summarise("Total" = sum(amount))
        
        mth_exp <- df %>% 
            group_by(lubridate::month(date)) %>% 
            filter(lubridate::month(date) == input$slider_jv8sv5rtdw) %>% 
            filter(item == "restock RF" | item == "misc cost") %>% 
            summarise("Total" = sum(amount))
        
        mth_net <- mth_sales$Total - mth_exp$Total
        mth_margin <- mth_net / mth_sales$Total * 100
        output$output_mth_sales <- renderText(mth_sales$Total)
        output$output_mth_exp <- renderText(mth_exp$Total)
        output$output_mth_net <- renderText(mth_net)
        output$output_mth_margin <- renderText(mth_margin)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

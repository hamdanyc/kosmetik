# dashboard for Wawa kosmetik
# https://abi-posit-wawakosmetik.share.connect.posit.cloud/
# rsconnect::writeManifest()

library(shiny)
library(dplyr)
library(mongolite)
library(ggplot2)
library(bs4Dash)
library(DT)

# read sales data
uri <- Sys.getenv("URI")
db <- mongo(collection="sales", db="oa", url=uri)
df <- db$find('{}')

dt <- lubridate::today()
dm <- lubridate::month(dt)

# Define ui ----
ui <- dashboardPage(
    title = "Shiny Application",
    dashboardHeader(
        title = "Wawa Roadmap"
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                text = "Transaksi",
                tabName = "tb_trans"
            ),
            menuItem(
                text = "Ringkasan Tahunan",
                tabName = "tab_thn"
            ),
            menuItem(
                text = "Ringkasan Bulanan",
                tabName = "tab_bln"
            ),
            menuItem(
                text = "Kemaskini Bulanan",
                tabName = "tab_edtbln"
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "tb_trans",
                h1(
                    "Transaksi"
                ),
                dateInput(
                    inputId = "input_tkh",
                    label = "Tarikh",
                    value = dt
                ),
                selectInput(
                    inputId = "input_item",
                    label = "Item",
                    choices = c("sales","restock","misc cost"),
                    selected = "sales"
                ),
                numericInput(
                    inputId = "input_amaun",
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
                tabName = "tab_thn",
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
                    title = "Margin: (%)", textOutput(
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
                tabName = "tab_bln",
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
                    title = "Margin: (%)", textOutput(
                        outputId = "output_mth_margin"
                    ), 
                    solidHeader = TRUE,
                    background = "warning"
                )
            ),
            tabItem(
                tabName = "tab_edtbln",
                h1(
                    "Verifikasi | Kemaskini Mengikut Bulan"
                ),
                sliderInput(
                    inputId = "slider_tbmth",
                    label = "Bulan",
                    min = 0,
                    max = 12,
                    value = dm
                ),
                DT::dataTableOutput('tbmth')
            )
        )
    )
)

# server logic ----
server <- function(input, output) {
    observeEvent(input$input_y48vyz6564, {
        tb <- tibble(date = lubridate::as_date(format(input$input_tkh,"%Y-%m-%d")), 
                     item = input$input_item, amount = input$input_amaun)
        output$df <- renderTable({
            tb
        })
        # insert database
        db$insert(tb)
        showModal(modalDialog(
            title = "Notis",
            "Maklumat telah dikemaskini!"
        ))
    })
    
    # Calculate total Sales - Exp
    total_sales <- df %>% 
        filter(item == "sales") %>% 
        summarise("Total" = sum(amount)) 
    
    total_exp <- df %>% 
        filter(item == "restock" | item == "misc cost") %>% 
        summarise("Total" = sum(amount))
    
    net_profit <- total_sales$Total - total_exp$Total
    net_margin <- net_profit / total_sales$Total * 100
    output$output_yr_sales <- renderText(total_sales$Total)
    output$output_yr_exp <- renderText(total_exp$Total)
    output$output_yr_net <- renderText(net_profit)
    output$output_yr_margin <- renderText(round(net_margin))
    
    # monthly aggregate
    observeEvent(input$slider_jv8sv5rtdw,{
        mth_sales <- df %>% 
            group_by(lubridate::month(date)) %>% 
            filter(lubridate::month(date) == input$slider_jv8sv5rtdw) %>% 
            filter(item == "sales") %>% 
            summarise("Total" = sum(amount))
        
        mth_exp <- df %>% 
            group_by(lubridate::month(date)) %>% 
            filter(lubridate::month(date) == input$slider_jv8sv5rtdw) %>% 
            filter(item == "restock" | item == "misc cost") %>% 
            summarise("Total" = sum(amount))
        
        mth_net <- mth_sales$Total - mth_exp$Total
        mth_margin <- mth_net / mth_sales$Total * 100
        output$output_mth_sales <- renderText(mth_sales$Total)
        output$output_mth_exp <- renderText(mth_exp$Total)
        output$output_mth_net <- renderText(mth_net)
        output$output_mth_margin <- renderText(round(mth_margin))
    })
    
    # View | edit month transaction
    observeEvent(input$slider_tbmth,{
        output$tbmth <- DT::renderDataTable({
            td <- df %>% filter(lubridate::month(date) == input$slider_tbmth)
            datatable(td, editable = "row", options = list(
                dom = 'Bfrtip',
                buttons = c('create', 'edit', 'remove'),
                fixedHeader = TRUE
            ))
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

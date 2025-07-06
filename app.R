# dashboard for Wawa kosmetik
# https://abi-posit-wawakosmetik.share.connect.posit.cloud/
# rsconnect::writeManifest()

library(shiny)
library(dplyr)
library(mongolite)
library(ggplot2)
library(bs4Dash)
library(DT)
library(jsonlite)

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
                text = "Kemaskini Transaksi | Bulanan",
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
                    inputId = "slider_mth",
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
        # db$insert(tb)
        showModal(modalDialog(
            title = "Notis",
            "Maklumat telah dikemaskini!"
        ))
    })
    
    # Calculate total Sales - Exp
    df <- db$find('{}')
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
    observeEvent(input$slider_mth,{
        # pipe <- '[{ $project: {date: 1, item: 1, amount: 1, mth: {$month: "$date" }}}, {$match:{ mth: 6}}]'
        
        pipe_list <- list(
            list('$project' = list(date = 1, item = 1, amount = 1, mth = list('$month' = '$date'))),
            list('$match' = list(mth = input$slider_mth))
        )
        pipe <- toJSON(pipe_list, auto_unbox = TRUE)
        
        # Query db with aggregate
        td <- db$aggregate(pipe = pipe)
        
        if (nrow(td) > 0) {
            
            mth_sales <- td %>% 
                filter(item == "sales") %>% 
                summarise("Total" = sum(amount))
            
            mth_exp <- td %>% 
                filter(item == "restock" | item == "misc cost") %>% 
                summarise("Total" = sum(amount))
            
            mth_net <- mth_sales$Total - mth_exp$Total
            mth_margin <- mth_net / mth_sales$Total * 100
            
            output$output_mth_sales <- renderText(mth_sales$Total)
            output$output_mth_exp <- renderText(mth_exp$Total)
            output$output_mth_net <- renderText(mth_net)
            output$output_mth_margin <- renderText(round(mth_margin))
        } else {
            output$output_mth_sales <- renderText("")
            output$output_mth_exp <- renderText("")
            output$output_mth_net <- renderText("")
            output$output_mth_margin <- renderText("")
        }
    })
    
    # View | edit transaction
    observeEvent(input$slider_tbmth,{
        output$tbmth <- DT::renderDataTable({
            # pipe <- '[{ $project: {_id: 0, date: 1, item: 1, amount: 1, mth: {$month: "$date" }}}, {$match:{ mth: 6}}]'
            pipe_list <- list(
                list('$project' = list("_id" = 0, date = 1, item = 1, amount = 1, mth = list('$month' = '$date'))),
                list('$match' = list(mth = input$slider_tbmth))
            )
            pipe <- toJSON(pipe_list, auto_unbox = TRUE)
            
            # Query db with aggregate
            td <- db$aggregate(pipe = pipe)
            
            if (nrow(td) > 0) {
            datatable(td, rownames = FALSE, editable = list(
                target = 'row', disable = list(columns = c(1,4))),
                options = list(dom = 'Bfrtip',
                               buttons = c('create', 'edit', 'remove'),
                               fixedHeader = TRUE
                ))
            } else{
                showModal(modalDialog(
                    title = "Maaf",
                    "Tiada Maklumat diketemukan!"
                ))
            }
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

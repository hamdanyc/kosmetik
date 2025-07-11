# dashboard for Wawa kosmetik
# https://abi-posit-wawakosmetik.share.connect.posit.cloud/
# rsconnect::wrtypeanifest()

library(shiny)
library(dplyr)
library(mongolite)
library(ggplot2)
library(bs4Dash)
library(DT)
library(jsonlite)

# read sales data
uri <- Sys.getenv("URI")
colSales <- mongo(collection="sales", db="oa", url=uri)
colItem <- mongo(collection="items", db="oa", url=uri)
dt <- lubridate::today()
dm <- lubridate::month(dt)

# Init values
res <- (colItem$find('{}', fields = '{"_id": 0, "item": 1, "price": 1}')) %>% 
    as_tibble()

# func price
price <- function(item){
    qry <- toJSON(list(item = item, auto_unbox = TRUE))
    pc <- colItem$find(qry, fields = '{"_id": 0, "price": 1}') %>% unlist()
    return(pc)
}
    
# Define ui ----
ui <- dashboardPage(
    title = "Shiny Application",
    dashboardHeader(
        title = "Wawa Roadmap"
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                text = "Transaksi Jualan | Belian",
                tabName = "tb_trans"
            ),
            menuItem(
                text = "Jadual Item",
                tabName = "tb_items"
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
                text = "Verifikasi | Transaksi (Bulanan)",
                tabName = "tab_edtbln"
            ),
            menuItem(
                text = "Verifikasi | Item (Bulanan)",
                tabName = "tab_itembln"
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "tb_trans",
                h1(
                    "Transaksi Jualan | Belian"
                ),
                dateInput(
                    inputId = "input_tkh",
                    label = "Tarikh",
                    value = dt
                ),
                selectInput(
                    inputId = "input_type",
                    label = "Jenis",
                    choices = c("sales","restock","misc cost"),
                    selected = "sales"
                ),
                selectInput(
                    inputId = "input_item",
                    label = "Produk",
                    choices = colItem$find('{}', fields = '{"_id": 0, "item": 1}')
                ),
                numericInput(
                    inputId = "input_unit",
                    label = "Unit",
                    value = 3
                ),
                numericInput(
                    inputId = "input_price",
                    label = "Harga",
                    value = colItem$find('{"item": "Basic set"}', fields='{"_id":0,"price":1}')
                ),
                numericInput(
                    inputId = "input_amaun",
                    label = "Amaun",
                    value = 300
                ),
                # textOutput("price"), not run 
                actionButton(
                    inputId = "btn_trans",
                    "Terima!",
                    class = "btn-success"
                )
            ),
            tabItem(
                tabName = "tb_items",
                h1(
                    "Jadual Item"
                ),
                textInput(
                    inputId = "input_name_item",
                    label = "Nama produk"
                ),
                numericInput(
                    inputId = "input_price_item",
                    label = "Harga",
                    value = 3
                ),
                actionButton(
                    inputId = "btn_item",
                    "Terima!",
                    class = "btn-success"
                ),
                tableOutput(
                    outputId = "out_items"
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
                    title = "Belian:", textOutput(
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
                    title = "Belian:", textOutput(
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
                    "Verifikasi | Transaksi (Bulanan)"
                ),
                sliderInput(
                    inputId = "slider_tbmth",
                    label = "Bulan",
                    min = 0,
                    max = 12,
                    value = dm
                ),
                DT::dataTableOutput('tbmth')
            ),
            tabItem(
                tabName = "tab_itembln",
                h1(
                    "Verifikasi | Item (Bulanan)"
                ),
                sliderInput(
                    inputId = "slider_tbitem",
                    label = "Bulan",
                    min = 0,
                    max = 12,
                    value = dm
                ),
                DT::dataTableOutput('tbitem')
            )
        )
    )
)

# server logic ----
server <- function(input, output, session) {
    observeEvent(input$btn_trans, {
        tb <- tibble(date = lubridate::as_datetime(format(input$input_tkh,"%Y-%m-%d")), 
                     type = input$input_type, item = input$input_item, unit = input$input_unit, 
                     price = input$input_price, amount = input$input_amaun)
        
        output$df <- renderTable({
            tb
        })
        
        # insert database
        colSales$insert(tb)
        showModal(modalDialog(
            title = "Notis",
            "Maklumat telah dikemaskini!"
        ))
    })
    
    price <- output$price <- renderText({
        qry <- toJSON(list(item = input$input_item), auto_unbox = TRUE)
        colItem$find(qry, fields = '{"_id": 0, "price": 1}') %>% unlist()
        # colItem$find('{"item": input$input_item}', fields='{"_id":0,"price":1}') %>% unlist()
    })
    
    amount <- reactive({
        # assign variables
        req(input$input_unit, input$input_price) # Ensures both inputs are available before calculation
        input$input_unit * input$input_price
    })
    
    observeEvent(input$input_item,{
        price <- res$price
        # update variable
        updateNumericInput(session, "input_price", value = price())
    })
    
    observeEvent(input$input_price,{
        price <- res$price
        # update variable
        # updateNumericInput(session, "input_price", value = price())
        updateNumericInput(session, "input_amaun", value = amount())
    })

    observeEvent(input$btn_item, {
        tb <- tibble(item = input$input_name_item, price = input$input_price_item)
        output$df <- renderTable({
            tb
        })

        # insert database
        colItem$insert(tb)
        showModal(modalDialog(
            title = "Notis",
            "Maklumat telah dikemaskini!"
        ))
    })
    
    # Display Items collections
    output$out_items <-  renderTable({
        colItem$find('{}')
    })
    
    # Calculate total Sales - Exp
    df <- colSales$find('{}')
    total_sales <- df %>% 
        filter(type == "sales") %>% 
        summarise("Total" = sum(amount)) 
    
    total_exp <- df %>% 
        filter(type == "restock" | type == "misc cost") %>% 
        summarise("Total" = sum(amount))
    
    net_profit <- total_sales$Total - total_exp$Total
    net_margin <- net_profit / total_sales$Total * 100
    output$output_yr_sales <- renderText(total_sales$Total)
    output$output_yr_exp <- renderText(total_exp$Total)
    output$output_yr_net <- renderText(net_profit)
    output$output_yr_margin <- renderText(round(net_margin))
    
    # monthly aggregate
    observeEvent(input$slider_mth,{
        # pipe <- '[{ $project: {date: 1, type: 1, amount: 1, mth: {$month: "$date" }}}, {$match:{ mth: 6}}]'
        
        pipe_list <- list(
            list('$project' = list(date = 1, type = 1, amount = 1, mth = list('$month' = '$date'))),
            list('$match' = list(mth = input$slider_mth))
        )
        pipe <- toJSON(pipe_list, auto_unbox = TRUE)
        
        # Query db with aggregate
        td <- colSales$aggregate(pipe = pipe)
        
        if (nrow(td) > 0) {
            
            mth_sales <- td %>% 
                filter(type == "sales") %>% 
                summarise("Total" = sum(amount))
            
            mth_exp <- td %>% 
                filter(type == "restock" | type == "misc cost") %>% 
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
            # pipe <- '[{ $project: {_id: 0, date: 1, type: 1, amount: 1, mth: {$month: "$date" }}}, {$match:{ mth: 6}}]'
            pipe_list <- list(
                list('$project' = list("_id" = 0, date = 1, type = 1, amount = 1, mth = list('$month' = '$date'))),
                list('$match' = list(mth = input$slider_tbmth))
            )
            pipe <- toJSON(pipe_list, auto_unbox = TRUE)
            
            # Query db with aggregate
            td <- colSales$aggregate(pipe = pipe)
            
            if (nrow(td) > 0) {
            datatable(td[,-4], rownames = FALSE, editable = list(
                target = 'row', disable = list(columns = c(0,3))),
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
    
    # View | Items transaction
    observeEvent(input$slider_tbitem,{
        output$tbitem <- DT::renderDataTable({
            # pipe <- '[{ $project: {_id: 0, date: 1, type: 1, amount: 1, mth: {$month: "$date" }}}, {$match:{ mth: 6}}]'
            pipe_list <- list(
                list('$project' = list("_id" = 0, date = 1, type = 1, item = 1, unit = 1, amount = 1, mth = list('$month' = '$date'))),
                list('$match' = list(mth = input$slider_tbitem))
            )
            pipe <- toJSON(pipe_list, auto_unbox = TRUE)
            
            # Query db with aggregate
            td <- colSales$aggregate(pipe = pipe) %>% 
                select(date, type, item, unit, amount)
            
            if (nrow(td) > 0) {
                datatable(td, rownames = FALSE, editable = list(
                    target = 'row', disable = list(columns = c(0,3))),
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

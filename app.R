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

# read sales data ----
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
    bs4DashSidebar(
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
                    value = 0
                ),
                textOutput("price"),
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
                DTOutput(
                    outputId = "out_items"
                ),
                actionButton(
                    inputId = "btn_items",
                    "Terima!",
                    class = "btn-success"
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
                    inputId = "slider_edtbln",
                    label = "Bulan",
                    min = 0,
                    max = 12,
                    value = dm
                ),
                DT::dataTableOutput('tbitem'),
                tags$h4("Ringkasan"),
                tableOutput('tbitem_sum')
            )
        )
    )
)

# server logic ----
server <- function(input, output, session) {
    # Transaksi Jualan ----
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
    
    price <- renderText({
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
        # update variable
        updateNumericInput(session, "input_price", value = price())
    })
    
    observeEvent(c(input$input_unit, input$input_price),{
        updateNumericInput(session, "input_amaun", value = amount())
    })
    
    data <- dplyr::as_tibble(colItem$find(fields = '{"_id": 0, "item": 1, "price": 1}')) # Fetch all documents
    values <- reactiveValues(data = data)
    
    # Jadual item ----
    output$out_items <- renderDT({
        datatable(values$data, editable = list(target = 'cell', numeric = c(2)), rownames = FALSE)
    })
    
    observeEvent(input$out_items_cell_edit, {
        req(input$out_items_cell_edit)
        info <- input$out_items_cell_edit
        str(info)
        i <- info$row
        j <- info$col + 1
        value <- info$value
        
        # Accessing and updating the reactive values
        values$data[i, j] <- value
        values$data <- values$data  # Trigger reactivity
    })
    
    observeEvent(input$btn_items, {
        
        # Get the edited data
        edited_data <- values$data

        for (i in 1:nrow(edited_data)) {
            row <- edited_data[i, ]
            
            # Construct the query
            query <- toJSON(list(item = row$item), auto_unbox = TRUE)
            update_data <- list(price = row$price)
            
            # Construct the update document
            update <- paste0('{"$set": ', toJSON(update_data, auto_unbox = TRUE), '}')
            
            # Update the document
            colItem$update(query, update)
        }
        
        showModal(modalDialog(title = "Makluman","Item telah dikemaskini!",
                              size = "s",
                              easyClose = TRUE))
    })
    
    # Aggregate values ----
    ## annual Sales | exp ----
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
    
    ## monthly sale | exp ----
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
    
    # Verifikasi | Transaksi (Bulanan) (deprecated) ----
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
    
    # View | Items transaction ----
    observeEvent(input$slider_edtbln,{
        pipe_list <- list(
            list('$project' = list("_id" = 0, date = 1, type = 1, item = 1, unit = 1, amount = 1, mth = list('$month' = '$date'))),
            list('$match' = list(mth = input$slider_edtbln))
        )
        pipe <- toJSON(pipe_list, auto_unbox = TRUE)
 
        output$tbitem <- DT::renderDataTable({
            # pipe <- '[{ $project: {_id: 0, date: 1, type: 1, amount: 1, mth: {$month: "$date" }}}, {$match:{ mth: 6}}]'
            # Query db with aggregate
            td <- colSales$aggregate(pipe = pipe)
            
            if (nrow(td) > 0) {
                datatable(td[,-6], rownames = FALSE, editable = FALSE)
            } else{
                showModal(modalDialog(
                    title = "Maaf",
                    "Tiada Maklumat diketemukan!"
                ))
            }
        })
        
        # aggregate value
        output$tbitem_sum <- renderTable({
            # df <- tibble("Sales" = 123, "Restock" = 34)
            td <- colSales$aggregate(pipe = pipe)
            
            if (nrow(td) > 0 ){
                td %>% group_by(type) %>% 
                    summarise(Total = sum(amount))
            }
        })

    })
}

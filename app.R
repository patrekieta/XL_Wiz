library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(sortable)
library(shinyjs)
library(data.table)
library(openxlsx2)
library(DT)
library(tidyr)
library(rstudioapi)


# vax_data <- read_parquet(paste0("I:/COVID-19/Vaccine/Vaccine Dashboard/Patient_TCPH",Sys.Date()-1,".parquet"))
# vax_data <- vax_data[1:10000,]
data("mtcars")
data("quakes")
data_names <- names(unlist(.GlobalEnv))
data_names <- as.list(data_names[data_names!="data_names" & data_names!=".Random.seed" & data_names != "all_data"])

# envir <- parent.frame()

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Test"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Document Info", tabName = "header", icon = icon("address-card")),
      menuItem("Document", tabName = "document", icon = icon("file-excel")),
      menuItem("Help", tabName = "help", icon = icon("circle-info"))
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(tabName = "header"
              
      ),

      tabItem(tabName = "document",
              fluidRow(
              uiOutput("document_control")
              ),
              fluidRow(
                dataTableOutput("sheet_track")
              )
      ),

      tabItem(tabName = "help")
    )
  )


)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # sheetInput <- function(FUN, id, ...) {
  #   inputs <- as.character(FUN(paste0(id), ...))
  #   inputs
  # }
  
  # shinyInput <- function(FUN, id, ...) {
  #     inputs <- as.character(FUN(paste0(id), ...))
  #   inputs
  # }
  
  track_data <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 2))
  names(track_data) <- c("main","new")
  # track_data[1,1] <- actionButton(inputId = "main_sheet", label = "Main", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)')
  # track_data[1,2] <- actionButton(inputId = "new_sheet", label = "Add New Sheet", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)')
  # track_data[1,1] <- as.character(actionButton,id = "main_sheet", label = "Main")
  track_data[1,1] <- as.character(actionButton(inputId = "main_button", label = "Main", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)'))
  track_data[1,2] <- as.character(actionButton(inputId = "new_sheet", label = "Add New Sheet", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)'))
  # track_data[2,] <- sheetInput(actionButton,ncol(track_data),'button_', label = track_data[1,], onclick = 'Shiny.onInputChange(\"select_button\",  this.id)')
  
  
  main_ui <- fluidPage(fluidRow(column(6,
                                       selectInput(
                                         inputId = "data_select_main",
                                         label = "Select Data for this page",
                                         choices = data_names,
                                         multiple = FALSE)
  ),
  column(6,actionButton("test","test"))
  ),
  fluidRow( 
    column(12, style = "padding:2vmin;", 
           dataTableOutput("tbl_main_head")
    )),
  fluidRow(column(12,style = "padding:2vmin;", dataTableOutput("tbl_main")))
  )

  
  
  sheet_control <- reactiveValues(main = main_ui, sheet = NULL, tracker = track_data, page = 1)
  all_data <- reactiveValues(main = NULL, main_head = NULL)

  observeEvent(input$data_select_main, {
    all_data$main = get(input$data_select_main)
    temp_data = as.data.frame(names(get(input$data_select_main)))
    names(temp_data) <- "1"
    temp_data$id <- row.names(temp_data)
    temp_data <- pivot_wider(data = temp_data, names_from = "id", values_from = "1")
    all_data$main_head <- temp_data
  })

  output$tbl_main <- renderDataTable(
    datatable(all_data$main)
  )
  
  output$tbl_main_head <- renderDataTable(
    datatable(all_data$main_head, editable = TRUE, options = list(dom = 't', headerCallback = JS(
      "function(thead, data, start, end, display){",
      "  $(thead).remove();",
      "}")), rownames = FALSE, caption = "EDIT COLUMN NAMES", selection = "none")
  )
 
  observeEvent(input$tbl_main_head_cell_edit,{
    all_data$main_head[1,input$tbl_main_head_cell_edit$col+1] <- input$tbl_main_head_cell_edit$value
    names(all_data$main) <- all_data$main_head[1,]
  })
  
  output$document_control <- renderUI({
   sheet_control$main
  })
  
  output$sheet_track <- renderDataTable(
    datatable(sheet_control$tracker, options = list(dom = 't', headerCallback = JS(
      "function(thead, data, start, end, display){",
      "  $(thead).remove();",
      "}"))
      , rownames = FALSE, caption = "Select Sheet", selection = "none", escape = FALSE), server = FALSE
  )
  
  # observeEvent(input$new_sheet,{
  #   temp_track <- sheet_control$tracker
  #   new_sheet <- as.data.frame(matrix(data = NA, nrow = 1, ncol=1))
  #   names(new_sheet) <- paste0(sheet_control$page)
  #   new_sheet <- sheetInput(actionButton, paste0("Sheet",sheet_control$page), label = paste0("Sheet",sheet_control$page), onclick = 'Shiny.onInputChange(\"select_button\",  this.id)')
  #   temp_track <- cbind(temp_track,new_sheet)
  #   sheet_control$tracker <- temp_track
  #   print("1")
  # })
  
  observeEvent(input$select_button,{
    
    if(input$select_button == "new_sheet"){
      temp_track <- sheet_control$tracker
        new_sheet <- as.data.frame(matrix(data = NA, nrow = 1, ncol=1))
        names(new_sheet) <- paste0("Sheet",sheet_control$page)

        new_sheet[1,1] <- as.character(actionButton(inputId = paste0("Sheet",sheet_control$page), label = paste0("Sheet",sheet_control$page), onclick = 'Shiny.onInputChange(\"select_button\",  this.id)'))

        temp_track <- cbind(temp_track,new_sheet)
        temp_track <- temp_track[,c("main",paste0("Sheet",1:sheet_control$page),"new")]
        sheet_control$page = sheet_control$page + 1
        sheet_control$tracker <- temp_track
    }
    shinyjs::reset(id = "select_button")
    
  })
  
  observeEvent(input$main_button,{



    print(2)
    shinyjs::reset(id = "select_button")
  })
  # 
  # observe({print(input$main_button)})
  

}

shinyApp(ui = ui, server = server, options = options(port = 0))
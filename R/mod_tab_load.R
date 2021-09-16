#' tab_load UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_load_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Load the CSI dataset"),
    tags$hr(style="border-color: black;"),
    fluidRow(
      box(title = p("Load a CSI Statement", style="color:#B88C4A"),
          fileInput(ns("file_csi"), "Load an .xlsx/.xls file", buttonLabel = "Load file",
                    accept = c(".xlsx", ".xls"))
      )
    ),
    uiOutput(ns("store_csi_UI"))
    
  )
}

#' tab_load Server Functions
#'
#' @noRd 
mod_tab_load_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    rv <- rv(
      csi = NULL,
      stores = NULL,
      csi_date = NULL
    )
    
    # csi ---
    csi <- reactive({
      
      req(input$file_csi)
      
      file = input$file_csi
      
      excel <- readxl::excel_format(file$name)
      
      if(is.na(excel)){
        shinyFeedback::showFeedbackDanger("file_csi", "This is not an excel file")
        return()
      }
      
      temp <- safe_readXL(file$datapath)
      
      if(is.null(temp$result)){
        
        shinyFeedback::hideFeedback("file_csi")
        shinyFeedback::showFeedbackDanger("file_csi", "This is not a valid csi file")
        
        return(NULL)
      }
      
      dta <- temp$result %>% 
        rename(
          date = ...1,
          store_code = ...2
        )
      
      rv$csi_date <- get_csi_date(dta)
      
      out <- process_csi(dta) 
      
      shinyFeedback::hideFeedback("file_csi")
      shinyFeedback::showFeedbackSuccess("file_csi", "All good")
      
      out
      
    })
    
    
    output$store_csi_UI <- renderUI({
      
      req(csi())
      
      tagList(
        fluidRow(
          box(title = "View a store's csi",
              selectInput(ns("store"), "Select a store", 
                          choices = unique(csi()$store_code)
              ),
              DT::DTOutput(ns("store_csi"))
          )
        )
      )
      
    })
    
    # Save to rv for return
    observeEvent(csi(), {
      
      rv$csi    <- csi()
      rv$stores <- unique(csi()$store_code)
      
    })
    
    store_csi <- reactive({
      
      req(csi())
      csi() %>% 
        filter(store_code == input$store) %>% 
        tidyr::nest(data = -store_code) %>% 
        mutate(data =  purrr::map(data,
                                  ~janitor::adorn_totals(., where = "row", fill = "", 
                                                         na.rm = TRUE, name = "Total", -AWB
                                  )
        ) ) %>% 
        filter(store_code == input$store) %>% 
        select(data) %>% 
        tidyr::unnest(cols = c(data))
      
      
    })
    
    output$store_csi <- DT::renderDT({
      
      store_csi() %>% 
        datatable(
          rownames = FALSE,
          options = list(
            paging = FALSE,
            ordering = FALSE
          )
        )
    })
    
    
    
    return(rv)
    
  })
}

## To be copied in the UI
# mod_tab_load_ui("tab_load_ui_1")

## To be copied in the server
# mod_tab_load_server("tab_load_ui_1")

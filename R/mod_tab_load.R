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
          radioButtons(ns("csi_type"), "Type of CSI", choices = c("ACS", "Ticket Hour"), inline = TRUE),
          fileInput(ns("file_csi"), "Load an .xlsx/.xls file", buttonLabel = "Load file",
                    accept = c(".xlsx", ".xls", ".csv"))
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
    
    callModule(mod_downloadTable_server, id = "down_csi", table_name = "CSI", store_csi)
    
    rv <- rv(
      csi = NULL,
      stores = NULL,
      csi_date = NULL,
      csi_type = NULL
    )
    
    
    observeEvent(input$csi_type, {
      rv$csi_type <- input$csi_type
    })
    
    # csi ---
    csi <- reactive({
      
      req(input$file_csi)
      
      file = input$file_csi
      
      file_type <- tools::file_ext(file$name)
      
      csi_type <- input$csi_type
      
      correct_file_type <- switch (csi_type,
                                   "ACS" = c("xls", "xlsx"),
                                   "Ticket Hour" = c("xls", "xlsx"), #"csv",
                                   stop("invalid type", .call = FALSE)
      )
      
      browser()
      if(!file_type %in% correct_file_type ) {
        
        msg <- paste0("This is not a valid ", csi_type, " file. We need a ", paste0(correct_file_type, collapse = '/'), " file")
        
        shinyFeedback::hideFeedback("file_csi")
        shinyFeedback::showFeedbackDanger("file_csi", msg)
        
        return()
      }
      
      dta <- tryCatch(
        
        switch (csi_type,
                       "ACS" = read_acs_csi(file$datapath),
                       "Ticket Hour" = read_ticket_hour(file$datapath),
                       stop("Have you added a new type malaka?", .call = FALSE)
        ),
        
        error = function(e){
          
          error_csi_file(csi_type)
          return()
        }
        
      )
 
      
      # 
      if(is.null(dta)){
        
        error_csi_file(csi_type)
        
        return(NULL)
      }
      
      
      # Finished
      
      shinyFeedback::hideFeedback("file_csi")
      shinyFeedback::showFeedbackSuccess("file_csi", paste0(csi_type, " file read OK!"))
      
      
      
      rv$csi_date <- get_csi_date(dta, csi_type)
      
      
      # Extra Processing of the files
      
      dta <- switch (csi_type,
                     
                     "ACS" = process_csi(dta),
                     
                     "Ticket Hour" = dta,
                     
                     stop("Have you added a new type malaka?", .call = FALSE)
      ) 
      
      
      dta
      
    })
    
    
    output$store_csi_UI <- renderUI({
      
      req(csi())
      
      tagList(
        fluidRow(
          box(title = "View a store's csi", width = 10,
              selectInput(ns("store"), "Select a store", 
                          choices = unique(csi()$store_code)
              ),
              mod_downloadTable_ui(ns("down_csi")),
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
        {
          if(input$csi_type == "ACS") {
            mutate(., data =  purrr::map(data, ~add_totals(., -AWB)))
          } else {
            mutate(., data =  purrr::map(data, ~add_totals(., all_of(vars_sum_ticketHour))))
          }
        } %>% 
        filter(store_code == input$store) %>% 
        select(data) %>% 
        tidyr::unnest(cols = c(data)) %>% 
        #rename if TH
        {
          if(input$csi_type == "ACS") {
            .
          } else {
            select(., any_of(col_names_ticket))
          }
        }
        
      
      
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

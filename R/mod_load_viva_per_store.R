#' load_viva_per_store UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_load_viva_per_store_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    box(width = 12,
        title = tags$b("Load a VIVA statement"),
        fileInput(ns("file_statement"), "Load an .csv file", buttonLabel = "Load file", accept = c(".csv")),
        shinyjs::hidden(
          div(
            id = ns("statement_date_div"),
            hr(),
            mod_downloadTable_ui(ns("down_statement"), 'Download statement as .xlsx')
          )
        )
    )
    # box(title = "VIVA per Store report", width = 9,
    #     DT::DTOutput(ns("statement"))
    # )
  )
}
    
#' load_viva_per_store Server Functions
#'
#' @noRd 
mod_load_viva_per_store_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    statement_type <- 'viva_per_store'
    
    correct_file_type <- c("csv")
    
    rv <- rv(
      statement = NULL,
      statement_type = statement_type,
      statement_date = NULL
    )
    
    
    callModule(mod_downloadTable_server, id = "down_statement", table_name = "VIVA_PER_STORE_STATEMENT", statement)
    
    observeEvent(statement(), {
      
      req(statement())
      
      shinyjs::show("statement_date_div")
      
    })
    
    
    # DATA ---
    statement <- reactive({
      
      req(input$file_statement)
      
      file = input$file_statement
      
      file_type <- tools::file_ext(file$name)
      
      statement_type <- input$statement_type
      
      if(!file_type %in% correct_file_type ) {
        
        msg <- paste0("This is not a valid ", statement_type, " file. We need a ", paste0(correct_file_type, collapse = '/'), " file")
        
        shinyFeedback::hideFeedback("file_statement")
        shinyFeedback::showFeedbackDanger("file_statement", msg)
        
        return(NULL)
      }
      
      dta <- tryCatch({
        
        read_viva_statement(file$datapath)
        
      }, error = function(e){
        
        error_statement_type(statement_type, session)
        
        return(NULL)
        
      })
      
      
      # 
      if(is.null(dta)){
        
        error_statement_type(statement_type, session)
        
        return(NULL)
      }
      
      
      # Finished
      
      shinyFeedback::hideFeedback("file_statement")
      shinyFeedback::showFeedbackSuccess("file_statement", paste0(statement_type, " file read OK!"))
      
      # Extra Processing of the files
      
      dta |> 
        filter(`Transaction Type` == 'SA') |> 
        mutate(
          `ACS Account Code/Store` = stringr::str_remove(`ACS Account Code/Store`, "-1$"),
          `ACS Account Code/Store` = stringr::str_trim(`ACS Account Code/Store`)
        ) |> 
        rename(
          store = `ACS Account Code/Store`
        )
      
    })
    
    
    # Save to rv for return
    observeEvent(statement(), {
      
      rv$statement <- statement()
      
    })
    
    output$statement <- DT::renderDT({
      
      statement() %>% 
        #select(any_of(names_cashier_per_store)) |> 
        datatable(
          rownames = FALSE,
          options = list(
            paging = TRUE,
            ordering = FALSE,
            pageLength = 5
          )
        )
    })
    
    
    # observeEvent(input$statement_date, {
    #   
    #   req(input$statement_date)
    #   
    #   shinyFeedback::hideFeedback("statement_date", session)
    #   
    #   rv$statement_date <- input$statement_date
    #   
    # })
    
    return(rv)
    
  })
}
    
## To be copied in the UI
# mod_load_viva_per_store_ui("load_viva_per_store_1")
    
## To be copied in the server
# mod_load_viva_per_store_server("load_viva_per_store_1")

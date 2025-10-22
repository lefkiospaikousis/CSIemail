#' cashier_monitoring UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cashier_monitoring_ui <- function(id){
  ns <- NS(id)
  
  
  tagList(
    
    fluidRow(
      mod_load_cashier_per_store_ui(ns("load_cashier_per_store_1"))
    ),
    
    fluidRow(
      mod_load_moneygram_statement_ui(ns("load_moneygram_statement_1"))
    ),
    
    fluidRow(
      actionButton(ns("generate report"), "Generate Report", icon = icon("database"), class = "btn btn-primary" ),
    )
    
  )
}

#' cashier_monitoring Server Functions
#'
#' @noRd 
mod_cashier_monitoring_server <- function(id){
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    res_load_moneygram <- mod_load_moneygram_statement_server("load_moneygram_statement_1")
    
    res_load_cashier_per_store <- mod_load_cashier_per_store_server("load_cashier_per_store_1")
    
    
    statements <- rv(
      cashier_per_store = NULL,
      moneygram = NULL
    )
    
    observeEvent(res_load_moneygram$statement, {
      
      req(res_load_moneygram$statement)
      
      showNotification("A Moneygram statement has been loaded", type = "message")
      
      statements$moneygram <- res_load_moneygram$statement
      
    })
    
    
    observeEvent(res_load_cashier_per_store$statement, {
      
      req(res_load_cashier_per_store$statement)
      
      showNotification("A Cashier per Store statement has been loaded", type = "message")
      
      statements$cashier_per_store <- res_load_cashier_per_store$statement
      
    })
    
    
    # observeEvent(input$statement_type, {
    #   rv$statement_type <- input$statement_type
    # })
    
    
    observeEvent(input$add_to_db, {
      
      req(rv$statement)
      
      if(is.null(rv$statement_date)){
        
        shinyFeedback::hideFeedback("statement_date", session)
        shinyFeedback::showFeedbackDanger("statement_date", session = session,
                                          paste0("Add a statement date"))
        
        showNotification("Please add the statement date", type = "error")
        return()
      }
      
      # Check if the date is already in the DB
      already_in_db <- check_statement_in_db(rv$statement_date, input$statement_type)
      
      if(already_in_db){
        
        msg <- glue::glue(
          "A statement of type '{rv$statement_type}' for date {format(rv$statement_date, '%d/%m/%Y')}
          is already in the Database. Please change the date"
        )
        
        message_failure(msg)
        
        return()
        
      } 
      
      
      # Add to DB
      
      new_statement <- rv$statement |> 
        # rename before entering the DB
        select(store, all_of(names_cashier_per_store)) |>
        mutate(
          date = as.character(rv$statement_date)
          #added_by = current_user()$user
        )
      
      
      res_append <- append_to_db(new_statement, input$statement_type)
      
      if(isFALSE(res_append)){
        
        message_failure('Something went wrong when adding the statement to the DataBase. Please try again')
        return()
      }
      
      msg <- glue::glue(
        "The statement of type '{rv$statement_type}' for date {format(rv$statement_date, '%d/%m/%Y')}
        has been added to the Database"
      )
      
      
      showNotification(msg, type = "message")
      message_success(msg)
      
      
      # Reset everything
      rv$statement <- NULL
      rv$statement_type <- NULL
      rv$statement_date <- NULL
      
      shinyjs::hide("statement_date_div")
      
      #updateRadioButtons(session, "statement_type", selected = character(0))
      updateDateInput(session, "statement_date", value = NA)
      shinyjs::reset("file_statement")
      
      shinyFeedback::hideFeedback("file_statement")
      shinyFeedback::hideFeedback("add_to_db")
      
    })
    
    
    return(rv)
    
    
  })
}

## To be copied in the UI
# mod_cashier_monitoring_ui("cashier_monitoring_1")

## To be copied in the server
# mod_cashier_monitoring_server("cashier_monitoring_1")

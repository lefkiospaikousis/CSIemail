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
      box(width = 3,
          title = h3("Load a Monitoring Statement"),
          #p('Load a Monitoring Statement'),
          radioButtons(ns("statement_type"), "Type of statement", choices = statement_types),
          fileInput(ns("file_statement"), "Load an .xlsx/.xls file", buttonLabel = "Load file", accept = c(".xlsx", ".xls")),
          shinyjs::hidden(
            div(
              id = ns("statement_date_div"),
              hr(),
              dateInput(ns("statement_date"), 'Statement Date', format = "dd/mm/yyyy", value = NA),
              mod_downloadTable_ui(ns("down_statement"), 'Download statement as .xlsx'),
              hr(),
              actionButton(ns("add_to_db"), "Done (add to DB)", class = 'btn-submit', width = '100%')
            )
          ),
      ),
      box(title = "Statement", width = 9,
          #selectInput(ns("store"), "Select a store", choices = unique(dta()$store_code)),
          
          DT::DTOutput(ns("statement"))
      )
    )
  )
}

#' cashier_monitoring Server Functions
#'
#' @noRd 
mod_cashier_monitoring_server <- function(id){
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    callModule(mod_downloadTable_server, id = "down_statement", table_name = "STATEMENT", statement)
    
    correct_file_type <- c("xls", "xlsx")
    
    rv <- rv(
      statement = NULL,
      statement_type = NULL,
      statement_date = NULL
    )
    
    
    observeEvent(input$statement_type, {
      rv$statement_type <- input$statement_type
    })
    
    # if the file is read ok then show the date input 
    
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
        
        
        switch (statement_type,
                'cashier_per_store' = read_monitoring_statement(file$datapath),
                stop("Have you added a new type malaka?", .call = FALSE)
        )
        
      },error = function(e){
        
        error_statement_type(statement_type, session)
        
        return(NULL)
        
      }
      
      )
      
      
      # 
      if(is.null(dta)){
        
        error_statement_type(statement_type, session)
        
        return(NULL)
      }
      
      
      # Finished
      
      shinyFeedback::hideFeedback("file_statement")
      shinyFeedback::showFeedbackSuccess("file_statement", paste0(statement_type, " file read OK!"))
      
      
      # Extra Processing of the files
      
      dta
      
    })
    
    
    # Save to rv for return
    observeEvent(statement(), {
      
      rv$statement <- statement()
      rv$statement_date <- NULL
      updateDateInput(session, "statement_date", value = NA)
    })
    
    
    output$statement <- DT::renderDT({
      
      statement() %>% 
        #select(any_of(names_cashier_monitoring)) |> 
        datatable(
          rownames = FALSE,
          options = list(
            paging = TRUE,
            ordering = FALSE
          )
        )
    })
    
    observeEvent(input$statement_date, {
      
      req(input$statement_date)
      
      shinyFeedback::hideFeedback("statement_date", session)
      
      rv$statement_date <- input$statement_date
      
    })
    
    
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
        select(store, all_of(names_cashier_monitoring)) |>
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

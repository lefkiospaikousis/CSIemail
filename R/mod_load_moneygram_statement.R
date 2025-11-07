#' load_moneygram_statement UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_load_moneygram_statement_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    box(width = 12,
        title = tags$b("Load a Moneygram Statement"),
        fileInput(ns("file_statement"), "Load an .xlsx/.xls file", buttonLabel = "Load file", accept = c(".xlsx", ".xls")),
        shinyjs::hidden(
          div(
            id = ns("statement_date_div"),
            hr(),
            mod_downloadTable_ui(ns("down_statement"), 'Download statement as .xlsx')
            
          )
        )
    )
    # box(title = "Moneygram statement", width = 9,
    #     DT::DTOutput(ns("statement"))
    # )
    
  )
}

#' load_moneygram_statement Server Functions
#'
#' @noRd 
mod_load_moneygram_statement_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    statement_type <- 'moneygram'
    
    correct_file_type <- c("xls", "xlsx")
    
    rv <- rv(
      statement = NULL,
      statement_type = statement_type,
      statement_date = NULL
    )
    
    callModule(mod_downloadTable_server, id = "down_statement", table_name = "MONEYGRAM_STATEMENT", statement)
    
    
    observeEvent(statement(), {
      
      req(statement())
      
      shinyjs::show("statement_date_div")
      
    })
    
    
    # DATA ---
    statement <- reactive({
      
      req(input$file_statement)
      
      file = input$file_statement
      
      file_type <- tools::file_ext(file$name)
      
      rv$statement_date <- moneygram_date(input$file_statement$name)
      
      if(!file_type %in% correct_file_type ) {
        
        msg <- paste0("This is not a valid ", statement_type, " file. We need a ", paste0(correct_file_type, collapse = '/'), " file")
        
        shinyFeedback::hideFeedback("file_statement")
        shinyFeedback::showFeedbackDanger("file_statement", msg)
        
        return(NULL)
      }
      
      dta <- tryCatch({
        
        read_moneygram_statement(file$datapath)
        
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
      
      
      
      rv$statement <- statement() |> 
        # add the store id
        left_join(
          session$userData$moneygram_stores |> select(agent_id, store), 
          by = c("agent_id")
          ) |> 
        select(store, everything())
      
    })
    
    
    output$statement <- DT::renderDT({
      
      rv$statement %>% 
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
    
    
    
    return(rv)
    
    
  })
}

## To be copied in the UI
# mod_load_moneygram_statement_ui("load_moneygram_statement_1")

## To be copied in the server
# mod_load_moneygram_statement_server("load_moneygram_statement_1")

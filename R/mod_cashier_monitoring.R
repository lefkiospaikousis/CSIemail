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
  
  # For the progress modal
  tagList(
    tags$script(HTML("
    Shiny.addCustomMessageHandler('updateProgress', function(content) {
      $('#progress_content').html(content);
    });
  ")),
    
    fluidRow(
      column(3,
             
             mod_load_cashier_per_store_ui(ns("load_cashier_per_store_1")),
             
             mod_load_moneygram_statement_ui(ns("load_moneygram_statement_1")),
             
             mod_load_viva_per_store_ui(ns("load_viva_per_store_1")),
             
      ),
      column(9,
             box(title = tags$b("Loaded files"), 
                 width = 12,
                 
                 tabsetPanel(
                   id = ns('tabs_loaded_statements'),
                   tabPanel(
                     title = "Cashier per Store",
                     br(),
                     DT::DTOutput(ns("statement_cashier_per_store"))
                   ),
                   tabPanel(
                     title = "Moneygram Statement",
                     br(),
                     DT::DTOutput(ns("statement_moneygram"))
                   ),
                   tabPanel(
                     title = "VIVA statement",
                     br(),
                     DT::DTOutput(ns("statement_viva_per_store"))
                   )
                 )
             )
             
      )
      
    ),
    fluidRow(
      column(3,
             hr(),
             box(
               width = 12,
               title = tags$b("Generate Cashier Monitoring Templates"),
               "Once Cashier report and  Moneygram statements are loaded, click the button below to 
               generate the Excel templates per city and email them directly
               to the city cashiers",
               br(),
               br(),
               shinyjs::disabled(
                 actionButton(ns("generate_reports"), "Generate Reports and Email them", 
                              width = "100%", icon = icon("industry"), 
                              class = "btn-generate" )
               )
             )
      )
    )
    
  )
}

#' cashier_monitoring Server Functions
#'
#' @noRd 
mod_cashier_monitoring_server <- function(id, dbase_csi){
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    # Reactive values to track progress
    progress <- reactiveValues(
      step = 0,  # 0 = not started (will show Preparing..), 1 = (actual) preparing, 2 = done
      current_report = 1,
      total_reports = NULL,
      messages = character(0),
      errors = 0,
      is_running = FALSE,
      is_complete = FALSE
    )
    
    statements <- rv(
      cashier_per_store = NULL,
      moneygram = NULL,
      viva_per_store = NULL
    )
    
    city_emails <- reactive({
      
      if(golem::app_dev()){
        db_table <- "city_emails2"
      } else {
        db_table <- db_tables[["city_emails"]]
      }
      
      dbase_csi |> 
        tbl(db_table) |> 
        collect()
      
    })
    
    store_groups <- reactive({
      dbase_csi |> 
        tbl(db_tables[["cashier_groups"]]) |> 
        collect()
    })
    
    cities <- reactive({
      unique(store_groups()$city)
    })
    
    observeEvent(store_groups(), {
      
      progress$total_reports <- length(unique(store_groups()$city))
      
    })
    
    observe({
      
      # Enable the generate reports button only if both statements are loaded
      if (!is.null(statements$cashier_per_store) && !is.null(statements$moneygram)) {
        shinyjs::enable("generate_reports")
      } else {
        shinyjs::disable("generate_reports")
      }
      
    })
    
    res_load_moneygram <- mod_load_moneygram_statement_server("load_moneygram_statement_1")
    
    res_load_cashier_per_store <- mod_load_cashier_per_store_server("load_cashier_per_store_1")
    
    res_load_viva_per_store <- mod_load_viva_per_store_server("load_viva_per_store_1")
    
    
    observeEvent(res_load_moneygram$statement, {
      
      req(res_load_moneygram$statement)
      
      showNotification("A Moneygram statement has been loaded", type = "message")
      
      statements$moneygram <- res_load_moneygram$statement
      
      updateTabsetPanel(session, inputId = "tabs_loaded_statements", selected = "Moneygram Statement")
      
    })
    
    
    observeEvent(res_load_cashier_per_store$statement, {
      
      req(res_load_cashier_per_store$statement)
      
      showNotification("A Cashier per Store statement has been loaded", type = "message")
      
      statements$cashier_per_store <- res_load_cashier_per_store$statement
      
      updateTabsetPanel(session, inputId = "tabs_loaded_statements", selected = "Cashier per Store")
      
    })
    
    observeEvent(res_load_viva_per_store$statement, {
      
      req(res_load_viva_per_store$statement)
      
      showNotification("A VIVA per Store statement has been loaded", type = "message")
      
      # Currently not used in report generation
      statements$viva_per_store <- res_load_viva_per_store$statement
      
      updateTabsetPanel(session, inputId = "tabs_loaded_statements", selected = "VIVA statement")
      
    })
    
    
    output$statement_moneygram <- DT::renderDT({
      
      validate(need(!is.null(statements$moneygram), "No Moneygram statement loaded"))
      req(statements$moneygram)
      
      DT::datatable(
        statements$moneygram,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
      
    })
    
    output$statement_cashier_per_store <- DT::renderDT({
      
      validate(need(!is.null(statements$cashier_per_store), "No Cashier per Store statement loaded"))
      
      req(statements$cashier_per_store)
      
      DT::datatable(
        statements$cashier_per_store,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
      
    })
    
    output$statement_viva_per_store <- DT::renderDT({
      
      validate(need(!is.null(statements$viva_per_store), "No VIVA statement loaded"))
      req(statements$viva_per_store)
      
      DT::datatable(
        statements$viva_per_store,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
      
    })
    
    # GENERATE REPORTS -------------------------------------------------------
    
    # Timer to control the process
    timer <- reactiveTimer(500)  # Check every 500ms
    
    # Observer for progress changes to update modal
    observe({
      
      if (length(progress$messages) > 0) {
        message_html <- paste0("<p>", paste(progress$messages, collapse = "</p><p>"), "</p>")
        
        # Add progress bar
        progress_percent <- round(((progress$current_report - 1) / progress$total_reports) * 100)
        if (progress$is_complete) progress_percent <- 100
        
        progress_bar <- paste0(
          '<div class="progress mb-3">',
          '<div class="progress-bar" role="progressbar" style="width: ', progress_percent, '%">',
          progress_percent, '%',
          '</div></div>'
        )
        
        content <- paste0(
          progress_bar,
          '<div style="max-height: 200px; overflow-y: auto;">',
          message_html,
          '</div>'
        )
        
        # Update the modal content
        session$sendCustomMessage("updateProgress", content)
      }
    })
    
    
    # Main process controller. Triggered by a timer only to see
    # at what stage we are at and trigger calculation of next report
    # Depends only on timer (see bindEvent())
    observe({
      
      if (progress$is_running && !progress$is_complete) {
        
        if (progress$step == 0) {
          
          # Show preparing message for current report
          report_name <- cities()[progress$current_report]
          
          # progress$messages <- c(progress$messages, paste(icon('info', style = 'color:blue;font-size:14px'),
          #                                                  "Preparing", report_name, "..."))
          
          progress$step <- 1
          
        } else if (progress$step == 1) {
          
          # Generate the report
          report_name <- cities()[progress$current_report]
          
          # This will block, but the "Preparing" message is already shown
          tryCatch({
            
            city = cities()[progress$current_report]
            
            stores_in_city <- isolate(store_groups()) |> 
              filter(city == !!city) |> 
              pull(store)
            
            dta_cashier_city <- isolate(statements$cashier_per_store) |> 
              filter(store %in% stores_in_city) |> 
              # rename first
              select(store, all_of(names_cashier_per_store)) |> 
              select(store, courier, total_cash, total_card)
            
            dta_moneygram_city <- isolate(statements$moneygram) |> 
              filter(store %in% stores_in_city)
            
            dta_viva_city <- isolate(statements$viva_per_store) |> 
              select(store, total = Amount) |> 
              group_by(store) |>
              summarise(
                total = sum(total, na.rm = TRUE),
                .groups = 'drop'
              ) |> 
              filter(store %in% stores_in_city)
            
            # I re-load every time. Object Oriented and by reference
            # and messes up the next iteration if just used the same
            # copyWorkkbbok does not work as intented . WTF?
            wb <-  openxlsx::loadWorkbook(get_golem_config('cashier_template'))
            
            prepare_store_monitoring(
              wb,
              dta_cashier_city,
              dta_moneygram_city,
              dta_viva_city,
              city = city
            )
            
            file_name <- glue::glue('{get_golem_config("path_store_reports")}/TAMEIAKH_{city}-{Sys.Date()}.xlsx')
            
            openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)
            
            # Send Email  
            #city = 'Nicosia'
           
            recipients <- city_emails() |> 
              filter(city == !!city) |> 
              pull(email)
            
            email_cashier(
              city = city,
              recipients = recipients,
              path_attachement = file_name,
              mail_credentials = mail_credentials()
            )
            
            
            rm(wb)
            
            #if(city == 'Nicosia') stop("Some issue here")
            
            progress$messages <- c(progress$messages, paste(icon('check', style = 'color:green;font-size:14px'), 
                                                            " Done with ", report_name, " and sent email"))
            
          }, error = function(e) {
            
            progress$errors <- progress$errors + 1
            
            progress$messages <- c(progress$messages, 
                                   paste(icon('xmark', style = 'color:red;font-size:14px'),
                                         " Error when preparing ", report_name, ": Error message: ", e$message))
            
          })
          
          progress$step <- 2
          
        } else if (progress$step == 2) {
          
          # Move to next report or finish
          if (progress$current_report < progress$total_reports) {
            
            progress$current_report <- progress$current_report + 1
            progress$step <- 0  # Start preparing next report
            
            
          } else {
            
            # All reports done
            
            if(progress$errors > 0){
              progress$messages <- c(progress$messages, 
                                     paste(icon('exclamation-triangle', style = 'color:orange;font-size:20px'), 
                                           glue::glue(" Completed with {progress$errors} errors. 
                                                      Please check the messages above.")))
            } else {
              
              progress$messages <- c(progress$messages, 
                                     paste(icon('circle-check', style = 'color:green;font-size:20px'), 
                                           " All reports completed successfully!"))
            } 
            
            progress$is_complete <- TRUE
            progress$is_running <- FALSE
            
            
            shinyjs::show("close_modal")
            
          }
        }
      }
      
    }) |> 
      bindEvent(timer())
    
    
    # Launch the Modal and indicate that process started
    observeEvent(input$generate_reports, {
      
      req(statements$cashier_per_store)
      req(statements$moneygram)
      
      # Reset progress
      progress$step <- 0
      progress$current_report <- 1
      progress$total_reports <- length(cities())
      progress$messages <- character(0)
      progress$is_running <- FALSE
      progress$is_complete <- FALSE
      
      # Show modal
      showModal(modalDialog(
        title = "Generating Reports",
        div(
          id = "progress_content",
          p("Starting report generation..."),
          div(id = "progress_messages")
        ),
        footer = tagList(
          shinyjs::hidden(
            actionButton(ns("close_modal"), "Close", class = "btn btn-secondary")
          )),
        easyClose = FALSE,
        fade = FALSE
      ))
      
      # Start the process
      progress$is_running <- TRUE
      
    })
    
    
    observeEvent(input$close_modal, {
      removeModal()
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

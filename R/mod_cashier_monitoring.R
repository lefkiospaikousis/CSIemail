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
      mod_load_cashier_per_store_ui(ns("load_cashier_per_store_1"))
    ),
    
    fluidRow(
      mod_load_moneygram_statement_ui(ns("load_moneygram_statement_1"))
    ),
    
    fluidRow(
      mod_load_viva_per_store_ui(ns("load_viva_per_store_1"))
    ),
    
    
    # add space before the box and space after
    fluidRow(
      #column(3),
      box(
        width = 3,
        title = tags$b("Generate Cashier Monitoring Templates"),
        "Once both statements are loaded, click the button below to generate the Excel templates per city and email them directly
        to the city cashiers",
        br(),
        br(),
        shinyjs::disabled(
          actionButton(ns("generate_reports"), "Generate Reports and Email them", width = "100%", icon = icon("industry"), 
                       class = "btn btn-submit" )
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
      is_running = FALSE,
      is_complete = FALSE
    )
    
    statements <- rv(
      cashier_per_store = NULL,
      moneygram = NULL,
      viva_per_store = NULL
    )
    
    store_emails <- reactive({
      dbase_csi |> 
        tbl(db_tables[["city_emails"]]) |> 
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
      
    })
    
    
    observeEvent(res_load_cashier_per_store$statement, {
      
      req(res_load_cashier_per_store$statement)
      
      showNotification("A Cashier per Store statement has been loaded", type = "message")
      
      statements$cashier_per_store <- res_load_cashier_per_store$statement
      
    })
    
    observeEvent(res_load_viva_per_store$statement, {
      
      req(res_load_viva_per_store$statement)
      
      showNotification("A VIVA per Store statement has been loaded", type = "message")
      
      # Currently not used in report generation
      statements$viva_per_store <- res_load_viva_per_store$statement
      
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
            
            # I re-load every time. Object Oriented and by reference
            # and messes up the next iteration if just used the same
            # copyWorkkbbok does not work as intented . WTF?
            wb <-  openxlsx::loadWorkbook('data-raw/template_report.xlsx')
            
            prepare_store_monitoring(
              wb,
              dta_cashier_city,
              dta_moneygram_city,
              city = city
            )
            
            file_name <- glue::glue('SampleData/TAMEIAKH_{city}-{Sys.Date()}.xlsx')
            
            openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)
            
            rm(wb)
            
            progress$messages <- c(progress$messages, paste(icon('check', style = 'color:green;font-size:14px'), 
                                                            " Done:", report_name))
            
          }, error = function(e) {
            progress$messages <- c(progress$messages, paste(icon('xmark-check', style = 'color:red;font-size:14px'),
                                                            " Error in", report_name, ":", e$message))
          })
          
          progress$step <- 2
          
        } else if (progress$step == 2) {
          
          # Move to next report or finish
          if (progress$current_report < progress$total_reports) {
            
            progress$current_report <- progress$current_report + 1
            progress$step <- 0  # Start preparing next report
            
            
          } else {
            
            # All reports done
            progress$messages <- c(progress$messages, paste(icon('circle-check', style = 'color:green;font-size:20px'), 
                                                            " All reports completed successfully!"))
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



# observeEvent(input$generate_report, {
#   
#   req(statements$cashier_per_store)
#   req(statements$moneygram)
#   
#   
#   # Show modal
#   showModal(modalDialog(
#     title = "Generating Reports",
#     div(
#       id = "progress_content",
#       p("Starting report generation..."),
#       div(id = "progress_messages")
#     ),
#     footer = tagList(
#       shinyjs::hidden(
#         actionButton(ns("close_modal"), "Close", class = "btn btn-secondary")
#       )),
#     easyClose = FALSE,
#     fade = FALSE
#   ))
#   
#   
#   # Reset progress
#   progress$current_report <- 0
#   progress$messages <- character(0)
#   progress$is_complete <- FALSE
#   
#   showNotification("Generating the Cashier Monitoring Report...", type = "message")
#   
#   # Show modal
#   showModal(modalDialog(
#     title = "Generating Reports per City",
#     div(
#       id = "progress_content",
#       p("Starting report generation..."),
#       div(id = "progress_messages")
#     ),
#     footer = tagList(
#       shinyjs::hidden(
#         actionButton(ns("close_modal"), "Close", class = "btn btn-secondary")
#       )),
#     easyClose = FALSE,
#     fade = FALSE
#   ))
#   
#   # Generate reports sequentially
#   total_reports <- isolate(progress$total_reports)
#   
#   cities <- unique(store_groups()$city)
#   
#   
#   for (i in 1:total_reports) {
#     
#     #later::later(
#     # local({
#     
#     report_num <- i
#     
#     city = cities[i]
#     
#     #function() {
#     
#     #for (city in cities) {
#     
#     message("Processing city: ", city)
#     
#     # Add "preparing" message
#     progress$messages <- c(isolate(progress$messages), 
#                            glue::glue("Preparing report for `{city}`..."))
#     
#     stores_in_city <- isolate(store_groups()) |> 
#       filter(city == !!city) |> 
#       pull(store)
#     
#     dta_cashier_city <- isolate(statements$cashier_per_store) |> 
#       filter(store %in% stores_in_city) |> 
#       # rename first
#       select(store, all_of(names_cashier_per_store)) |> 
#       select(store, courier, total_cash, total_card)
#     
#     dta_moneygram_city <- isolate(statements$moneygram) |> 
#       filter(store %in% stores_in_city)
#     
#     # I re-load every time. This is oo and by reference
#     # and messes up the next iteration if just used the same
#     # copyWorkkbbok does not work as intented . WTF
#     wb <-  openxlsx::loadWorkbook('data-raw/template_report.xlsx')
#     
#     prepare_store_monitoring(
#       wb,
#       dta_cashier_city,
#       dta_moneygram_city,
#       city = city
#     )
#     
#     #Sys.sleep(5)
#     
#     openxlsx::saveWorkbook(wb, glue::glue('SampleData/test_report_{city}-{Sys.Date()}.xlsx'), overwrite = TRUE)
#     
#     rm(wb)
#     
#     #}
#     
#     # Add "done" message
#     # progress$messages <- c(isolate(progress$messages),  
#     #                        glue::glue("Done with the report for `{city}`"))
#     
#     # Update progress
#     progress$current_report <- report_num
#     
#     # Check if all reports are complete
#     if (report_num == total_reports) {
#       
#       progress$messages <- c(isolate(progress$messages),
#                              paste0(icon('circle-check', style = 'color:green;font-size:20px'),
#                                     " All reports completed successfully!"))
#       
#       progress$is_complete <- TRUE
#       
#       
#       # Enable close button
#       shinyjs::show("close_modal")
#       
#       
#       
#       # Close modal after showing completion message
#       # later::later(function() {
#       #   removeModal()
#       # }, delay = 2)
#     }
#     #}
#     
#     #@}), delay = (i - 1) * 2 + 0.5)  # Stagger reports by 2 seconds each, start after 0.5s
#   }
#   
#   
# })

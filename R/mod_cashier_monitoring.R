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
    tags$script(HTML("
    Shiny.addCustomMessageHandler('updateProgressEmail', function(content) {
      $('#email_progress_content').html(content);
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
               generate the Excel templates per city and email them email them to the city cashiers",
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
    
    files_to_send <- reactiveVal(NULL)
    
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
        
        #message_html <- paste0("<p>", paste(progress$messages, collapse = "</p><p>"), "</p>")
        message_html <- paste0("<p>", paste(progress$messages, collapse = " | "), "</p>")
        
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
          '<div style="max-height: 300px; overflow-y: auto;">',
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
            
            #if(city == 'Nicosia' || city == 'Limassol') stop('serious issue')
            
            file_name <- glue::glue('{get_golem_config("path_store_reports")}/TAMEIAKH_{city}-{Sys.Date()}.xlsx')
            
            openxlsx::saveWorkbook(wb, file_name, overwrite = TRUE)
            
            new_file <- data.frame(city = city, file_name = file_name)
            
            files_to_send(isolate(bind_rows(files_to_send(), new_file)))
            
            rm(wb)
            
            progress$messages <- c(progress$messages, paste(icon_success(), ' ', report_name, ' done!'))
            
          }, error = function(e) {
            
            progress$errors <- progress$errors + 1
            
            progress$messages <- c(progress$messages, paste(icon_failure(), 
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
                                     paste("<br> <br>",
                                           icon_warning(),  
                                           glue::glue(" Completed with {progress$errors} errors. 
                                                      Please check the messages above.")))
            } else {
              
              progress$messages <- c(progress$messages, 
                                     paste("<br> <br>", icon_success_round(), " All reports created successfully!"))
            } 
            
            progress$is_complete <- TRUE
            progress$is_running <- FALSE
            
            
            shinyjs::show('download_all_reports', anim = TRUE)
            shinyjs::show("block_send_emails", anim = TRUE)
            
            updateCheckboxGroupInput(
              session,
              "cities_to_send",
              choices = files_to_send()$city,
              selected = files_to_send()$city
            )
            
            shinyjs::show("close_modal")
            
          }
        }
      }
      
    }) |> 
      bindEvent(timer())
    
    
    observeEvent(input$send_emails, {
      
      req(files_to_send())
      
      if(is.null(input$cities_to_send) || length(input$cities_to_send) == 0){
        showNotification("Please select at least one city to send the emails", type = "error")
        return()
      }
      
      # Clear the email progress content
      
      session$sendCustomMessage("updateProgressEmail", "<p>Starting to send emails. Please wait...</p>")
      
      #Send Emails with attachments
      
      cities <- input$cities_to_send |> purrr::set_names()
      
      res <- purrr::map(cities, function(city) {
        
        tryCatch({
          
          file_name <- files_to_send()$file_name[files_to_send()$city == city]
          
          recipients <- city_emails() |>
            filter(city == !!city) |>
            pull(email)
          
          #if(city == 'Nicosia' || city == 'Limassol') stop('serious issue')
          
          email_cashier(
            city = city,
            recipients = recipients,
            path_attachement = file_name,
            mail_credentials = mail_credentials()
          )
          
          showNotification(glue::glue("Email sent successfully to {city}"), type = "message")
          
          return("OK")
          
        }, error = function(e) {
          
          showNotification(glue::glue("Error when sending email to {city}"), type = "error")
          
          e$message
          
        })
        
      })
      
      
      cities_succeeded <- names(res)[res == "OK"]
      cities_failed <- names(res)[res != "OK"]
      
      msg_success <- paste0(icon_success(), " ", cities_succeeded, ' sent!') |> paste(collapse = ' | ')
      
      msg_errors <- ''
      
      for (city in cities_failed) {
        error_message <- res[[city]]
        msg_errors <- c(msg_errors, paste0(icon_failure(), city, ' | Error message: ', error_message))
      }
      
      content <- paste0("<p><p>", paste(c(msg_success, msg_errors ), collapse = "</p><p>"), "</p>")
      
      
      # All reports done
      
      if(length(cities_failed) == 0){
        
        msg_end <- paste(icon_success_round(), " All reports were sent successfully!")
        
      } else {
        
        msg_end <- paste(icon_warning(),
                         glue::glue(" Completed with {length(cities_failed)} errors. Please check the messages above."))
        
      } 
      
      content <- paste0(content, "<p>", msg_end, "</p>")
      
      session$sendCustomMessage("updateProgressEmail", content)
      
    })
    
    
    output$download_all_reports <- downloadHandler(
      
      filename = function() {
        
        paste0("Cashier_Reports_", Sys.Date(), ".zip")
        
      },
      
      content = function(file) {
        
        req(files_to_send())
        
        # Create a temporary directory to store the files
        temp_dir <- tempdir()
        
        # Copy the report files to the temporary directory
        file.copy(files_to_send()$file_name, temp_dir, overwrite = TRUE)
        
        # Create a zip file containing all the report files
        zip::zip(
          zipfile = file, files = file.path(temp_dir, basename(files_to_send()$file_name)),
          mode = "cherry-pick"
        )
        
      }
    )
    
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
        title = "Generate Reports",
        size = 'l',
        div(
          id = "progress_content",
          p("Starting report generation...")
        ),
        shinyjs::hidden(
          downloadButton(ns("download_all_reports"), "Download All Reports as ZIP", class = "btn-generate")
        ),
        shinyjs::hidden(
          div(id = ns('block_send_emails'),
              hr(),
              h4("Send Report via Email"),
              checkboxGroupInput(ns("cities_to_send"), 
                                 "Select cities to send emails to:",
                                 choices = character(0),
                                 selected = character(0)),
              
              actionButton(ns("send_emails"), "Send Emails to the selected cities", class = "btn-generate")
          )),
        div(
          id = "email_progress_content"
          #p("Starting report generation..."),
          #div(id = "email_progress_messages")
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
      
      # intialise the files to send emails
      files_to_send(NULL)
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

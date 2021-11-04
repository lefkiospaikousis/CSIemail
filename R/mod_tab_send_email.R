#' tab_send_email UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_send_email_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      col_10(
        box(title = "", width = NULL, 
            # actionButton("browser", "browser"),
            tags$script("$('#browser').show();"),
            actionButton(ns("send_emails"), "Send Emails", width = "100%", class = "btn-info"),
            hr(width = "80%"),
            htmlOutput(ns("csi_type_UI")),
            h4("List of stores with CSI - Select the stores to send email to"),
            reactable::reactableOutput(ns("stores"))
        )
      )
    )
  )
}

#' tab_send_email Server Functions
#'
#' @param from String length 1. The senders email address
#' @param creds_key String length 1. The ID value of the key (in the system key-value store)
#' @noRd 
mod_tab_send_email_server <- function(id, conn, trigger, csi_type, csi, csi_date, from, creds_key){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    WaiterSendEmails<- div(
      style="color:black;", waiter::spin_2(), h3("Sending Emails")
    ) 
    
    rv <- rv(
      send_ok = FALSE,
      success_emails = NULL,
      send_report = NULL
    )
    
    tbl_emails <- reactive({
      
      trigger()
      
      conn %>% 
        tbl("emails") %>% 
        collect() %>% 
        tidyr::nest(email = email) %>% 
        mutate(email = purrr::map(email, ~pull(., email)))
      
    })
    
    
    output$csi_type_UI <- renderText({
      
      
      paste0("<b>CSI date: </b>", csi_date(), "<br>", 
             "<b>CSI type: </b>", csi_type(), "<br>", 
             "<b>Detected: </b>", length(unique(csi_by_store()$store_code)), " stores with CSI"
             )
    })
    
    
    csi_by_store <- reactive({
      
      req(csi(), csi_date())
      
      folder_path <- tempdir() 
      
      csi_date <- switch (csi_type(),
                          
                          "ACS" = format(lubridate::dmy(isolate(csi_date())), '%d-%m-%Y'),
                          "Ticket Hour" = gsub("/", "_", isolate(csi_date())),
                          stop("Wrong csi type")
      )
      
      csi_nest <- 
        csi() %>%
        tidyr::nest(data = -store_code)
      
      csi_nest_total <- switch (csi_type(),
                                
                                # ACS
                                "ACS" = csi_nest %>% 
                                  mutate(data =  purrr::map(data, ~ add_totals(., -AWB)))
                                ,
                                # Ticket hour CSI
                                "Ticket Hour" = csi_nest %>% 
                                  mutate(data =  purrr::map(data, ~ add_totals(., all_of(vars_sum_ticketHour))))
                                ,
                                stop("Unknown CSI type", .call = FALSE)
      )
      
      
      # add the emails and filename
      csi_nest_total %>% 
        left_join(tbl_emails(), by = c("store_code")) %>% 
        mutate(
          filename = glue::glue("{folder_path}/{store_code}_{csi_date}.xlsx")
        )
      
    })
    
    
    observeEvent(csi_by_store(), {
      
      # Initialise the send report
      rv$send_report <-  csi_by_store() %>% 
        select(-data, -filename) %>% 
        mutate(send_success = FALSE)
      
    })
    
    
    selected_stores <- reactive({
      
      reactable::getReactableState("stores", "selected")
      
    })
    
    csi_by_store_filtered <- reactive({
      
      # filters the nested tibble of selected stores
      csi_by_store()[selected_stores(), ] 
    })
    
    output$stores <- reactable::renderReactable({
      
      validate(need(csi_by_store(), "Haven't loaded a CSI file yet!"))
      
      # I use a reactive value because the check-marks (send_success) on the 
      # table are not updated. Don' t know why
      
      rv$send_report %>% 
        select(-uid) %>% 
        reactable::reactable(
          striped = TRUE,
          defaultPageSize = 20,
          #filterable = TRUE,
          searchable = TRUE,
          highlight = TRUE,
          onClick = "select",
          selection = "multiple",
          columns = list(
            
            send_success = reactable::colDef(name = "Email is sent?", 
                                             cell = function(value) {
                                               # Render as an X mark or check mark
                                               if (isFALSE(value)) "\u274c No" else "\u2714\ufe0f Yes"
                                             }),
            email        = reactable::colDef(name = "Email", 
                                             cell = function(value){
                                               
                                               if(is.null(value)) "" else value
                                             }),
            store_name   = reactable::colDef(name = "Store name"),
            
            store_code   = reactable::colDef(name = "Store code")
          )
        )
    })
    
    
    
    observeEvent(rv$success_emails, {
      
      # I use a reactive value because the check-marks (send_success) on the 
      # table are not updated. Don' t know why
      
      if(isTRUE(rv$send_ok)) {
        
        new_data <- 
          csi_by_store() %>% 
          select(-data, -filename) %>% 
          mutate(
            send_success = if_else(email %in% rv$success_emails, TRUE, FALSE)
          )
        
        rv$send_report <- new_data
      }
      
    })
    
    
    observeEvent(input$send_emails, {
      
      req(csi_by_store())
      
      selected <- isolate(selected_stores())
      
      if(is.null(selected)) {
        
        showModal(modalDialog(
          title = "No stores are selected!",
          p("Please select the stores to send emails to"),
          easyClose = TRUE
        ))
        
        return()
      }
      
      
      
      dta <- csi_by_store_filtered() %>% 
        filter(!purrr::map_lgl(email, is.null))
      
      
      if(nrow(dta) == 0) {
        
        showToast(
          "error", "The selected stores do not have email addresses",
          keepVisible = TRUE, .options = list(positionClass = "toast-top-center" )
        )
        
        return()
      }
      
      #null_emails <- nrow(dta) - nrow(csi_by_store_filtered())
      
      showModal(verify_send(session, csi_by_store_filtered()))
      
    })
    
    
    
    observeEvent(input$confirm_send, {
      
      removeModal()
      
      waiter::waiter_show(color = "#EBE2E231", html = WaiterSendEmails)
      
      dta <- csi_by_store_filtered() %>% 
        filter(!purrr::map_lgl(email, is.null))
      
      
      tryCatch(
        
        expr = {
          
          # 1. Save to disk first ------------------------------------#
          save_csi_to_disk(dta)
          
          # 2. Send emails -------------------------------------------#
          # Email msg
          date_time <- blastula::add_readable_time()
          
          email_msg <-
            blastula::compose_email(
              body = blastula::md(glue::glue(
                "Hello,

            The {csi_type()} CSI is attached:

            ")),
              footer = blastula::md(glue::glue("Email sent on {date_time}."))
            )
          
          # Add attachment
          dta <- 
            dta %>% 
            mutate(
              email_msg = purrr::map(filename, ~ blastula::add_attachment(email_msg, .x))
            )
          
          # Send! Note that everything is saved in a nested tibble
          dta <- 
            dta %>% 
            mutate(
              
              send_result =  purrr::map2(
                
                .x = email_msg, 
                .y = email,
                .f = purrr::safely(
                  
                  function(msg, address){
                    
                    blastula::smtp_send(
                      email   = msg,
                      to      = address, 
                      from    = from,
                      subject = paste0(csi_type(), " CSI - Date: ", csi_date()),
                      credentials = blastula::creds_key(creds_key) # blastula::creds_file("gmail_creds")
                    )
                  }
                )
              )
            )
          
          # Send success?
          dta <- 
            dta %>% 
            mutate(
              send_success = purrr::map_lgl(
                purrr::map(send_result, "error"), is.null)
            )
          
          
          # Return Success status
          #
          
          success_emails <- 
            dta %>% 
            rowwise() %>% 
            filter(isTRUE(send_success)) %>% 
            ungroup() %>% 
            pull(email)
          
          rv$send_ok <- TRUE
          
          waiter::waiter_hide()
          
          if(any(dta$send_success)) {
            
            n_stores <- length(unique(
              filter(dta, email %in% success_emails) %>% .$store_code
            ))
            
            showModal(
              modalDialog(
                title = "Success sending emails(s)",
                p(glue::glue("
                             {length(success_emails)} email(s) have been send to 
                             {n_stores} store(s)
                             ")
                ),
                easyClose = TRUE
              )
            )
            
            rv$send_ok <- TRUE
            
            rv$success_emails <- union(isolate(rv$success_emails), success_emails)
            
          } else {
            
            msg <- "NO emails were send! Please check the email addresses"
            message_email_failure(msg)
            print(msg) # log the issue
            rv$send_ok <- FALSE
            waiter::waiter_hide()
          }
          
        },
        
        error = function(e) {
          
          msg <- "There was a problem and no emails were send"
          message_email_failure(msg)
          print(msg) # log the issue
          waiter::waiter_hide()
          
          rv$send_ok <- FALSE
        }
        
        
      )
      
      
      # 3. Remove files -----------------------------------------#
      tryCatch(
        
        expr = {
          
          lapply(dta$filename, unlink)
        },
        
        error = function(e){
          
          msg <- "Unable to delete the temporary CSI files on disk."
          showToast(
            type = "warning", title = msg,
            message = "Emails thought have been sent! Don't worry about those. Report this to Lefkios",
            keepVisible = TRUE,
            .options = list(
              closeButton = TRUE,
              positionClass = "toast-top-left"
            )
          )
          
          print(msg) # log the issue
          
        }
      )
      
      waiter::waiter_hide()
      
    })
    
    
    
  })
}

## To be copied in the UI
# mod_tab_send_email_ui("tab_send_email_ui_1")

## To be copied in the server
# mod_tab_send_email_server("tab_send_email_ui_1")

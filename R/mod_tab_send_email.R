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
      col_8(
        box(title = "", width = NULL,
            # actionButton("browser", "browser"),
            tags$script("$('#browser').show();"),
            actionButton(ns("send_emails"), "Send Emails", width = "100%", class = "btn-info"),
            hr(width = "80%"),
            h4("List of stores with CSI"),
            reactable::reactableOutput(ns("stores"))
        )
      )
    )
  )
}

#' tab_send_email Server Functions
#'
#' @noRd 
mod_tab_send_email_server <- function(id, conn, trigger, csi, csi_date){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    WaiterSendEmails<- div(
      style="color:black;", waiter::spin_2(), h3("Sending Emails")
    ) 
    
    rv <- rv(
      send_ok = FALSE,
      success_stores = NULL,
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
    
    csi_by_store <- reactive({
      
      req(csi(), csi_date())
      
      folder_path <- tempdir() 
      csi_date <- isolate(csi_date())
      
      csi() %>%
        tidyr::nest(data = -store_code) %>% 
        mutate(data =  purrr::map(data,
                                  ~janitor::adorn_totals(., where = "row", fill = "", na.rm = TRUE, name = "Total", -AWB)
        ) ) %>% 
        # add the emails
        left_join(tbl_emails(), by = c("store_code")) %>% 
        # add the filename
        mutate(
          filename = glue::glue("{folder_path}/{store_code}_{format(lubridate::dmy(csi_date), '%d-%m-%Y')}.xlsx")
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
            send_success =reactable::colDef(cell = function(value) {
              # Render as an X mark or check mark
              if (isFALSE(value)) "\u274c No" else "\u2714\ufe0f Yes"
            })
          )
        )
    })
    
    
    
    observeEvent(rv$success_stores, {
      
      # I use a reactive value because the check-marks (send_success) on the 
      # table are not updated. Don' t know why
      
      if(isTRUE(rv$send_ok)) {
        
        new_data <- 
          csi_by_store() %>% 
          select(-data, -filename) %>% 
          mutate(
            send_success = if_else(store_code %in% rv$success_stores, TRUE, FALSE)
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
      
      waiter::waiter_show(color = "#EBE2E231", html = WaiterSendEmails)

      dta <- 
        csi_by_store()[selected, ] %>% 
        filter(!purrr::map_lgl(email, is.null))
      
      if(nrow(dta) == 0) {
        
        waiter::waiter_hide()
        
        showToast(
          "error", "The selected stores do not have email addresses",
          keepVisible = TRUE, .options = list(positionClass = "toast-top-center" )
        )
        
        return()
      }
      
      
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

            The CSI is attached:

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
                      from    = "lefkiospaik@gmail.com",
                      subject = paste0("CSI date: ", csi_date()),
                      credentials = blastula::creds_key("gmail") # blastula::creds_file("gmail_creds")
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
          rv$success_stores <- 
            dta %>% 
            rowwise() %>% 
            filter(isTRUE(send_success)) %>% 
            ungroup() %>% 
            pull(store_code)
          
          rv$send_ok <- TRUE
          
          waiter::waiter_hide()
          
          if(any(dta$send_success)) {
            
            showModal(
              modalDialog(
                title = "Success sending emails(s)",
                p(glue::glue("Emails have been send to {length(rv$success_stores)} store(s)")),
                easyClose = TRUE
              )
            )
            
            rv$send_ok <- TRUE
            
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
      
    })
    
    
    
  })
}

## To be copied in the UI
# mod_tab_send_email_ui("tab_send_email_ui_1")

## To be copied in the server
# mod_tab_send_email_server("tab_send_email_ui_1")


# modal_resend <- function(session) {
#   ns <- session$ns
#   
#   modalDialog(
#     title = "Notice!",
#     p(strong("Emails have already been sent!")),
#     p("Do you want to send the emails again?"),
#     tagList(
#       actionButton(ns("btn_send_again"), "Yes, go ahead", class = "btn-success"),
#       modalButton("Changed my mind. STOP!")
#     ),
#     footer = NULL
#     
#   )
# }
# 
# observeEvent(input$btn_send_again, {
#   
#   rv$resend_ok <- TRUE
#   removeModal()
# })

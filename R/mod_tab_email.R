#' tab_email UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_email_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      col_8(
        box(title = "", width = NULL,
            # actionButton("browser", "browser"),
            tags$script("$('#browser').show();"),
            actionButton(ns("send_emails"), "Send Emails", width = "100%", class = "btn-info"),
            hr(width = "80%"),
            #DT::DTOutput(ns("stores"))
            h4("List of stores with CSI"),
            reactable::reactableOutput(ns("stores"))
        )
      )
    )
  )
}

#' tab_email Server Functions
#'
#' @noRd 
mod_tab_email_server <- function(id, csi, csi_date){
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
    
    
    observeEvent(csi(), {
      
      # Initialise the send report
      
      rv$send_report <-  csi() %>% 
        select(-data, -filename) %>% 
        mutate(send_success = FALSE)
      
    })
    
    
    
    selected_stores <- reactive({
      
      reactable::getReactableState("stores", "selected")
      
    })
    
    
    output$stores <- reactable::renderReactable({
      
      validate(need(csi(), "Haven't loaded a CSI file yet!"))
      
      # I use a reactive value because the check-marks (send_success) on the table are not
      # updated. Don;t know why
      
      rv$send_report %>% 
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
      
      # I dont use e updateTable because the check-marks (send_success) on the table are not
      # updated. Don;t know why
      if(isTRUE(rv$send_ok)) {
        
        new_data <- 
          csi() %>% 
          select(-data, -filename) %>% 
          mutate(
            send_success = if_else(store_code %in% rv$success_stores, TRUE, FALSE)
          )
        
        rv$send_report <- new_data
      }
      
    })
    
    observeEvent(input$send_emails, {
      
      req(csi())
      
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
      
      tryCatch(
        
        expr = {
          
          dta <- 
            csi()[selected, ] %>% 
            filter(!purrr::map_lgl(email, is.null))
          
          
          if(nrow(dta) == 0) {
            
            waiter::waiter_hide()
            
            shinyFeedback::showToast(
              "error", "The selected stores do not have email addresses",
              keepVisible = TRUE, .options = list(positionClass = "toast-top-center" )
            )
            
            return()
          }
          
          # 1. Save to disk first -----------------#
          save_csi_to_disk(dta)
          
          
          # 2. Send emails -----------------#
          
          # Email mesg
          date_time <- blastula::add_readable_time()
          
          email_msg <-
            blastula::compose_email(
              body = blastula::md(glue::glue(
                "Hello,

            The CSI is attached:

            ")),
              footer = blastula::md(glue::glue("Email sent on {date_time}."))
            )
          
          # add attachment
          dta <- 
            dta %>% 
            mutate(
              email_msg = purrr::map(filename, ~ blastula::add_attachment(email_msg, .x))
            )
          
          # Send
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
                      credentials = blastula::creds_file("gmail_creds")
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
          
          # 
          rv$send_ok <- TRUE
          
          waiter::waiter_hide()
          
          if(any(dta$send_success)) {
            shinyFeedback::showToast("success", 
                                     glue::glue("Emails have been send to {length(rv$success_stores)} stores"),
                                     .options = list(
                                       positionClass = "toast-top-center"
                                     )
                                     
            )
          } else {
            
            message_email_failure("There was a problem and no emails were send")
            rv$send_ok <- FALSE
            waiter::waiter_hide()
          }
          
        },
        
        error = function(e) {
          
          message_email_failure("There was a problem and no emails were send")
          
          waiter::waiter_hide()
          
          rv$send_ok <- FALSE
        }
        
        
      )
      
      
      
      
      # 3. Remove files --------#
      tryCatch(
        
        expr = {
          
          lapply(dta$filename, unlink)
        },
        
        error = function(e){
          
          shinyFeedback::showToast(
            type = "warning",
            message = "Unable to delete the temporary CSI files on disk. 
                                   Emails thought have been sent!",
            keepVisible = TRUE,
            .options = list(
              closeButton = TRUE,
              positionClass = "toast-top-center"
            )
          )
          
        }
      )
      
    })
    
    
    
  })
}

## To be copied in the UI
# mod_tab_email_ui("tab_email_ui_1")

## To be copied in the server
# mod_tab_email_server("tab_email_ui_1")


# modal_resent <- function(session) {
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

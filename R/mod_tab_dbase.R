#' tab_dbase UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_dbase_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      col_8(
        box(width = NULL,
            div(id = "tbl_buttons",
                actionButton(ns("btn_add"), "Add", icon("plus")),
                actionButton(ns("btn_edit"), "Edit", icon("edit")),
                #actionButton(ns("btn_copy"), "Copy", icon("copy")),
                actionButton(ns("btn_delete"), "Delete", icon("trash-alt")),
                style = "margin-bottom:8px" 
            ),
            br(),
            DTOutput(ns("tbl_emails"))
        )
      )
    )
    
  )
}

#' tab_dbase Server Functions
#'
#' @param conn The connection to the RSQLite
#' @noRd 
mod_tab_dbase_server <- function(id, conn){
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    
    tbl_emails_proxy <- DT::dataTableProxy("tbl_emails")
    
    rv <- rv(
      db_trigger = 0,
      #form_mode = NULL,  # edit or new
      store_to_edit = NULL
    )
    
    email_regex <- "^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}$"
    email_regex <- "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$"
    
    tbl_emails <- reactive({
      
      rv$db_trigger
      
      conn %>% 
        tbl("emails") %>% 
        collect()
    })
    
    
    
    output$tbl_emails <- renderDT({
      
      tbl_emails() %>%
        #select(-uid) %>% 
        datatable(
          options = list()
          , rownames = FALSE
          , selection = "single"
          , caption = "The ACS stores and associated email(s)"
          , filter = "top"
        )
    })
    
    
    ids_selected <- reactive(input$tbl_emails_rows_selected)
    
    
    observeEvent(input$btn_add, {
      
      #rv$form_mode <- "add"
      showModal(entry_form(session))
      
    })
    
    observeEvent(input$btn_edit, {
      
      #rv$form_mode <- "edit"
      
      if(is.null(ids_selected())) {
        
        showToast("warning", "Please select a store on the table first",
                                 .options = list(positionClass = "toast-top-center")
        )
        
      } else {
        
        # Edit the store
        store <- as.list(tbl_emails()[ids_selected(), ])
        
        showModal(
          entry_form(session, edit = TRUE, store = store)
        )
        
        updateTextInput(session, "store_code", value = store$store_code)
        updateTextInput(session, "store_name", value = store$store_name)
        updateTextInput(session, "email", value = store$email)
        
        #rv$edit_mode = TRUE
        rv$store_to_edit = store
      }
      
      
    })
    
    observeEvent(input$submit, {
      
        tryCatch(
          
          expr = {
            
            if(is.null(rv$store_to_edit)) {
              
              # new store/ email
            append_data(conn, "emails", form_data())
            
              showToast("success",
                        paste0("Store: ",  form_data()$store_code, " was added in the database"),
                        .options = list(positionClass = "toast-top-center")
              )

            } else {
              
              # edit store
              DBI::dbExecute(
                conn,
                "UPDATE emails SET store_name=$store_name, store_code=$store_code, email=$email WHERE uid=$uid",
                params = as.list(form_data())
                
              )
              
              showToast("success",
                        paste0("Store: ",  form_data()$store_code, " was edited in the database"),
                        .options = list(positionClass = "toast-top-center")
              )
              
              rv$store_to_edit = NULL
              
            }
            
            removeModal()
            
            rv$db_trigger <- isolate(rv$db_trigger) + 1
            
          },
          
          error = function(e) {
            
            showModal(modalDialog(title = "Unable to add/ edit store to the database"),
                      p("Cannot add/ edit this to the database. Something is wrong"),
                      p("Maybe the connection to the database is lost. Refresh the webage and try again")
            )
          } 
        )
        
      
      #rv$form_mode <- NULL
    })
    
    observeEvent(input$email, {
      
      if(!stringr::str_detect(input$email, email_regex)) {
        shinyFeedback::showFeedbackDanger(input$email, text = "Invalid email address")
      }
      
      
    })
    
    form_data <- reactive({
      
      store <- isolate(rv$store_to_edit)
      
      data.frame(
        uid = if(!is.null(store)) {store$uid} else {uuid::UUIDgenerate()},
        store_code = input$store_code,
        store_name = input$store_name,
        email      = input$email, 
        stringsAsFactors = FALSE)
      
    })
    
    
    
    observeEvent(input$btn_delete, {
      
      #ids <- input$tbl_emails_rows_selected
      
      if(is.null(ids_selected())) {
        
        showToast("warning", "Please select a store on the table first",
                                 .options = list(positionClass = "toast-top-center")
        )
        
      } else {
        
        store <- tbl_emails()[ids_selected(), ]
        
        showModal(verify_delete(session, store))
        
      }
      
    })
    
    observeEvent(input$submit_delete, {
      
      removeModal()
      
      #ids <- input$tbl_emails_rows_selected
      uids <- tbl_emails()[ids_selected(), ]$uid
      store_name <- tbl_emails()[ids_selected(), ]$store_name
      
      tryCatch(
        
        expr = {
          delete_data(conn, "emails", "uid", uids)
          
          rv$db_trigger <- isolate(rv$db_trigger) + 1
          
          showToast("success",
                    paste0("Store: ",  store_name, " was deleted from the Database"),
                    .options = list(positionClass = "toast-top-center")
          )
        },
        
        error = function(e) {
          
          msg <- "Error Deleting Store"
          print(msg)
          print(error)
          showToast("error", msg)
          
        }
      )
      
      
    })
    
    
    
    
    
    
    
    
    
    return(rv)
    
  })
  
}

## To be copied in the UI
# mod_tab_dbase_ui("tab_dbase_ui_1")

## To be copied in the server
# mod_tab_dbase_server("tab_dbase_ui_1")

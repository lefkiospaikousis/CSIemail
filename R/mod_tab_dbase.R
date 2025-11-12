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
      box(title = 'Emails', width = 8,
          tabsetPanel(
            tabPanel('Store Emails',
                     buttons_edit(ns),
                     DTOutput(ns("tbl_emails"))
            ),
            tabPanel('Cashier Groups Emails',
                     mod_cachier_groups_emails_ui(ns("cachier_groups_emails_1"))
            ),
            tabPanel('Cashier Groups',
                     mod_cashier_groups_ui(ns("cachier_groups_1"))
            )
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
    
    res_cashier_groups_emails <- mod_cachier_groups_emails_server("cachier_groups_emails_1", conn)
    res_cashier_groups <- mod_cashier_groups_server("cachier_groups_1", conn)
    
    tbl_emails_proxy <- DT::dataTableProxy("tbl_emails")
    
    rv <- rv(
      db_trigger = 0,
      store_to_edit = NULL
    )
    
    email_regex <- "^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}$"
    email_regex <- "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$"
    
    tbl_emails <- reactive({
      
      rv$db_trigger
      
      conn %>% 
        tbl(db_tables[['store_emails']]) %>% 
        collect()
    })
    
    
    output$tbl_emails <- renderDT({
      
      tbl_emails() %>%
        select(-uid) %>% 
        datatable(
          options = list()
          , rownames = FALSE
          , colnames = c("Store Code" = "store_code", "Store Name" = "store_name", "Email" = "email")
          , selection = "single"
          , caption = "The ACS stores and associated email(s)"
          , filter = "top"
        )
    })
    
    
    ids_selected <- reactive(input$tbl_emails_rows_selected)
    
    
    observeEvent(input$btn_add, {
      
      showModal(entry_form(session))
      
    })
    
    observeEvent(input$btn_edit, {
      
      if(is.null(ids_selected())) {
        
        showToast("warning", "Please select a row on the table",
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
                      glue("Store: {form_data()$store_name} - {form_data()$store_code} was edited in the database"),
                      .options = list(positionClass = "toast-top-center")
            )
            
            rv$store_to_edit = NULL
            
          }
          
          removeModal()
          
          rv$db_trigger <- isolate(rv$db_trigger) + 1
          
        },
        
        error = function(e) {
          
          print(e)
          showModal( showModal( modal_error_editing_dbase('store') ) )
          
          return(NULL)
            
        } 
      )
      
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
      
      if(is.null(ids_selected())) {
        
        showToast("warning", "Please select a row on the table",
                  .options = list(positionClass = "toast-top-center")
        )
        
      } else {
        
        store <- tbl_emails()[ids_selected(), ]
        
        showModal(verify_delete(session, store))
        
      }
      
    })
    
    observeEvent(input$submit_delete, {
      
      removeModal()
      
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
          print(e)
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

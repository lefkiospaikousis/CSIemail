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
 
    rv <- rv(
      db_trigger = 0
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
        datatable(
          
          options = list()
          , rownames = FALSE
          , selection = "single"
          , caption = "The ACS stores and associated email(s)"
          , filter = "top"
        )
    })
    
    
    observeEvent(input$btn_add, {
      
      showModal(entry_form(session))
      
    })
    
    observeEvent(input$submit, {
      
      tryCatch(
        
        expr = {
          
          append_data(conn, "emails", form_data())
          
          removeModal()
          
          rv$db_trigger <- isolate(rv$db_trigger) + 1
        },
       
        error = function(e) {
          
          showModal(modalDialog(title = "Unable to add to the database"),
                    p("Cannot add this to the database. Something is wrong"),
                    p("Maybe the connection to the database is lost. Refresh and try again")
                    )
        } 
      )
        
      
    })
    
    observeEvent(input$email, {
      
        if(!stringr::str_detect(input$email, email_regex)) {
          shinyFeedback::showFeedbackDanger(input$email, text = "Invalid email address")
        }
      
        
    })
    
    form_data <- reactive({
      
      # req or dependent on SUBMIT BUTTON 
      
      data.frame(#row_id = UUIDgenerate(),
        store_code = input$store_code,
        store_name = input$store_name,
        email      = input$email, 
        #date = as.character(format(Sys.Date(), format="%d-%m-%Y")),
        stringsAsFactors = FALSE)
      
    })
    
    return(rv)
    
  })
  
  
  
  
}
    
## To be copied in the UI
# mod_tab_dbase_ui("tab_dbase_ui_1")
    
## To be copied in the server
# mod_tab_dbase_server("tab_dbase_ui_1")

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
      col_6(
        box(width = NULL,
            div(id = "tbl_buttons",
                actionButton(ns("btn_add"), "Add", icon("plus")),
                actionButton(ns("btn_edit"), "Edit", icon("edit")),
                actionButton(ns("btn_copy"), "Copy", icon("copy")),
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
    
#' tab_email Server Functions
#'
#' @param conn The connection to the RSQLite
#' @noRd 
mod_tab_email_server <- function(id, conn){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    tbl_emails <- reactive({
      
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
    
  })
  
  
  
  form_data <- reactive({
    
   # req or dependent on SUBMIT BUTTON 
    
    data.frame(#row_id = UUIDgenerate(),
      store_code = input$store_code,
      store_name = input$store_name,
      email = input$email, 
      date = as.character(format(Sys.Date(), format="%d-%m-%Y")),
      stringsAsFactors = FALSE)
    
  })
  
  
  
}
    
## To be copied in the UI
# mod_tab_email_ui("tab_email_ui_1")
    
## To be copied in the server
# mod_tab_email_server("tab_email_ui_1")

#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny dplyr shinydashboard
#' @importFrom DT datatable DTOutput renderDT
#' @importFrom purrr map walk walk2 map2 pmap
#' @importFrom shinyjs enable disable toggleState
#' @importFrom glue glue
#' @importFrom reactable colDef colFormat reactable
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  # RSQLite connection ------------------------------------------------------
  
  configuration <- Sys.getenv("GOLEM_CONFIG_ACTIVE", "default")
  path <- get_golem_config("db_path", configuration)
  
  email_db <- DBI::dbConnect(RSQLite::SQLite(), path)
  
  shiny::onStop(function(){
    
    cat("Doing application cleanup\n")
    cat("-----\n Removing connections\n")
    
    DBI::dbDisconnect(email_db)
    
  })
  
  
  
  mod_tab_load_server("tab_load_ui_1")
  
  mod_tab_email_server("tab_email_ui_1", conn = email_db)
  
  
  
}

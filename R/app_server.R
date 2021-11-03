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
#' @importFrom shinyFeedback showToast
#' @importFrom stats na.omit setNames
#' @noRd
app_server <- function( input, output, session ) {
  
  rv <- rv(
    
    csi = NULL,
    stores = NULL,
    db_trigger = 0,
    csi_type = NULL,
    csi_date = NULL
  )
  
  # RSQLite connection ------------------------------------------------------
  
  configuration <- Sys.getenv("GOLEM_CONFIG_ACTIVE", "default")
  path <- get_golem_config("db_path", configuration)
  
  email_db <- DBI::dbConnect(RSQLite::SQLite(), path)
  
  shiny::onStop(function(){
    
    cat("Doing application cleanup\n")
    cat("-----\n Removing connections\n")
    
    DBI::dbDisconnect(email_db)
    
  })
  
  # 1. Load  csi
  load <- mod_tab_load_server("tab_load_ui_1")
  
  observeEvent(load$stores, {
    
    rv$stores   <- load$stores
    rv$csi      <- load$csi
    rv$csi_date <- load$csi_date
    rv$csi_type <- load$csi_type
    
  }, ignoreNULL = TRUE)
  
  
  
  # 2. Send emails
  
  mod_tab_send_email_server("tab_email_ui_1",
                            conn = email_db,
                            trigger = reactive(rv$db_trigger),
                            reactive(rv$csi_type),
                            reactive(rv$csi),
                            reactive(rv$csi_date)
                            )
  
  
  
  # ----- #
  # Database
  dbase <- mod_tab_dbase_server("tab_dbase_ui_1", conn = email_db)
  
  observeEvent(dbase$db_trigger, {
    
    rv$db_trigger   <- isolate(rv$db_trigger) + 1
    
  }, ignoreNULL = TRUE)
  
  output$outs <- renderPrint({
    
    reactiveValuesToList(rv)
  })
  
}

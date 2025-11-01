#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny dplyr shinydashboard
#' @importFrom DT datatable DTOutput renderDT
#' @importFrom purrr map map2 pmap walk walk2 pwalk map_lgl
#' @importFrom shinyjs enable disable toggleState
#' @importFrom glue glue
#' @importFrom reactable colDef colFormat reactable
#' @importFrom shinyFeedback showToast
#' @importFrom stats na.omit setNames
#' @importFrom openxlsx writeData writeDataTable
#' @noRd
app_server <- function( input, output, session ) {
  
  shiny::onStop(function(){
    
    cat("Doing application cleanup\n")
    cat("-----\n Removing connections\n")
    
    DBI::dbDisconnect(dbase_csi)
    
  })
  
  
  observe({
    
    moneygram_stores <- readxl::read_excel(get_golem_config('moneygram_stores'))
    
    moneygram_stores <- moneygram_stores |> 
      mutate(`Agent ID` = as.character(`Agent ID`)) |>
      select(agent_id = 'Agent ID', store = `Acs store code`)
    
    session$userData$moneygram_stores <- moneygram_stores
    
    
  })
  
  
  rv <- rv(
    
    csi = NULL,
    stores = NULL,
    db_trigger = 0,
    csi_type = NULL,
    csi_date = NULL
  )
  
  
  configuration <- Sys.getenv("GOLEM_CONFIG_ACTIVE", "default")
  
  res_auth <- shinymanager::secure_server(
    
    check_credentials = shinymanager::check_credentials(
      
      db = CSIemail:::get_golem_config("db_users", configuration),
      passphrase = get_golem_config("users_passphrase", configuration)
      
    ),
    keep_token = TRUE
  )
  
  
  user_info <- reactive({
    
    reactiveValuesToList(res_auth)
    
  })
  
  observe({
    session$userData$user <- user_info()$user
  })
  
  output$userName <- renderText({
    
    #user_info()$user
    paste0("User: ", session$userData$user)
    
  })
  
  # RSQLite connection ------------------------------------------------------
  
  # Database for emails
  path <- get_golem_config("db_path", configuration)
  
  #creds_key <- get_golem_config("smtp_creds", configuration)
  
  #from <- blastula::creds_key(creds_key)$user
  
  dbase_csi <- DBI::dbConnect(RSQLite::SQLite(), path)
  
  
  # User IP Information -----------------------------------------------------
  
  IP_info <- reactive({ input$getIP })
  
  output$ip_user <- renderPrint({
    
    #req(IP_info())
    ip_user()
    #IP_info()$ip
  })
  
  ip_user <- reactive({
    #cat("The remote IP is", isolate(input$remote_addr), "\n")
    input$remote_addr
  })
  
  # TABS --------------------------------------------------------------------
  
  
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
                            conn = dbase_csi,
                            trigger = reactive(rv$db_trigger),
                            reactive(rv$csi_type),
                            reactive(rv$csi),
                            reactive(rv$csi_date)
  )
  
  
  
  # 3. Email dbase
  
  res_tab_dbase <- mod_tab_dbase_server("tab_dbase_ui_1", conn = dbase_csi)
  
  observeEvent(res_tab_dbase$db_trigger, {
    
    rv$db_trigger   <- isolate(rv$db_trigger) + 1
    
  }, ignoreNULL = TRUE)
  
  output$outs <- renderPrint({
    
    reactiveValuesToList(rv)
  })
  
  
  # 4. Cashier Monitoring
  
  mod_cashier_monitoring_server("cashier_monitoring_1", dbase_csi)
  
  
}

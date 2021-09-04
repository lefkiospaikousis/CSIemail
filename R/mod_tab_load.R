#' tab_load UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_load_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Load the CSI dataset"),
    tags$hr(style="border-color: black;"),
    fluidRow(
      box(title = p("Load a CSI Statement", style="color:#B88C4A"),
          fileInput(ns("file_csi"), "Load an .xlsx/.xls file", buttonLabel = "Load file",
                    accept = c(".xlsx", ".xls")),
          DT::DTOutput(ns("csi")),
          actionButton(ns("btn_save_csi"), "Save to DB")
      )
    )
    
  )
}
    
#' tab_load Server Functions
#'
#' @noRd 
mod_tab_load_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    dta <- rv(
      csi = NULL
    )
    
    # csi ---
    csi <- reactive({
      
      req(input$file_csi)
      
      file = input$file_csi
      
      excel <- readxl::excel_format(file$name)
      
      if(is.na(excel)){
        shinyFeedback::showFeedbackDanger("file_csi", "This is not an excel file")
        return()
      }
      
      #temp <- load_csi(file$datapath)
      
      temp <- readxl::read_excel(file$datapath) %>% 
        rename(
          date = ...1,
          store_code = ...2
        )
      
      if(is.null(temp)){
        
        error_in_csi()
        shinyFeedback::hideFeedback("file_csi")
        shinyFeedback::showFeedbackDanger("file_csi", "This is not a valid csi file")
        
        return(NULL)
      }
      
      out <- process_csi(temp)
      
      shinyFeedback::hideFeedback("file_csi")
      shinyFeedback::showFeedbackSuccess("file_csi", "All good")
      
      out
    })
    
    observeEvent(csi(), {
      
      dta$csi <- csi()
      
    })
    
    
    output$csi <- DT::renderDT({
      
      csi() %>% 
        datatable(
          options = list(
            # paging = FALSE, 
            scrollX = TRUE, scrollY = "220px",
            rownames = FALSE
          )
        )
    })
    
    # # Save in database --------------------------------------------------------
    # 
    # # BANK 
    # observeEvent(input$btn_save_bank, {
    #   
    #   bank <- isolate(unique(bank()$bank))
    #   
    #   if(!isTruthy(bank())){
    #     showModal(modalDialog(
    #       title  = "No data to save",
    #       p("Probably you haven't uploaded any BANK transactions file"),
    #       easyClose = TRUE
    #     ))
    #   } else {
    #     
    #     #check duplicate entries
    #     new_entries = switch (bank,
    #                           "HB" = check_bank_duplicates(conn,"hellenic", bank()),
    #                           "BOC" = check_bank_duplicates(conn,"BOC", bank())
    #     )
    #     
    #     if(nrow(new_entries) == 0){
    #       showModal(modalDialog(
    #         title  = " BANK database",
    #         p("All BANK transactions in the uploaded file are already in the database!"),
    #         p("Perhaps load a different file?")
    #       ))
    #       
    #     } else {
    #       # update
    #       showModal(modalDialog(
    #         title  = "Updating BANK database",
    #         p("Updating BANK database. This might take a few seconds"),
    #         p("Please wait"),
    #         footer = NULL
    #       ))
    #       
    #       waiter::waiter_show(color = "#EBE2E231")
    #       
    #       if(bank == "HB"){
    #         DBI::dbAppendTable(conn,"hellenic", new_entries)
    #       } else {
    #         DBI::dbAppendTable(conn,"BOC", new_entries)
    #       }
    #       
    #       removeModal()
    #       waiter::waiter_hide()
    #       
    #       n_dups <- nrow(bank()) - nrow(new_entries)
    #       
    #       showModal(modalDialog(
    #         title  = "DONE updating BANK database",
    #         p("Dataset was checked for entries already in the Database. There were: ", n_dups),
    #         p("The BANK database is updated with", nrow(new_entries), " new transactions"),
    #         easyClose = TRUE
    #       ))
    #       
    #     }
    #   }
    #   
    # })
    
    # # csi
    # observeEvent(input$btn_save_csi, {
    #   
    #   if(!isTruthy(csi())){
    #     showModal(modalDialog(
    #       title  = "No data to save",
    #       p("Probably you haven't uploaded any csi transactions file"),
    #       easyClose = TRUE
    #     ))
    #   } else {
    #     
    #     #check duplicate entries
    #     new_entries = check_csi_duplicates(conn,"csi", csi())
    #     
    #     if(nrow(new_entries) == 0){
    #       showModal(modalDialog(
    #         title  = "csi database",
    #         p("All csi transactions in the uploaded file are already in the database!"),
    #         p("Perhaps load a different file?")
    #       ))
    #       
    #     } else {
    #       # update
    #       showModal(modalDialog(
    #         title  = "Updating csi database",
    #         p("Updating csi database. This might take a few seconds"),
    #         p("Please wait"),
    #         footer = NULL
    #       ))
    #       
    #       waiter::waiter_show(color = "#EBE2E231")
    #       DBI::dbAppendTable(conn,"csi", new_entries)
    #       removeModal()
    #       waiter::waiter_hide()
    #       
    #       n_dups <- nrow(csi()) - nrow(new_entries)
    #       
    #       showModal(modalDialog(
    #         title  = "DONE updating csi database",
    #         p("Dataset was checked for entries already in the Database. There were: ", n_dups),
    #         p("The csi database is updated with", nrow(new_entries), " new transactions"),
    #         easyClose = TRUE
    #       ))
    #       
    #     }
    #   }
    #   
    # })
    
    return(dta)
    
  })
}
    
## To be copied in the UI
# mod_tab_load_ui("tab_load_ui_1")
    
## To be copied in the server
# mod_tab_load_server("tab_load_ui_1")

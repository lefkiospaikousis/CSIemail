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
                    accept = c(".xlsx", ".xls"))
          #DT::DTOutput(ns("csi")),
          #actionButton(ns("btn_save_csi"), "Save to DB")
      )
    ),
    uiOutput(ns("store_csi_UI"))
    
  )
}

#' tab_load Server Functions
#'
#' @param conn The connection to the RSQLite
#' @param trigger Numeric. The trigger if the emails in the db is updated
#' @noRd 
mod_tab_load_server <- function(id, conn, trigger){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    rv <- rv(
      csi = NULL,
      stores = NULL,
      csi_date = NULL
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
      
      temp <- safe_readXL(file$datapath)
      
      #temp <- readxl::read_excel(file$datapath) 
      
      if(is.null(temp$result)){
        
        #error_in_csi()
        shinyFeedback::hideFeedback("file_csi")
        shinyFeedback::showFeedbackDanger("file_csi", "This is not a valid csi file")
        
        return(NULL)
      }
      
      
      dta <- temp$result %>% 
        rename(
          date = ...1,
          store_code = ...2
        )
      
      rv$csi_date <- get_csi_date(dta)
      
      out <- process_csi(dta)
      
      shinyFeedback::hideFeedback("file_csi")
      shinyFeedback::showFeedbackSuccess("file_csi", "All good")
      
      out
      
    })
    
    
    stores <- reactive({
      
      req(csi())
      
      unique(csi()$store_code)
      # distinct(csi(), store_name, store_code) %>% 
      #   tibble::deframe()
      
    })
    
    
    # nested tibble
    tbl_emails <- reactive({
      
      trigger()
      
      conn %>% 
        tbl("emails") %>% 
        collect() %>% 
        tidyr::nest(email = email) %>% 
        mutate(email = purrr::map(email, ~pull(., email)))
      
    })
    
    
    csi_by_store <- reactive({
      
      req(csi(), rv$csi_date)
      
      folder_path <- tempdir() 
      csi_date <- isolate(rv$csi_date)
      
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
    
    
    output$store_csi_UI <- renderUI({
      
      req(csi_by_store())
      
      tagList(
        fluidRow(
          box(title = "View a store's csi",
              selectInput(ns("store"), "Select a store", choices = NULL),
              DT::DTOutput(ns("store_csi"))
          )
        )
      )
      
    })
    
    output$store_csi <- DT::renderDT({
      
      req(csi_by_store())
      
      csi_by_store() %>% 
        filter(store_code == input$store) %>% 
        select(data) %>% 
        tidyr::unnest(cols = c(data)) %>% 
        datatable(
          rownames = FALSE,
          options = list(
             paging = FALSE,
             ordering = FALSE
          )
        )
        
    })
    
    # Save to rv for return
    observeEvent(stores(), {
      
      rv$csi    <- csi_by_store()
      rv$stores <- stores()
      
      updateSelectInput(inputId = "store", choices = rv$stores)
    })
    
    
    output$csi <- DT::renderDT({
      
      csi() %>% 
        datatable(
          options = list(
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
    
    
    return(rv)
    
  })
}

## To be copied in the UI
# mod_tab_load_ui("tab_load_ui_1")

## To be copied in the server
# mod_tab_load_server("tab_load_ui_1")

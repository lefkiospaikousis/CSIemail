save_csi_to_disk <- function(dta, csi_type, csi_date) {
  
  stopifnot(all(c("store_code", "store_name", "data", "filename") %in% names(dta)))
  
  Sys.setlocale(locale = "greek")
  on.exit(Sys.setlocale(locale = "English_United Kingdom"))
  
  # create workbooks
  my_dta <- 
    dta %>% 
    mutate(
      wb = pmap(
        list(data, store_code, store_name),
        ~ as_excel_wb(..1, ..2, ..3, csi_type, csi_date )
      )
    )
  
  
  
  
  
  pwalk(
    select(my_dta, wb, filename),
    function(wb, filename){
      openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
    }
  )
  
  
}

as_excel_wb <- function(dta, store_code, store_name, csi_type, csi_date){
  
  
  path <-switch (csi_type,
                 "ACS CSI" =get_golem_config("acs_template"),
                 "Ticket Hour Sales" = get_golem_config("th_template"),
                 stop("Unknown CSI type", .call = FALSE)
  )
  
  total_amount <- switch (csi_type,
          "ACS CSI" = tail(dta[, ncol(dta)], 1),
          "Ticket Hour Sales" = tail(dta, 1)$os,
          stop("Unknown CSI type", .call = FALSE)
  )
  
  
  total_text <- switch (csi_type,
                        "ACS CSI" = paste0("(Total Amount : € ", total_amount, ")"),
                        "Ticket Hour Sales" = paste0("(O/S Amount : € ", total_amount, ")"),
                        stop("Unknown CSI type", .call = FALSE)
  )
  
  
  xl_out <- openxlsx::loadWorkbook(path)
  
  
  start_row <- 13
  n_row <- nrow(dta)
  total_row <- start_row + n_row + 3
  
  total_style <- openxlsx::createStyle(fontSize = 14, textDecoration = "bold" )
  
  writeDataTable(xl_out, sheet = 1,
                           dta, startRow = start_row,
                           tableStyle = "TableStyleMedium16"
  )
  
  writeData(xl_out, 1, store_name, startRow = 9, startCol = 2)
  writeData(xl_out, 1, store_code, startRow = 10, startCol = 2)
  
  writeData(xl_out, 1, format(Sys.Date(), "%d-%m-%Y"), startRow = 3, startCol = 2)
  writeData(xl_out, 1, 
            format(lubridate::with_tz(Sys.time(), tzone = "EET"), "%H:%M:%S"), 
            startRow = 4, startCol = 2)
  
  # Declare total
  writeData(xl_out, 1, total_text, startRow = total_row, startCol = 4)
  openxlsx::addStyle(xl_out, 1,total_style ,
                     rows = total_row,
                     cols = 4, gridExpand = TRUE
                     )
  
  
  if(length(csi_date) == 1){
    
    # single date for ACS csi
    csi_date <- as.character(csi_date[[1]])
    
    writeData(xl_out, 1, csi_date, startRow = 6, startCol = 2)
    writeData(xl_out, 1, csi_date, startRow = 7, startCol = 2)
  }
  
  if(length(csi_date) == 2){
    # from date and to date for TH
    csi_date <- format(csi_date, "%d-%m-%Y")
    
    writeData(xl_out, 1, csi_date[[1]], startRow = 6, startCol = 2)
    writeData(xl_out, 1, csi_date[[2]], startRow = 7, startCol = 2)
  }
  
  if(length(csi_date) > 2){
    stop("Length of csi_date is greater than 2 - min and max", call. = FALSE)
  }
  
  return(xl_out)
}

#' Modal dialog for Sending verification
#' Informs the user on how many emails to how many stores are going to be send
#' 
#' @param session The current session object
#' @param dta 
#' @noRd
verify_send <- function(session, dta) {
  
  ns <- session$ns
  
  stopifnot(nrow(dta) > 0)
  
  dta_w_emails <- filter(dta, !map_lgl(email, is.null))
  
  n_emails <- length(dta_w_emails$email)
  n_stores <- length(unique(dta_w_emails$store_code))
  
  
  modalDialog(
    div(
      style = "padding: 10px;",
      class = "text-center",
      h4(
        style = "line-height: 1.00;",
        paste0(
          'You are about to send  "', 
          n_emails, '" email(s) to "',
          n_stores, '" store(s)'
        )
      )
    ),
    title = "Verify sending emails",
    size = "m",
    footer = list(
      modalButton("Cancel"),
      actionButton(
        ns("confirm_send"),"OK Send!", class = "btn-success",style="color: #fff;"
      )
    )
  )
  
}

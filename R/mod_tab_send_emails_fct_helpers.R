save_csi_to_disk <- function(dta) {
  
  stopifnot(all(c("store_code", "store_name", "data", "filename") %in% names(dta)))
  
  Sys.setlocale(locale = "greek")
  on.exit(Sys.setlocale(locale = "English_United Kingdom"))
  
  # create workbooks
  my_dta <- 
    dta %>% 
    rowwise() %>% 
    mutate(
      wb = list(as_excel_wb(data))
    )
  
  purrr::pwalk(
    select(my_dta, wb, filename),
    function(wb, filename){
      openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
    }
  )
  
  
}

as_excel_wb <- function(dta){
  
  wb <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(wb, "CSI")
  
  openxlsx::writeDataTable(wb, 1, dta,  tableStyle = "TableStyleMedium16")
  
  return(wb)
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
  
  dta_w_emails <- filter(dta, !purrr::map_lgl(email, is.null))
  
  # xs <- nrow(dta) - nrow(dta_w_emails)
  # print(xs)
  
  n_emails <- length(dta_w_emails$email)
  n_stores <- length(unique(dta_w_emails$store_code))
  
    #shinyjs::show(ns("ddd")) #, condition = xs>0, asis = TRUE)
  
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
      # ,shinyjs::hidden(
      #   div(id = "ddd", p("misshhhed"))
      # )
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

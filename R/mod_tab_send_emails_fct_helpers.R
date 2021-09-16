save_csi_to_disk <- function(dta) {
  
  
  stopifnot(all(c("store_code", "store_name", "data", "filename") %in% names(dta)))
  
  purrr::pwalk(
    select(dta, data, filename), 
    function(data, filename){
      writexl::write_xlsx(data, filename) 
    }
  )
  
}

#' Modal dialog for Sending verification
#' Informs the user on how many emails to how many stores are going to be send
#' 
#' @param session The current session object
#' @param dta 
#' @noRd
verify_send <- function(session, dta) {
  
  stopifnot(nrow(dta) > 0)
  ns <- session$ns
  
  n_emails <- length(unique(dta$email))
  n_stores <- length(unique(dta$store_code))
  
  modalDialog(
    div(
      style = "padding: 10px;",
      class = "text-center",
      h4(
        style = "line-height: 1.00;",
        paste0(
          'You are about to send  "', 
          n_emails, '" emails to "',
          n_stores, '" stores'
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

#' ui 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
buttons_edit <- function(ns){
  
  div(
    id = ns("tbl_buttons"),
    actionButton(ns("btn_add"), "Add", icon("plus"), 
                 style="color: #fff; background-color: #86af49"),
    actionButton(ns("btn_edit"), "Edit", icon("edit"), 
                 style="color: #fff; background-color: #337ab7"),
    actionButton(ns("btn_delete"), "Delete", icon("trash-alt"), 
                 class = "btn-danger",
                 style="color: #fff"),
    
    style = "margin-bottom:20px; margin-top:20px"
  )
}


#' Sidebar block showing the active config profile and where emails go
#'
#' Makes a dev/test configuration visible at a glance, so nobody mistakes it
#' for production (or the other way round).
#' @noRd
mode_badge <- function() {

  info <- app_mode_info()

  div(
    style = "margin-left:25px; font-size:12px; line-height:1.6;",
    div(paste0("Config: ", info$profile)),
    div(paste0("SMTP credentials: ", info$smtp_source)),
    div(
      paste0("Cashier emails: ", info$cashier_table),
      if (info$test_recipients) {
        span(" (TEST)", style = "color:#ff6b6b; font-weight:bold;")
      }
    )
  )
}

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

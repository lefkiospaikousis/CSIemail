#' notifications 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
message_email_failure <- function(message){
  
  shinyFeedback::showToast("error", 
                           message = message, 
                           keepVisible = TRUE,
                           .options = list(
                             positionClass = "toast-top-center"
                           ))
  
}

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



error_csi_file <- function(){
  
  shinyFeedback::hideFeedback("file_csi")
  shinyFeedback::showFeedbackDanger("file_csi", "Something went wrong with your CSI file. Probably not a valid csi file")
  
}

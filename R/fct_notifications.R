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



error_csi_file <- function(csi_type){
  
  shinyFeedback::hideFeedback("file_csi")
  shinyFeedback::showFeedbackDanger("file_csi", 
                                    paste0("Something went wrong with your data file. Probably not a valid ", csi_type, " file"))
  
}

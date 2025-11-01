#' email_helpers 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
mail_credentials <- function() {
  
  # at dev we are using a file
  # at ACS the creds key
  creds <- if(golem::app_dev()){
    blastula::creds_file(get_golem_config('smtp_creds'))
  } else {
    blastula::creds_key(get_golem_config('smtp_creds'))
  }
  
  return(creds)
  
}

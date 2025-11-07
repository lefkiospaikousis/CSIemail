#' email_cashier 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
email_cashier <- function(city, recipients, path_attachement, mail_credentials) {
  
  
  email_subject <- glue::glue('Accounts report for {city}')
  
  email_body <- blastula::md(
    
    glue::glue("Hello, 
               
               Please find attached the accounts report for {city}.
               
               
               Best regards,
               
               ACS Accounts
               ")
    
  )
  
  email <- blastula::compose_email(
    body = email_body,
    footer = blastula::md(glue::glue("This is an automated email by ACS CSI system."))
  )
  
  if (!is.null(path_attachement)) {
    email <- blastula::add_attachment(email, path_attachement)
  }
  
  email %>%
    blastula::smtp_send(
      from = mail_credentials$user,
      to = recipients,
      subject = email_subject,
      credentials = mail_credentials
    )
  
  cli::cli_alert_success("Emails sent to {city} cashier.")
  
  return(TRUE)
  
}


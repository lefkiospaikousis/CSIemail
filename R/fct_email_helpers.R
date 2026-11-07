#' email_helpers
#'
#' @description Where emails are sent from and to. Both decisions come from the
#'   config profile (inst/golem-config.yml), not from the `golem.app.prod`
#'   option, so one switch controls the behaviour:
#'   - `smtp_creds_source`: "file" (blastula creds file) or "keyring"
#'   - `cashier_emails_table`: DB table with the city cashier recipients
#'     ("city_emails" = real cashiers, "city_emails2" = test list)
#'
#'   If a key is missing (e.g. an older golem-config.yml on the server), the
#'   previous behaviour based on `golem::app_dev()` is used and a warning is
#'   logged.
#'
#' @noRd
mail_credentials <- function() {

  id <- get_golem_config("smtp_creds")

  creds <- switch(
    smtp_creds_source(),
    file    = blastula::creds_file(id),
    keyring = blastula::creds_key(id)
  )

  return(creds)

}


#' Where the SMTP credentials are read from: "file" or "keyring"
#' @noRd
smtp_creds_source <- function() {

  src <- get_golem_config("smtp_creds_source")

  if (is.null(src)) {
    src <- if (golem::app_dev()) "file" else "keyring"
    warn_missing_config_key("smtp_creds_source", src)
  }

  if (!src %in% c("file", "keyring")) {
    stop(
      "Config key 'smtp_creds_source' must be \"file\" or \"keyring\", not \"", src, "\"",
      call. = FALSE
    )
  }

  src
}


#' The DB table holding the city cashier email recipients
#' @noRd
cashier_emails_table <- function() {

  tbl_name <- get_golem_config("cashier_emails_table")

  if (is.null(tbl_name)) {
    tbl_name <- if (golem::app_dev()) "city_emails2" else db_tables[["city_emails"]]
    warn_missing_config_key("cashier_emails_table", tbl_name)
  }

  tbl_name
}


#' Summary of where emails go, for the sidebar and the startup log
#' @return list(profile, smtp_source, cashier_table, test_recipients)
#' @noRd
app_mode_info <- function() {

  cashier_table <- cashier_emails_table()

  list(
    profile         = active_config(),
    smtp_source     = smtp_creds_source(),
    cashier_table   = cashier_table,
    test_recipients = !identical(cashier_table, db_tables[["city_emails"]])
  )
}


warn_missing_config_key <- function(key, fallback) {

  cli::cli_alert_warning(
    "Config key {.field {key}} is missing from golem-config.yml (profile {.val {active_config()}}). Falling back to {.val {fallback}} based on golem.app.prod. Add the key to the config file."
  )
}

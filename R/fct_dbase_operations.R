#' Append a dataset to a table in DB 
#'
#'
#' @return TRUE/ FALSE depending on the succesfull update
#'
#' @noRd
append_to_db <- function(data, table_name) {
  
  tryCatch({
    
    path_db <- get_golem_config("db_path")
    
    conn <- DBI::dbConnect(RSQLite::SQLite(), path_db)
    
    DBI::dbAppendTable(conn, table_name, data)
    
    DBI::dbDisconnect(conn)
    
    cli::cli_alert_success("Successfully updated table: {table_name}")
    
    TRUE
  }, error = function(e) {
    
    cli::cli_alert_danger("Failed to update table: {table_name} - {e$message}")
    
    if (exists("conn")) {
      DBI::dbDisconnect(conn)
    }
    
    FALSE
  })
  

}


#' Read the moneygram agent -> ACS store mapping from the DB
#'
#' Reads the `moneygram_stores` table fresh and cleans it for joining onto a
#' moneygram statement: casts the `Agent ID` (stored as REAL) to character and
#' renames to `agent_id`/`store`. Read at point-of-use so that edits made in
#' the Database tab are picked up without an app restart.
#'
#' @param conn The RSQLite connection
#' @return A tibble with columns `agent_id` (character) and `store`
#' @noRd
get_moneygram_stores <- function(conn) {

  conn |>
    tbl("moneygram_stores") |>
    collect() |>
    mutate(`Agent ID` = as.character(`Agent ID`)) |>
    select(agent_id = `Agent ID`, store = `Acs store code`)

}


#' Check if a statemnt is already in the db based on the date
#'

check_statement_in_db <- function(statement_date, statement_type) {
  
  path_db <- get_golem_config("db_path")

  conn <- DBI::dbConnect(RSQLite::SQLite(), path_db)
  
  query <- glue::glue("SELECT COUNT(*) as count FROM '{statement_type}' WHERE date = '{statement_date}'")
  
  result <- DBI::dbGetQuery(conn, query)
  
  DBI::dbDisconnect(conn)
  
  statement_exists <- result$count[1] > 0
  
  return(statement_exists)
  
}





# Shared test fixtures.
#
# Tests never touch DB/csi_db.db (git-ignored, real data). Instead each test
# gets a small synthetic SQLite database with the same schema, deleted
# automatically when the test finishes.

#' A temporary SQLite DB with the reference tables and a few fake rows
#'
#' @param env Environment whose exit cleans up the connection and the file
#' @return A DBI connection
local_fixture_db <- function(env = parent.frame()) {

  path <- withr::local_tempfile(fileext = ".db", .local_envir = env)
  conn <- DBI::dbConnect(RSQLite::SQLite(), path)
  withr::defer(DBI::dbDisconnect(conn), envir = env)

  DBI::dbWriteTable(conn, "emails", data.frame(
    store_name = c("Store A", "Store B", "Store C"),
    email      = c("a@example.com", "b@example.com", "c@example.com"),
    uid        = c("uid-a", "uid-b", "uid-c"),
    store_code = c("AA", "BB", "CC")
  ))

  city_emails <- data.frame(
    city  = c("Nicosia", "Limassol"),
    email = c("nicosia@example.com", "limassol@example.com"),
    uuid  = c("ce-1", "ce-2")
  )
  DBI::dbWriteTable(conn, "city_emails", city_emails)
  DBI::dbWriteTable(conn, "city_emails2", city_emails)

  DBI::dbWriteTable(conn, "cashier_groups", data.frame(
    city  = c("Nicosia", "Nicosia", "Limassol"),
    store = c("AA", "BB", "CC"),
    uuid  = c("cg-1", "cg-2", "cg-3")
  ))

  # Spaced column names, REAL Agent ID - as in production
  DBI::dbExecute(conn, "CREATE TABLE moneygram_stores (`Agent ID` REAL, `Agent name` TEXT, `Acs store code` TEXT)")
  DBI::dbExecute(conn, "INSERT INTO moneygram_stores VALUES (1001, 'Agent A', 'AA'), (1002, 'Agent B', 'BB')")

  conn
}

#' Read a whole table keeping column names as-is
#' (DBI::dbReadTable() would turn `Agent ID` into `Agent.ID`)
read_tbl <- function(conn, table) {
  DBI::dbGetQuery(conn, paste0('SELECT * FROM "', table, '"'))
}

#' A one-row data frame like the value of a shiny fileInput
file_input <- function(path) {
  data.frame(name = basename(path), size = file.size(path), type = "", datapath = path)
}

#' Path to the git-ignored SampleData folder (may not exist on every clone)
sample_data_path <- function(...) {
  file.path(pkgload::pkg_path(), "SampleData", ...)
}

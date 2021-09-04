# Configuration, database and onStop

# RSQLite connection ------------------------------------------------------

configuration <- Sys.getenv("GOLEM_CONFIG_ACTIVE", "default")
path <- get_golem_config("db_path", configuration)

email_db <- DBI::dbConnect(RSQLite::SQLite(), path)

shiny::onStop(function(){
  
  cat("Doing application cleanup\n")
  cat("-----\n Removing connections\n")
  
  DBI::dbDisconnect(email_db)
  
})

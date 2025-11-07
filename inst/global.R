# Configuration, database and onStop

### NOT USED YET. NEED TO ADD it to run_app.R
### Not tested yet

# RSQLite connection ------------------------------------------------------

dbase_csi <- DBI::dbConnect(
  RSQLite::SQLite(),
  CSIemail::get_golem_config("db_path"))

shiny::onStop(function(){
  
  cat("Doing application cleanup\n")
  cat("-----\n Removing connections\n")
  
  DBI::dbDisconnect(dbase_csi)
})

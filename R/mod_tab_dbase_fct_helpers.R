
#' Function to label mamdatory fields
#' 
#' This function will be used later on to mark any fields in the entry form that
#' are mandatory.
#' Taken from 
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star { color: red; }"


#' Entry form to add a new store & email
#' a modalDialog. Needs to be a function to run properly (lazy loading?)
entry_form <- function(session) {
  
  ns <- session$ns
  
  modalDialog(
    tagList(
      h3("Add a new store"),
      textInput(ns("store_code"), "Store Code"),
      textInput(ns("store_name"), "Store Name"),
      textInput(ns("email"), "Email")
    ),
    tagList(
      actionButton(ns("submit"), "Submit", class = "btn-success"),
      modalButton("Cancel")
    )
    
    , footer = NULL
  )
  
}


#' Appends rows into a table
#' 
#' @param table String, The name of the DB table
#' @param value The dataframe of values.  See DBI::dbAppendTable()
#' @noRd
#' @export
append_data <- function(conn, table, value){
  
  if (!table %in% DBI::dbListTables(conn)) {
    stop("No such table in the DB")
  }
  
  DBI::dbAppendTable(conn, name = table, value = value)
  
}

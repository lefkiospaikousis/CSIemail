
#' Function to label mamdatory fields
#' 
#' This function will be used later on to mark any fields in the entry form that
#' are mandatory.
#' Taken from 
#' @noRd
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star { color: red; }"


#' Entry form to add a new store & email
#' a modalDialog. Needs to be a function to run properly (lazy loading?)
#' @noRd
entry_form <- function(session, edit = FALSE, store = NULL) {
  
  ns <- session$ns
  
  if(edit && is.null(store)) {
    stop("No store given")
    }
  
  
  mode <- if(edit) {"Edit"} else {"Add"}
  title <- glue("{mode} a store")
  
  modalDialog(
    tagList(
      h3(title),
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

#' Modal dialog for delete verification
#' 
#' @param session The current session object
#' @param store A one-row dataframe of the store
#' @noRd
verify_delete <- function(session, store) {
  
  stopifnot(nrow(store) == 1)
  ns <- session$ns
  
  modalDialog(
    div(
      style = "padding: 10px;",
      class = "text-center",
      h4(
        style = "line-height: 1.00;",
        paste0(
          'Are you sure you want to delete the email "', 
          store$email, '" from store "',
          store$store_name, '"?'
        )
      )
    ),
    title = "Delete a store's email",
    size = "m",
    footer = list(
      modalButton("Cancel"),
      actionButton(
        ns("submit_delete"),"Delete", class = "btn-danger",
        style="color: #fff;"
      )
    )
  )
  
}


#' Appends rows into a table
#' 
#' @param conn The connection object
#' @param table String, The name of the DB table
#' @param value The dataframe of values.  See DBI::dbAppendTable()
#' @export
append_data <- function(conn, table, value){
  
  if (!table %in% DBI::dbListTables(conn)) {
    stop("No such table in the DB")
  }
  
  DBI::dbAppendTable(conn, name = table, value = value)
  
}


#' removes rows from DB
#' @noRd
delete_data <- function(conn, table, col, ids = character(0)){
  
  if (!table %in% DBI::dbListTables(conn)) {
    stop("No such table in the DB")
  }
  
  sql <- paste0('DELETE FROM ', table, ' WHERE ', col, ' = :x')
  
  rs <- DBI::dbSendStatement(conn, sql)
  
  DBI::dbBind(rs, params = list(x = ids))
  
  DBI::dbClearResult(rs)
  
}



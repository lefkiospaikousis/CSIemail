
#' Read in a Ticket Hour CSI
#' @param path A path to a .csv file
read_ticket_hour <- function(path){
  
  stopifnot(tools::file_ext(path) == "csv")
  
  col_names <-
    c("store_code", "store_name", 
      "contact", "EMPTY", "th_id", 
      "items", "type", "date", "ref", "details",
      "value", "star", "os", "debit", "credit"
    )
  
  temp <- safe_readCSV(path, 
                       col_names = col_names, 
                       locale = readr::locale(encoding = 'windows-1253')
  ) 
  
  if(is.null(temp$result)) {
    
    return(NULL) 
    
  } else {
    
    temp$result %>% 
      select(all_of(col_names)) %>%
      select(-EMPTY, -star, -contact) 
    
  } 
  
}


#' Read an ACS CSI
#' @param path A path to a .xlsx or .xls file
read_acs_csi <- function(path){
  
  
  stopifnot(tools::file_ext(path) %in% c("xls", "xlsx"))
  
  temp <- safe_readXL(path)
  
  if(is.null(temp$result)) {
    
    return(NULL) 
    
  } else {
    
    temp$result %>% 
      rename(
        date = ...1,
        store_code = ...2
      ) %>% 
      mutate(AWB = as.character(AWB))
    
  } 
  
}


#' Add row totals to the data
#' @param dta The tibble of csi
#' @param ... The variables to add totals to
#' @noRd
add_totals <- function(dta, ...){
  
  janitor::adorn_totals(dta, 
                        where = "row", 
                        fill = "", 
                        na.rm = TRUE, 
                        name = "Total",
                        ...
  )
}




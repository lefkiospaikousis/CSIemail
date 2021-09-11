



save_csi_to_disk <- function(dta) {
  
  
  stopifnot(all(c("store_code", "store_name", "data", "filename") %in% names(dta)))

    purrr::pwalk(
      select(dta, data, filename), 
      function(data, filename){
        writexl::write_xlsx(data, filename) 
      }
    )
  
}


#' Load BANK statement
#' 
#' @param path The path to the file
#' @param bank String.One of c("HB", "BOC")
#' 
load_bank <- function(path, bank){
  
  labels <- tibble::deframe(column_names[[bank]])
  
  
  temp <- safe_readXL(path)
  
  if(is.null(temp$result)) {
    message("Failed to load the Bank statement")
    return(NULL)
    
  } else{
    
    dta <- temp$result
    
    vars_check <- check_var_names_bank(dta, bank)
    
    if(is.null(vars_check)){
      
      # in BOC some columns come in empty .NA
      emptycols <- colSums(is.na(dta)) == nrow(dta)
      # cheque number of BOC sometimes comes empty
      if(bank == "BOC") emptycols["Cheque Number:"] <- FALSE
      
      dta <- dta[!emptycols]
      
      dta <- dplyr::select(dta, !!!dplyr::all_of(labels))
      
      # Date column types
      
      # BOC
      if(inherits(dta$date, "POSIXct")){
        dta <- 
          dta %>% 
          dplyr::mutate(
            #dplyr::across(c(date, value_date),~as.Date(.))
            dplyr::across(c(date),~as.Date(.))
          ) %>% 
          dplyr::mutate(cheque_no = as.character(cheque_no))
      } else {
        dta <- 
          dta %>% 
          dplyr::mutate(
            #dplyr::across(c(date, value_date), ~lubridate::dmy(.))
            dplyr::across(c(date), ~lubridate::dmy(.))
          )
        
      }
      
      return(dta)
      
    } else  {
      
      msg <-  paste(vars_check, collapse = ", ")
      error_in_bank(msg,bank)
      return(NULL)
    }
  }
  
}

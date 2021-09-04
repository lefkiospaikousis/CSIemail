# Functions that do data validation of loaded files



check_filetype <- function(path){
  
  message <- "No valid type of the file. The valid files are .xlsx, .xls, and .csv"
  ext <- tools::file_ext(path)
  
  if(!ext %in% accepted_files){
    
    #stop(message)
    
    shiny::validate(message)
    
  } else TRUE
  
}

#' Check the column names of a loaded Bank dataset
#' @param dta The loaded data
#' @param bank String length 1. The name of the bank.Takes the global value \code{banks}
#' @return NULL if all ok, else a character vector of names that are missing
check_var_names_bank <- function(dta, bank){
  
  if(!bank %in% names(column_names)){
    stop("No col names defiend for this bank")
  }
  
  valid_names <- column_names[[bank]]$col_label
  
  nms <- names(dta)
  
  if(all( valid_names %in% nms)){
    
    return(NULL)
    
  } else{
    
    miss_cols <-  setdiff(valid_names, nms)
    
    return(miss_cols)

  }
  
}

#' Check the column names of a loaded SAGE dataset
#' @param dta The loaded data
#' @return NULL if all ok, else a character vector of names that are missing
check_var_names_SAGE <- function(dta){
  
  
  valid_names <- column_names[["SAGE"]]$col_label
  
  nms <- names(dta)
  
  if(all( valid_names %in% nms)){
    
    return(NULL)
    
  } else{
    
    miss_cols <-  setdiff(valid_names, nms)
    
    return(miss_cols)
    
  }
  
}

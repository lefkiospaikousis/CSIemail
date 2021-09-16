# Functions that do data validation of loaded files


#' Check that the file has the correct type
#' 
#' @param path The path of the file
check_filetype <- function(path){
  
  message <- "No valid type of the file. The valid files are .xlsx, .xls, and .csv"
  ext <- tools::file_ext(path)
  
  if(!ext %in% accepted_files){
    
    #stop(message)
    
    shiny::validate(message)
    
  } else TRUE
  
}


#' Check that the data supplied has the correct variable names
#' 
#' @param dta Tibble. The uploaded dataset?
#' @param type String length 1. One of `csi` or `ticket`
check_var_names <- function(dta, type){
  
  if(!type %in% names(column_names)){
    stop("No col names defiend for this file")
  }
  
  valid_names <- column_names[[type]]$col_label
  
  nms <- names(dta)
  
  if(all( valid_names %in% nms)){
    
    return(NULL)
    
  } else{
    
    miss_cols <-  setdiff(valid_names, nms)
    
    return(miss_cols)

  }
  
}



#' Read in a Ticket Hour CSI
#' @param path A path to a .csv file
read_ticket_hour <- function(path){
  
  #stopifnot(tools::file_ext(path) == "csv")
  stopifnot(tools::file_ext(path) %in% c("xls", "xlsx"))
  
  # read it as excel
  temp <- safe_readXL(path, col_names = FALSE)
  
  if(is.null(temp$result)) {
    
    return(NULL) 
    
  } else {
    
    out <- 
    temp$result %>% 
      select(1:15) %>% 
      setNames(unname(col_names_ticket)) %>% 
      select(-EMPTY, -star, -contact, -value, -ticket_id, -store_name, -items) %>% 
      {.}
    
    # if any of os, debit, credit is empty, then is read as logical
    logical <- select(out, all_of(vars_sum_ticketHour)) %>% purrr::keep(is.logical) 
    
    
    if(ncol(logical) == length(vars_sum_ticketHour)){
      
      return(NULL)
    }
    
    out
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




#' Process the uploaded ACS csi file
#' 
#' Only the ACS file
#' 
#' @param csi A tibble of the csi
#' @details This table is as is read by the readxl function
#' @return A tibble. A cleaned version of the csi transactions 
#' @noRd
process_csi <- function(csi) {
  
  stopifnot(inherits(csi, "data.frame"))
  stopifnot(c("store_code", "AWB") %in% names(csi))
  
  # NOTE: There are some stores who have multiple entries. No problem
  
  csi_clean <- 
    csi %>% 
    mutate(
      store_code = stringr::str_extract(store_code, "(?<=: ).+")
    ) %>%
    mutate(
      store_code = stringr::str_trim(store_code)
    ) %>% 
    tidyr::fill(store_code) %>% 
    slice(-c(1,2)) %>% 
    select(-1) %>%
    # The totals rows will be removed
    # Note that totals are with commas (,) 
    # We dont need them, will calculate later. 
    na.omit() %>% 
    mutate(
      across(AWB:last_col(), as.double)
    ) %>% 
    {.}
  
  csi_clean[[3]] <- anonymise2(csi_clean[[3]])
            
  csi_clean
  
}

#' Process the uploaded TH Sales file
#' 
#' Only the TH Sales file
#' 
#' @param csi A tibble of the TH sales
#' @details This table is as is read by the readxl function
#' @return A tibble. A cleaned version of the TH transactions 
#' @noRd
process_th <- function(csi) {
  
  stopifnot(inherits(csi, "data.frame"))
  stopifnot(c("date") %in% names(csi))
  
  # NOTE: There are some stores who have multiple entries. No problem
  
  csi_clean <- 
    csi %>% 
    mutate(
      date = lubridate::ymd(date)
    ) %>% 
    mutate(
      date = format(date, "%d/%m/%Y")
    ) %>% 
    {.}
  
  csi_clean
  
}


#' Return the date of the csi file
#' 
#' @param csi A tibble of the csi
#' @param csi_type String length 1. One of c("ACS CSI", "Ticket Hour Sales")
#' @return The csi date. Character type
#' @noRd
get_csi_date <- function(csi, csi_type){
  
  stopifnot(inherits(csi, "data.frame"))
  stopifnot("date" %in% names(csi))
  
  date <- switch(csi_type,
                 "ACS CSI" =  csi$date[1] %>% 
                   stringr::str_extract("(?<=: ).+"),
                 
                 "Ticket Hour Sales" = c(from = min(csi$date, na.rm = TRUE),
                                   to = max(csi$date, na.rm = TRUE)
                                   ),
                 
                 stop("Invalid CSI type")
  )
  
  date
  
}


anonymise <- function(string){
  
  n_char <- nchar(string)
  
  ind <- sample(seq_len(n_char), round(n_char*0.50))
  
  # Split and replace
  my_letters <- strsplit(string, "") %>% unlist() 
  my_letters[ind] <- "*"
  
  # return
  paste0(my_letters, collapse = "")
  
}

#' Vectorised version of anonymise
anonymise2 <- Vectorize(anonymise)


#' Process the uploaded csi file
#' 
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
  
  
  csi_clean
  
}

#' Return the date of the csi file
#' 
#' @param csi A tibble of the csi
#' @return The csi date. Character type
#' @noRd
get_csi_date <- function(csi){
  
  stopifnot(inherits(csi, "data.frame"))
  stopifnot("date" %in% names(csi))
  
  csi$date[1] %>% 
    stringr::str_extract("(?<=: ).+") 
  
}















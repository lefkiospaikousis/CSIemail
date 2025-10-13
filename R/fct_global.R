

#' Get the icon  as a character vector to pass to the DT tables
#' @noRd
get_icon <- function(x) as.character(shiny::icon(x))


#' Extracts the date from the path name
#' @noRd
moneygram_date <- function(path) {
  
  date <- stringr::str_extract(basename(path), "\\d{8}")
  lubridate::dmy(date)
  
}

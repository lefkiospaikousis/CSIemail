

#' Get the icon  as a character vector to pass to the DT tables
#' @noRd
get_icon <- function(x) as.character(shiny::icon(x))


#' Extracts the date from the path name
#' @noRd
moneygram_date <- function(path) {
  
  date <- stringr::str_extract(basename(path), "\\d{8}")
  lubridate::dmy(date)
  
}


icon_success <- function() icon('check', style = 'color:green;font-size:14px')
icon_failure <- function() icon('xmark', style = 'color:red;font-size:14px')

icon_success_round <- function() icon('circle-check', style = 'color:green;font-size:20px')

icon_warning <- function() icon('exclamation-triangle', style = 'color:orange;font-size:20px')

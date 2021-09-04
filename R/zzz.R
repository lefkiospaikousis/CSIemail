
#' Safe read an excel file
#' 
#' to construct it I need a "dummy"" output first and the
#' construct it in the .onLoad
#' see https://purrr.tidyverse.org/reference/faq-adverbs-export.html
#' We do it this wway so that the safe_readXL is created on packgae load
#' to  get the latest update of purrr
#' @param ... temporary
safe_readXL <- function(...) "dummy"


.onLoad <- function(libname, pkgname){

  #safe_readXL <<- purrr::safely(readxl::read_excel)

  safe_readXL <<- purrr::safely(readxl::read_excel)
}


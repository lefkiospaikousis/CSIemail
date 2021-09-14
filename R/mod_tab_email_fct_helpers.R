
#' Function to label mamdatory fields
#' 
#' This function will be used later on to mark any fields in the entry form that are mandatory.
#' Taken from 
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star { color: red; }"

#' Function to save the forma data into df format
#' 

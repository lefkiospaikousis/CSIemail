## code to prepare `create_global_values` dataset goes here

# Ticket Hour Column names
col_names_ticket <-
  
  # Column label - Variable name
  c(
    "Store Code" = "store_code", 
    "Store name" = "store_name",
    "Contact" = "contact", 
    "EMPTY" = "EMPTY", 
    "Ticket Hour ID" = "ticket_id",
    "Items" = "items", 
    "Type" = "type", 
    "Date" = "date", 
    "Reference" = "ref",
    "Details" = "details",
    "Value" = "value", 
    "star" = "star", 
    "os" = "os", 
    "Debit" = "debit", 
    "Credit" = "credit"
  )


vars_sum_ticketHour <- c("os", "debit", "credit")

usethis::use_data(
  col_names_ticket
  , vars_sum_ticketHour
  , internal = TRUE
  , overwrite = TRUE)

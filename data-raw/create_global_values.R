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

# Excel templates

xl_template_ACS_CSI <- openxlsx::loadWorkbook("data-raw/ACS CSI template.xlsx")

xl_template_TH_CSI <- openxlsx::loadWorkbook("data-raw/TH CSI template.xlsx")


usethis::use_data(
  col_names_ticket
  , vars_sum_ticketHour
  , xl_template_ACS_CSI
  , xl_template_TH_CSI
  , internal = TRUE
  , overwrite = TRUE
  )

#' Code to prepare global values
#' 
#' 

# Ticket Hour Column names ------------------------------------------------------------

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


# Cashier per Store - Column names in Greek ------------------------------------------------------------

names_cashier_per_store <- 
  
  c('courier' = "courier" , 
    'trans' = '\u039C\u03B5\u03C4\u03B1\u03C6.',# "Μεταφ.", 
    'buy_pay' = "\u0391\u03B3\u03BF\u03C1. \u03A0\u03BB\u03B7\u03C1\u03C9\u03BC\u03AD\u03C2", # "Αγορ. Πληρωμές", 
    'buy_get' = "\u0391\u03B3\u03BF\u03C1. \u0395\u03B9\u03C3\u03C0\u03C1\u03AC\u03BE\u03B5\u03B9\u03C2",# "Αγορ. Εισπράξεις", 
    'an_cash' = "\u0391\u03BD\u03C4\u03B9\u03BA. \u039C\u03B5\u03C4\u03C1\u03B7\u03C4\u03AC", # "Αντικ. Μετρητά",
    'total_cash' ="\u03A3\u03CD\u03BD\u03BF\u03BB\u03BF \u039C\u03B5\u03C4\u03C1\u03B7\u03C4\u03AC",# "Σύνολο Μετρητά", 
    'an_cheques' = "\u0391\u03BD\u03C4\u03B9\u03BA. \u0395\u03C0\u03B9\u03C4\u03B1\u03B3\u03AD\u03C2", #"Αντικ. Επιταγές", 
    'tip' = "Tip" , 
    'total_card' = "\u03A3\u03CD\u03BD\u03BF\u03BB\u03BF \u039A\u03AC\u03C1\u03C4\u03B1" # "Σύνολο Κάρτα"
  )

vars_sum_ticketHour <- c("os", "debit", "credit")

# What shwomn the UI and he value is the db table name
statement_types <- c("Cashier per Store" = 'cashier_per_store', "Moneygram" = 'moneygram')


# DBASE TABLES ----------------------------------------------------------------

db_tables <- list(
  cashier_per_store = "cashier_per_store",
  cashier_groups = "cashier_groups",
  city_emails = "city_emails",
  store_emails = "emails"
)


# EXCEL templates -------------------------------------------------------------

#xl_template_store_monitoring <- openxlsx::loadWorkbook('data-raw/template_report.xlsx')

xl_template_ACS_CSI <- openxlsx::loadWorkbook("data-raw/ACS CSI template.xlsx")

xl_template_TH_CSI <- openxlsx::loadWorkbook("data-raw/TH CSI template.xlsx")


# Save as internal data --------------------------------------------------------

usethis::use_data(
  col_names_ticket
  , vars_sum_ticketHour
  , db_tables
  , xl_template_ACS_CSI
  , xl_template_TH_CSI
  , xl_template_store_monitoring
  , names_cashier_per_store
  , statement_types
  , internal = TRUE
  , overwrite = TRUE
)

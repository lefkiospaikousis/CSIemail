#' Create store code and email dta from the excel

library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(stringr)
library(readxl)


tbl_emails <- readxl::read_excel("data-raw/store_emails.xlsx", col_names = c("store", "email"))

(
  tbl_emails_clean <- 
    tbl_emails %>% 
    rename(store_name = store) %>% 
    tidyr::separate_rows(email, sep = ";") %>% 
    mutate(
      uid = uuid::UUIDgenerate(n = nrow(cur_data())),
      store_code = str_sub(store_name, -3, -1),
      store_code = str_remove(store_code, "\\)")
      # this code extracts whats in the parenthesis
      #store_code = str_extract_all(store_name, "\\(([^)]+)\\)")
    ) %>% 
    mutate(store_code = if_else(store_code == "ara", "CD", store_code)) %>% 
    mutate(email = str_trim(email)) 
  
)

# sample emails to test
tbl_emails_clean <-
  tbl_emails_clean %>%
  bind_rows(
    tibble(
      uid = uuid::UUIDgenerate(n = 3),
      store_code = c("XX", "XX", "YY"),
      email = c("lefkiospaikousis@yahoo.com", "lefkiospaikousis@yahoo.co.uk", "lefkiospaik@gmail.com"),
      store_name = "Lefkios"
    )
    
  )

# extra emails for TH

th_emails <- readxl::read_excel("data-raw/store_emails_ticket_hour.xlsx")

th_emails <- 
  th_emails %>%
  tidyr::separate_rows(email, sep = "; ") %>% 
  mutate(email = str_trim(email)) %>% 
  rowwise() %>% 
  mutate(uid = uuid::UUIDgenerate()) %>% 
  ungroup()


all_emails <- 
  bind_rows(
    tbl_emails_clean, th_emails
  )

saveRDS(all_emails, here::here("DB/tbl_store_emails.rds"))


dbase <- DBI::dbConnect(RSQLite::SQLite(), "DB/csi_db.db")

DBI::dbListTables(dbase)

# !! This will overwrite the table
DBI::dbWriteTable(dbase, "emails", all_emails
                  , overwrite = TRUE
)

dbase %>% 
  tbl("emails")

DBI::dbDisconnect(dbase)



#' prepare_store_monitoring 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
prepare_store_monitoring <- function(wb, dta_cashier, dta_moneygram, city_name = 'Tamiaki') {
  
  
  cli::cli_alert_info("Preparing store monitoring for city {.val {city_name}}")
  
  totals_shop <- dta_cashier |> 
    group_by(store) |>
    summarise(
      store_cash = sum(total_cash, na.rm = TRUE),
      store_visa = sum(total_card, na.rm = TRUE)
    ) |> 
    left_join(
      dta_moneygram |> select(store, store_moneygram = total), 
      by = c("store")
    ) 
  
  
  
  all_stores <- dta_cashier |> 
    select(store, courier, total_cash) |>
    group_by(store) |> 
    nest(.key = 'per_courier') |>
    left_join(totals_shop, by = 'store') |> 
    mutate(
      # add store_cash as a row in data
      per_courier = list(
        bind_rows(
          tibble(
            courier = paste0(" "),
            total_cash = store_cash  # under the courier cash (total cash_) to add the total store cash
          ),
          per_courier
        )
      )
    ) |>
    ungroup()
  
  
  cli::cli_alert_info("Loaded template for store monitoring")
  
  col_tamiaki <- 2
  
  col_cash_received <- 3
  col_visa <- 4
  col_cheques <- 5
  col_difference <- 6
  
  col_notes <- 7
  
  col_moneygram_owed <- 9
  col_moneygram_received <- 10
  col_moneygram_difference <- 11
  col_total_tamiaki_moneygram <- 13
  
  font_size <- 14
  font_size_14_bold <- openxlsx::createStyle(fontSize = font_size, textDecoration = 'bold')
  
  total_csi_moneygram = sum(totals_shop$store_moneygram, na.rm = TRUE) + sum(totals_shop$store_cash, na.rm = TRUE)
  
  unlock <- openxlsx::createStyle(locked = FALSE)
  
  writeData(wb, 'Sheet1', 
            total_csi_moneygram, 
            startRow = 2, 
            startCol = 13, 
            withFilter = FALSE,
            colNames = FALSE
  )
  
  openxlsx::addStyle(wb, 'Sheet1',
                     style = openxlsx::createStyle(textDecoration = 'bold'), 
                     rows = 1:2, cols = 1:col_total_tamiaki_moneygram,
                     gridExpand = TRUE, stack = TRUE
  )
  
  # Cells difference make them red
  openxlsx::addStyle(wb, 'Sheet1',
                     style = openxlsx::createStyle(fontColour  = 'red'),
                     rows = 2, cols = col_difference,
                     gridExpand = TRUE, stack = TRUE
  )
  
  openxlsx::addStyle(wb, 'Sheet1', 
                     style = openxlsx::createStyle(fontColour  = 'red'),
                     rows = 2, cols = col_moneygram_difference,
                     gridExpand = TRUE, stack = TRUE
  )
  
  
  # Unlock the Deposit cells
  openxlsx::addStyle(wb, 'Sheet1', style = unlock,
                     rows = 27:28, cols = col_total_tamiaki_moneygram + 1 ,
                     gridExpand = TRUE, stack = TRUE
  )
  
  # unlock the empty cells for the other cheques
  openxlsx::addStyle(wb, 'Sheet1', style = unlock,
                     rows = 9:18, cols = c(col_total_tamiaki_moneygram, col_total_tamiaki_moneygram + 1),
                     gridExpand = TRUE, stack = TRUE
  )
  
  
  row_start <- 3
  
  
  for (store in all_stores$store){
    
    cli::cli_alert_info("Processing store {store}")
    
    #store = 'CA'
    
    tbl_store <- all_stores |> 
      filter(store == !!store) |> 
      unnest(cols = c(per_courier)) |> 
      select(store, courier, total_cash) |> 
      # arrange the row with courier = Store Total and put it last
      arrange(store, courier == " ") |> 
      ungroup()
    
    # STORE NAME
    # Write the store name as headline
    writeData(wb, 'Sheet1', 
              paste0('Store: ', store), 
              startRow = row_start, startCol = 1, 
              colNames = FALSE
    )
    
    # Bold the STORE NAME
    openxlsx::addStyle(wb, 'Sheet1', style = font_size_14_bold,
                       rows = row_start, cols = 1,
                       gridExpand = TRUE, stack = TRUE
    )
    
    
    
    ## Courier data -----------------------------------------------------------
    writeData(wb, 'Sheet1', 
              tbl_store |> select(-store), 
              startRow = row_start + 1, 
              startCol = 1, 
              colNames = FALSE
    )
    
    # style font size
    
    openxlsx::addStyle(wb, 'Sheet1', 
                       style = openxlsx::createStyle(fontSize = font_size),
                       rows = (row_start + 1):((row_start + 1) + nrow(tbl_store) - 1), 
                       cols = 1:2,
                       gridExpand = TRUE, stack = TRUE
    )
    
    # Write the formula for the difference
    length_entries <- nrow(tbl_store) - 1 # remove the total row
    
    writeFormula(wb, 'Sheet1', 
                 x = rep('(cash_received+visa_received+cheques_received)-tamiaki', length_entries),
                 startRow = (row_start + 1):((row_start + 1) + length_entries), 
                 startCol = col_difference
    )
    
    # Style the font size in the table
    openxlsx::addStyle(wb, 'Sheet1', 
                       style = openxlsx::createStyle(fontSize = font_size),
                       rows = (row_start + 1):((row_start + 1) + length_entries), 
                       cols = col_difference,
                       gridExpand = TRUE, stack = TRUE
    )
    
    
    # style to make the last row of the table (the total store) as bold
    openxlsx::addStyle(wb, 'Sheet1',
                       style = font_size_14_bold,
                       rows = nrow(tbl_store) + row_start,
                       cols = col_tamiaki, 
                       gridExpand = TRUE, stack = TRUE
    )
    
    
    # style all the cells in the table with borders
    rows_start_from = (row_start + 1)
    rows_end_to = (row_start + 1 + nrow(tbl_store) - 2)
    
    openxlsx::addStyle( wb, 'Sheet1',
                        style = openxlsx::createStyle(border = c('top', 'bottom', 'left', 'right')),
                        rows = rows_start_from:rows_end_to,
                        cols = c(col_cash_received:col_cheques, col_notes),
                        gridExpand = TRUE, stack = TRUE
    )
    
    # Unlock The User Input
    openxlsx::addStyle(wb, 'Sheet1', 
                       style = openxlsx::createStyle(locked = FALSE, fontSize = font_size),
                       rows = rows_start_from:rows_end_to, 
                       cols = c(col_cash_received:col_cheques, col_notes),
                       gridExpand = TRUE, stack = TRUE
    )
    
    
    ## Moneygram ---------------------------------------------------------------
    store_moneygram <- all_stores |> 
      filter(store == !!store) |>
      select(store_moneygram) 
    
    # we start at the store level row , which is the row_start
    writeData(wb, 'Sheet1', 
              store_moneygram, 
              startRow = row_start, 
              startCol = col_moneygram_owed, 
              colNames = FALSE
    )
    
    # Write the formula for the difference
    writeFormula(wb, 'Sheet1', 
                 x = 'moneygram_received-moneygram_owed',
                 startRow = row_start, 
                 startCol = col_moneygram_difference
    )
    
    # style the 3 cells in the row with borders
    openxlsx::addStyle(wb, 'Sheet1',
                       style = openxlsx::createStyle(border = c('top', 'bottom', 'left', 'right')),
                       rows = row_start,
                       cols = col_moneygram_owed:col_moneygram_difference,
                       gridExpand = TRUE, stack = TRUE
    )
    
    # style the received and difference cell as grey
    openxlsx::addStyle(wb, 'Sheet1',
                       style = openxlsx::createStyle(fgFill = 'yellow'),
                       rows = row_start,
                       cols = c(col_moneygram_owed, col_moneygram_difference),
                       gridExpand = TRUE, stack = TRUE
    )
    
    openxlsx::addStyle(wb, 'Sheet1', style = unlock,
                       rows = row_start, cols = col_moneygram_received,
                       gridExpand = TRUE, stack = TRUE
    )
    
    # update the row start for the next store
    row_start <- row_start + 2 + nrow(tbl_store)
    
  }
  
  #RENAME SHEET
  #openxlsx::renameWorksheet(wb, 'Sheet1', city_name)
  cli::cli_alert_success("Finished preparing store monitoring for city {.val {city_name}}")
  
}

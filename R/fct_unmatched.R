#' Rows of the loaded statements that will not appear in any city report
#'
#' A store only appears in a cashier report if it is listed in the
#' `cashier_groups` table, and a Moneygram row only gets a store if its
#' Agent ID is mapped in `moneygram_stores`. Everything else is dropped
#' silently by the report generation, so this lists it for the user.
#'
#' @param cashier The Cashier per Store statement (Greek headers, `store` col), or NULL
#' @param moneygram The Moneygram statement after the mapping join
#'   (`store`, `agent_id`, `agent_name`, `total`), or NULL
#' @param viva The VIVA statement (`store`, `Amount`), or NULL
#' @param groups The `cashier_groups` table (`city`, `store`)
#' @return A tibble with columns `statement`, `store`, `detail`, `amount`
#'   (zero rows when everything matches)
#' @noRd
find_unmatched <- function(cashier = NULL, moneygram = NULL, viva = NULL, groups) {

  known_stores <- unique(stats::na.omit(groups$store))

  empty <- tibble::tibble(
    statement = character(),
    store     = character(),
    detail    = character(),
    amount    = numeric()
  )

  # Stores present in a statement but missing from cashier_groups
  stores_not_grouped <- function(dta, amount_col, statement) {

    if (is.null(dta)) return(empty)

    dta |>
      filter(!is.na(store), !store %in% known_stores) |>
      group_by(store) |>
      summarise(amount = sum(.data[[amount_col]], na.rm = TRUE), .groups = "drop") |>
      mutate(
        statement = statement,
        detail    = "Store not in Cashier Groups"
      )
  }

  # Moneygram agents without a store mapping
  agents_not_mapped <- if (is.null(moneygram)) {
    empty
  } else {
    moneygram |>
      filter(is.na(store)) |>
      transmute(
        statement = "Moneygram",
        store     = NA_character_,
        detail    = paste0("Agent ID ", agent_id, " (", agent_name, ") not in Moneygram Stores"),
        amount    = total
      )
  }

  bind_rows(
    empty,
    stores_not_grouped(cashier, names_cashier_per_store[["total_cash"]], "Cashier per Store"),
    agents_not_mapped,
    stores_not_grouped(moneygram, "total", "Moneygram"),
    stores_not_grouped(viva, "Amount", "VIVA")
  ) |>
    select(statement, store, detail, amount)

}


#' Warning block for the report modal listing the unmatched rows
#'
#' @param unmatched Output of `find_unmatched()`
#' @return A tag, or NULL when there is nothing to report
#' @noRd
unmatched_warning_ui <- function(unmatched) {

  if (is.null(unmatched) || nrow(unmatched) == 0) return(NULL)

  items <- paste0(
    unmatched$statement, ": ",
    ifelse(is.na(unmatched$store), "", paste0(unmatched$store, " - ")),
    unmatched$detail,
    " (amount ", format(round(unmatched$amount, 2), nsmall = 2), ")"
  )

  div(
    class = "alert alert-warning",
    style = "max-height: 200px; overflow-y: auto;",
    tags$b(icon("exclamation-triangle"),
           glue::glue(" {nrow(unmatched)} row(s) of the loaded statements will NOT appear in any report:")),
    tags$ul(lapply(items, tags$li)),
    p(
      "Missing stores: add them in Database › Cashier Groups (picked up on the next Generate).",
      br(),
      "Unmapped Moneygram agents: add them in Database › Moneygram Stores, then load the Moneygram file again."
    )
  )
}

# The four Database-tab CRUD modules share one pattern: rv$*_to_edit NULL means
# INSERT, otherwise UPDATE that row. Cancel is a plain modalButton the server
# never sees, so Add must clear the edit target itself. These tests guard the
# 2026-11-07 fix for "Edit -> Cancel -> Add overwrites the edited row".

crud_cases <- list(
  list(
    label  = "Store Emails",
    server = mod_tab_dbase_server, table = "emails", key = "uid",
    select = "tbl_emails_rows_selected",
    new    = list(store_code = "ZZ", store_name = "New store", email = "new@example.com"),
    edit   = list(store_code = "BB", store_name = "Edited store", email = "b@example.com"),
    edited_col = "store_name", edited_val = "Edited store"
  ),
  list(
    label  = "Cashier Groups Emails",
    server = mod_cachier_groups_emails_server, table = "city_emails", key = "uuid",
    select = "city_emails_rows_selected",
    new    = list(city = "Paphos", email = "paphos@example.com"),
    edit   = list(city = "Larnaca", email = "limassol@example.com"),
    edited_col = "city", edited_val = "Larnaca"
  ),
  list(
    label  = "Cashier Groups",
    server = mod_cashier_groups_server, table = "cashier_groups", key = "uuid",
    select = "cashier_groups_rows_selected",
    new    = list(city = "Paphos", store = "ZZ"),
    edit   = list(city = "Nicosia", store = "B2"),
    edited_col = "store", edited_val = "B2"
  ),
  list(
    label  = "Moneygram Stores",
    server = mod_moneygram_stores_server, table = "moneygram_stores", key = "Agent ID",
    select = "tbl_moneygram_rows_selected",
    new    = list(agent_id = "2001", agent_name = "New agent", store_code = "ZZ"),
    # the (disabled) Agent ID box holds the existing id of row 2 in the real UI
    edit   = list(agent_id = "1002", agent_name = "Edited agent", store_code = "B2"),
    edited_col = "Acs store code", edited_val = "B2"
  )
)

row_by_key <- function(dta, key, value) {
  out <- dta[which(dta[[key]] == value), , drop = FALSE]
  rownames(out) <- NULL
  out
}

for (case in crud_cases) {

  test_that(paste0(case$label, ": Edit -> Cancel -> Add inserts a new row and leaves the edited row alone"), {

    conn <- local_fixture_db()
    before <- read_tbl(conn, case$table)

    shiny::testServer(case$server, args = list(conn = conn), {
      do.call(session$setInputs, setNames(list(1), case$select))
      session$setInputs(btn_edit = 1)   # opens the Edit modal on row 1
      # user presses Cancel: nothing reaches the server
      session$setInputs(btn_add = 1)    # opens the Add modal
      do.call(session$setInputs, case$new)
      session$setInputs(submit = 1)
    })

    after <- read_tbl(conn, case$table)
    expect_equal(nrow(after), nrow(before) + 1)

    key1 <- before[[case$key]][1]
    expect_equal(row_by_key(after, case$key, key1), row_by_key(before, case$key, key1))
  })

  test_that(paste0(case$label, ": a normal Edit updates only the selected row"), {

    conn <- local_fixture_db()
    before <- read_tbl(conn, case$table)
    key2 <- before[[case$key]][2]

    shiny::testServer(case$server, args = list(conn = conn), {
      do.call(session$setInputs, setNames(list(2), case$select))
      session$setInputs(btn_edit = 1)
      do.call(session$setInputs, case$edit)
      session$setInputs(submit = 1)
    })

    after <- read_tbl(conn, case$table)
    expect_equal(nrow(after), nrow(before))
    expect_equal(row_by_key(after, case$key, key2)[[case$edited_col]], case$edited_val)

    others <- before[[case$key]][-2]
    expect_equal(
      after[after[[case$key]] %in% others, , drop = FALSE],
      before[before[[case$key]] %in% others, , drop = FALSE],
      ignore_attr = TRUE
    )
  })

  test_that(paste0(case$label, ": a normal Add inserts a row"), {

    conn <- local_fixture_db()
    before <- read_tbl(conn, case$table)

    shiny::testServer(case$server, args = list(conn = conn), {
      session$setInputs(btn_add = 1)
      do.call(session$setInputs, case$new)
      session$setInputs(submit = 1)
    })

    expect_equal(nrow(read_tbl(conn, case$table)), nrow(before) + 1)
  })
}

test_that("Moneygram Stores rejects a duplicate Agent ID", {

  conn <- local_fixture_db()
  before <- read_tbl(conn, "moneygram_stores")

  shiny::testServer(mod_moneygram_stores_server, args = list(conn = conn), {
    session$setInputs(btn_add = 1)
    session$setInputs(agent_id = "1001", agent_name = "Dup", store_code = "NEW")
    session$setInputs(submit = 1)
  })

  expect_equal(read_tbl(conn, "moneygram_stores"), before)
})

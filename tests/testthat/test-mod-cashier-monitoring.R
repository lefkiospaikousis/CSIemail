# mod_cashier_monitoring re-reads its reference tables when the user acts
# (Re-check / Generate / Send), and reports rows that won't appear in any
# report. Uses the synthetic fixture DB; the recipients table name is mocked
# so the test doesn't depend on the machine's golem-config.yml.

fake_cashier <- function() {
  dta <- data.frame(store = c("AA", "ZZ"), courier = c("c1", "c2"))
  dta[[names_cashier_per_store[["total_cash"]]]] <- c(10, 20)
  dta[[names_cashier_per_store[["total_card"]]]] <- c(1, 2)
  dta
}

fake_moneygram <- function() {
  data.frame(store = c("AA", NA), agent_id = c("1001", "9999"),
             agent_name = c("Agent A", "Unknown"), total = c(5, 6))
}

test_that("Cashier Groups is re-read on Re-check and on Generate, not before", {

  conn <- local_fixture_db()
  local_mocked_bindings(cashier_emails_table = function() "city_emails2")

  shiny::testServer(mod_cashier_monitoring_server, args = list(dbase_csi = conn), {

    n0 <- nrow(store_groups())

    # an edit made in the Database tab (another module, same DB)
    DBI::dbExecute(conn, "INSERT INTO cashier_groups VALUES ('Paphos', 'DD', 'cg-4')")
    expect_equal(nrow(store_groups()), n0)

    session$setInputs(recheck_unmatched = 1)
    expect_equal(nrow(store_groups()), n0 + 1)

    statements$cashier_per_store <- fake_cashier()
    statements$moneygram <- fake_moneygram()

    DBI::dbExecute(conn, "INSERT INTO cashier_groups VALUES ('Larnaca', 'EE', 'cg-5')")
    session$setInputs(generate_reports = 1)   # the timer is never advanced: no files are written

    expect_equal(nrow(store_groups()), n0 + 2)
    expect_equal(progress$total_reports, 4)   # Nicosia, Limassol, Paphos, Larnaca
  })
})

test_that("recipients are re-read when Send refreshes them", {

  conn <- local_fixture_db()
  local_mocked_bindings(cashier_emails_table = function() "city_emails2")

  shiny::testServer(mod_cashier_monitoring_server, args = list(dbase_csi = conn), {

    e0 <- nrow(city_emails())
    DBI::dbExecute(conn, "INSERT INTO city_emails2 VALUES ('Paphos', 'paphos@example.com', 'ce-3')")
    expect_equal(nrow(city_emails()), e0)

    emails_refresh(emails_refresh() + 1)   # what the Send Emails button does
    expect_equal(nrow(city_emails()), e0 + 1)
  })
})

test_that("the recipients table comes from cashier_emails_table()", {

  conn <- local_fixture_db()
  DBI::dbExecute(conn, "DELETE FROM city_emails")   # make the two tables differ

  local_mocked_bindings(cashier_emails_table = function() "city_emails")
  shiny::testServer(mod_cashier_monitoring_server, args = list(dbase_csi = conn), {
    expect_equal(nrow(city_emails()), 0)
  })

  local_mocked_bindings(cashier_emails_table = function() "city_emails2")
  shiny::testServer(mod_cashier_monitoring_server, args = list(dbase_csi = conn), {
    expect_equal(nrow(city_emails()), 2)
  })
})

test_that("unmatched rows of the loaded statements are reported", {

  conn <- local_fixture_db()
  local_mocked_bindings(cashier_emails_table = function() "city_emails2")

  shiny::testServer(mod_cashier_monitoring_server, args = list(dbase_csi = conn), {

    expect_equal(nrow(unmatched()), 0)

    statements$cashier_per_store <- fake_cashier()
    statements$moneygram <- fake_moneygram()
    session$flushReact()

    um <- unmatched()
    expect_setequal(um$statement, c("Cashier per Store", "Moneygram"))
    expect_true("ZZ" %in% um$store)                      # cashier store not in any group
    expect_true(any(grepl("Agent ID 9999", um$detail)))  # unmapped Moneygram agent

    # fixing the group in the Database tab clears the cashier row after Re-check
    DBI::dbExecute(conn, "INSERT INTO cashier_groups VALUES ('Nicosia', 'ZZ', 'cg-9')")
    session$setInputs(recheck_unmatched = 1)
    expect_false("ZZ" %in% unmatched()$store)
  })
})

test_that("the three sample statements load through the UI inputs", {

  files <- c(
    cashier   = sample_data_path("_CASHIERS_CITY_USERS.xlsx"),
    moneygram = sample_data_path("_Activity Moneygram 24092025.xls"),
    viva      = sample_data_path("_VIVA_24-10-2025.csv")
  )
  skip_if_not(all(file.exists(files)), "SampleData/ (git-ignored) is not available")

  conn <- local_fixture_db()
  local_mocked_bindings(cashier_emails_table = function() "city_emails2")

  shiny::testServer(mod_cashier_monitoring_server, args = list(dbase_csi = conn), {

    session$setInputs(`load_cashier_per_store_1-file_statement` = file_input(files[["cashier"]]))
    session$setInputs(`load_moneygram_statement_1-file_statement` = file_input(files[["moneygram"]]))
    session$setInputs(`load_viva_per_store_1-file_statement` = file_input(files[["viva"]]))

    expect_false(is.null(statements$cashier_per_store))
    expect_false(is.null(statements$moneygram))
    expect_false(is.null(statements$viva_per_store))
    expect_s3_class(statements$statement_date, "Date")

    um <- unmatched()
    expect_named(um, c("statement", "store", "detail", "amount"))
    expect_true(is.numeric(um$amount))
  })
})

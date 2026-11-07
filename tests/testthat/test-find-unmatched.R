# find_unmatched() lists the rows of the loaded statements that the cashier
# report generation would drop: stores missing from cashier_groups and
# Moneygram agents without a store mapping.

groups <- data.frame(city = c("Nicosia", "Nicosia", "Limassol"), store = c("AA", "BB", "CC"))

cashier_stmt <- function(store, cash) {
  dta <- data.frame(store = store, courier = paste("courier", seq_along(store)))
  dta[[names_cashier_per_store[["total_cash"]]]] <- cash  # Greek header, as read from the file
  dta
}

test_that("nothing loaded gives an empty result with the expected columns", {

  out <- find_unmatched(groups = groups)
  expect_equal(nrow(out), 0)
  expect_named(out, c("statement", "store", "detail", "amount"))
})

test_that("everything matching gives an empty result", {

  out <- find_unmatched(
    cashier   = cashier_stmt(c("AA", "BB"), c(10, 20)),
    moneygram = data.frame(agent_id = "1001", agent_name = "A", total = 5, store = "AA"),
    viva      = data.frame(store = "CC", Amount = 7),
    groups    = groups
  )
  expect_equal(nrow(out), 0)
})

test_that("cashier stores missing from Cashier Groups are listed once, with their cash summed", {

  out <- find_unmatched(cashier = cashier_stmt(c("AA", "ZZ", "ZZ"), c(1, 2, 3)), groups = groups)
  expect_equal(nrow(out), 1)
  expect_equal(out$statement, "Cashier per Store")
  expect_equal(out$store, "ZZ")
  expect_equal(out$amount, 5)
})

test_that("unmapped Moneygram agents and Moneygram stores without a group are both listed", {

  mg <- data.frame(
    agent_id   = c("1001", "9999", "1003"),
    agent_name = c("Agent A", "Unknown agent", "Agent X"),
    total      = c(10, -4, 6),
    store      = c("AA", NA, "XX")
  )
  out <- find_unmatched(moneygram = mg, groups = groups)

  expect_equal(nrow(out), 2)
  unmapped <- out[is.na(out$store), ]
  expect_match(unmapped$detail, "Agent ID 9999")
  expect_equal(unmapped$amount, -4)
  expect_equal(out$store[!is.na(out$store)], "XX")
})

test_that("VIVA stores missing from Cashier Groups are listed with the amount summed", {

  out <- find_unmatched(viva = data.frame(store = c("NOPE", "NOPE", "AA"), Amount = c(1, 2, 3)), groups = groups)
  expect_equal(out$statement, "VIVA")
  expect_equal(out$store, "NOPE")
  expect_equal(out$amount, 3)
})

test_that("rows with no store at all in a statement are ignored, not reported as a store", {

  out <- find_unmatched(cashier = cashier_stmt(c(NA, "AA"), c(1, 2)), groups = groups)
  expect_equal(nrow(out), 0)
})

test_that("the modal warning is only shown when something is unmatched", {

  expect_null(unmatched_warning_ui(find_unmatched(groups = groups)))

  ui <- unmatched_warning_ui(find_unmatched(viva = data.frame(store = "NOPE", Amount = 1), groups = groups))
  html <- as.character(ui)
  expect_match(html, "1 row(s)", fixed = TRUE)
  expect_match(html, "NOPE")
})

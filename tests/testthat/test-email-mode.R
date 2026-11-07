# Where emails come from and go to is decided by the config profile
# (keys smtp_creds_source / cashier_emails_table), not by golem.app.prod.
# get_golem_config() is mocked so these tests don't depend on the
# machine's git-ignored inst/golem-config.yml.

mock_config <- function(...) {
  values <- list(...)
  function(value, ...) values[[value]]
}

test_that("active_config(): GOLEM_CONFIG_ACTIVE, then R_CONFIG_ACTIVE, then 'default'", {

  withr::local_envvar(GOLEM_CONFIG_ACTIVE = NA, R_CONFIG_ACTIVE = NA)
  expect_equal(active_config(), "default")

  withr::local_envvar(R_CONFIG_ACTIVE = "production")
  expect_equal(active_config(), "production")

  withr::local_envvar(GOLEM_CONFIG_ACTIVE = "dev")
  expect_equal(active_config(), "dev")
})

test_that("config keys decide the SMTP source and cashier table, whatever golem.app.prod says", {

  local_mocked_bindings(get_golem_config = mock_config(
    smtp_creds_source = "keyring", cashier_emails_table = "city_emails"
  ))
  withr::local_options(golem.app.prod = FALSE)

  expect_equal(smtp_creds_source(), "keyring")
  expect_equal(cashier_emails_table(), "city_emails")
  expect_false(app_mode_info()$test_recipients)

  local_mocked_bindings(get_golem_config = mock_config(
    smtp_creds_source = "file", cashier_emails_table = "city_emails2"
  ))
  withr::local_options(golem.app.prod = TRUE)

  expect_equal(smtp_creds_source(), "file")
  expect_equal(cashier_emails_table(), "city_emails2")
  expect_true(app_mode_info()$test_recipients)
})

test_that("missing keys (old config file) fall back to golem.app.prod and log a warning", {

  local_mocked_bindings(get_golem_config = mock_config())

  withr::local_options(golem.app.prod = TRUE)
  expect_message(src <- smtp_creds_source(), "missing")
  expect_equal(src, "keyring")
  expect_message(tbl <- cashier_emails_table(), "missing")
  expect_equal(tbl, "city_emails")

  withr::local_options(golem.app.prod = FALSE)
  expect_equal(suppressMessages(smtp_creds_source()), "file")
  expect_equal(suppressMessages(cashier_emails_table()), "city_emails2")
})

test_that("an invalid smtp_creds_source is an error", {

  local_mocked_bindings(get_golem_config = mock_config(smtp_creds_source = "ftp"))
  expect_error(smtp_creds_source(), "must be")
})

test_that("mail_credentials() reads a creds file or a keyring key depending on the config", {

  local_mocked_bindings(
    creds_file = function(file) list(kind = "file", id = file),
    creds_key  = function(id) list(kind = "keyring", id = id),
    .package = "blastula"
  )

  local_mocked_bindings(get_golem_config = mock_config(smtp_creds = "my_creds", smtp_creds_source = "file"))
  expect_equal(mail_credentials(), list(kind = "file", id = "my_creds"))

  local_mocked_bindings(get_golem_config = mock_config(smtp_creds = "acs", smtp_creds_source = "keyring"))
  expect_equal(mail_credentials(), list(kind = "keyring", id = "acs"))
})

test_that("the sidebar badge flags the test recipient list", {

  local_mocked_bindings(get_golem_config = mock_config(
    smtp_creds_source = "file", cashier_emails_table = "city_emails2"
  ))
  expect_match(as.character(mode_badge()), "(TEST)", fixed = TRUE)

  local_mocked_bindings(get_golem_config = mock_config(
    smtp_creds_source = "keyring", cashier_emails_table = "city_emails"
  ))
  expect_no_match(as.character(mode_badge()), "(TEST)", fixed = TRUE)
})

test_that("the config template defines the email-mode keys for every profile and holds no real secrets", {

  tpl <- app_sys("golem-config.example.yml")
  expect_true(file.exists(tpl))

  for (profile in c("default", "dev", "production")) {
    cfg <- config::get(file = tpl, config = profile)
    expect_true(cfg$smtp_creds_source %in% c("file", "keyring"), info = profile)
    expect_true(cfg$cashier_emails_table %in% c("city_emails", "city_emails2"), info = profile)
    expect_match(cfg$users_passphrase, "^<.*>$", info = profile)
  }

  prod <- config::get(file = tpl, config = "production")
  expect_equal(prod$smtp_creds_source, "keyring")
  expect_equal(prod$cashier_emails_table, "city_emails")
})

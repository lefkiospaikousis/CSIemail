# shinymanager renders its login page without calling app_ui(), so the "www"
# resource path (logo on the login page) must be registered by run_app()
# itself, before anyone has logged in.

test_that("run_app() serves www/ before any login, so the login logo loads", {

  if ("www" %in% names(shiny::resourcePaths())) {
    shiny::removeResourcePath("www")
  }
  withr::defer(if ("www" %in% names(shiny::resourcePaths())) shiny::removeResourcePath("www"))

  app <- run_app()

  expect_s3_class(app, "shiny.appobj")
  expect_true("www" %in% names(shiny::resourcePaths()))
  expect_true(file.exists(file.path(shiny::resourcePaths()[["www"]], "logo.png")))
})

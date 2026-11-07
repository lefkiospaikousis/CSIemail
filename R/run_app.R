#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts. 
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options 
run_app <- function(
  onStart = NULL,
  options = list(), 
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {

  # Serve inst/app/www under "www/" from the start. golem normally registers
  # it inside app_ui(), but shinymanager only calls app_ui() AFTER a
  # successful login - so without this the login page's logo (www/logo.png)
  # is a 404 until someone has logged in once in this R process.
  golem::add_resource_path("www", app_sys("app/www"))

  with_golem_options(
    app = shinyApp(
      #ui = app_ui,
      ui = shinymanager::secure_app(app_ui,
                                    enable_admin = TRUE,
                                    tags_top = tags$div(
        tags$h4("Cash Sales Invoice - Emails", style = "align:center"),
        tags$img(
          src =  "www/logo.png"  #"https://www.r-project.org/logo/Rlogo.png"
          , width = 100
        )
      )),
      server = app_server,
      onStart = onStart,
      options = options, 
      enableBookmarking = enableBookmarking, 
      uiPattern = uiPattern
    ), 
    golem_opts = list(...)
  )
}

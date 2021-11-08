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
  with_golem_options(
    app = shinyApp(
      ui = shinymanager::secure_app(app_ui, tags_top = tags$div(
        tags$h4("Cash Sales Invoice - Emails", style = "align:center"),
        tags$img(
          src =  "www/ACS_logo1.jpg"  #"https://www.r-project.org/logo/Rlogo.png"
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

#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    dashboardPage(
      dashboardHeader(title = "ACS CSI"),
      dashboardSidebar(
        sidebarMenu(id = "tabs",
                    #menuItem("Reports", tabName = "reports", icon = icon("chart-bar")),
                    #menuItem("Automatic", tabName = "rec_automatic", icon = icon("magic")),
                    menuItem("Send emails", tabName = "email", icon = icon("envelope")),
                    menuItem("Load files", tabName = "load", icon = icon("file-upload")),
                    tags$hr(style = "border-color: white; width:50%"),
                    p(paste0("Version: ", golem::get_golem_version()), style = "margin-left:25px")
                    
        )
      ),
      dashboardBody(
        tabItems(
          
          tabItem(tabName = "email",
                  h3("Send emails"),
                  mod_tab_email_ui("tab_email_ui_1")
                  
          ),
          tabItem(tabName = "load",
                  mod_tab_load_ui("tab_load_ui_1")
          )
        )
      )
      
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'CSIemail'
    ),
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    shinyFeedback::useShinyFeedback()
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}


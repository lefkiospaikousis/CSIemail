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
    # get the IP https://github.com/rstudio/shiny/issues/141
    # Try this on shinyaps.io to make sure. Locally it works
    div(style = "display: none;",
        textInput("remote_addr", "remote_addr",
                  if (!is.null(request[["HTTP_X_FORWARDED_FOR"]]))
                    request[["HTTP_X_FORWARDED_FOR"]]
                  else
                    request[["REMOTE_ADDR"]]
        )
    ),
    # Your application UI logic 
    dashboardPage(
      dashboardHeader(title = "ACS CSI"),
      dashboardSidebar(
        sidebarMenu(id = "tabs",
                    menuItem("1. Load file", tabName = "load", icon = icon("file-upload")),
                    menuItem("2. Send emails", tabName = "email", icon = icon("envelope")),
                    hr(style = "width:80%"),
                    menuItem("Store Email List", tabName = "dbase", icon = icon("database")),
                    menuItem("Cashier Monitoring", tabName = "monitoring", icon = icon("database")),
                    menuItemOutput("managerUI"),
                    tags$hr(style = "border-color: white; width:80%"),
                    p(paste0("Version: ", golem::get_golem_version()), style = "margin-left:25px"),
                    div(
                      textOutput("userName"), style = "margin-left:25px"
                      )
                    #, textOutput("userName")
                    #, verbatimTextOutput("ip_user")
                    
                    
                    
        )
      ),
      dashboardBody(
        tabItems(
          
          tabItem(tabName = "load",
                  mod_tab_load_ui("tab_load_ui_1")
          ),
          tabItem(tabName = "email",
                  #h3("Select stores to send emails to"),
                  mod_tab_send_email_ui("tab_email_ui_1")
                  
          ),
          tabItem(tabName = "monitoring",
                  mod_cashier_monitoring_ui("cashier_monitoring_1")
                  
          ),
          tabItem(tabName = "dbase",
                  h3("The store email list"),
                  mod_tab_dbase_ui("tab_dbase_ui_1")
                  
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


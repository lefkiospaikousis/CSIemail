#' user_management UI Function
#' 
#' Haven't used it. I use the {shinymanager} with a protected dbase for users
#' It is not a finished module yest, so don;t use it as is 
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_user_management_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Manage Users"),
    tags$hr(style="border-color: black;"),
    fluidRow(
      
      box(width = 4,
          h4("Add User"),
          textInput(ns("username"), "Username"),
          textInput(ns("password"), "Password"),
          actionButton(ns("submit"), "Done")
      )
      
    ),
    fluidRow(
      col_8(
        box(width = NULL,
            div(id = "tbl_buttons",
                actionButton(ns("btn_add"), "Add", icon("plus"), 
                             style="color: #fff; background-color: #86af49"),
                actionButton(ns("btn_edit"), "Edit", icon("edit"), 
                             style="color: #fff; background-color: #337ab7"),
                actionButton(ns("btn_delete"), "Delete", icon("trash-alt"), 
                             class = "btn-danger",
                             style="color: #fff"),
                
                style = "margin-bottom:8px" 
            ),
            br(),
            DTOutput(ns("tbl_users"))
        )
      )
    )
    
  )
}

#' user_management Server Functions
#'
#' @noRd 
mod_user_management_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    tbl_users_proxy <- DT::dataTableProxy("tbl_users")
    
    rv <- rv(
      db_trigger = 0,
      user_to_edit = NULL
    )
    
    tbl_users <- reactive({
      
      rv$db_trigger
      
      conn %>% 
        tbl("users") %>% 
        collect()
    })
    
    
    
    output$tbl_users <- renderDT({
      
      tbl_users() %>%
        #select(-uid) %>% 
        datatable(
          options = list()
          , rownames = FALSE
          , colnames = c("User Name" = "user_name", "Password" = "password")
          , selection = "single"
          , caption = "The ACS users and associated password(s)"
          , filter = "top"
        )
    })
    
    
    ids_selected <- reactive(input$tbl_users_rows_selected)
    
    
    observeEvent(input$btn_add, {
      
      showModal(entry_form(session))
      
    })
    
    observeEvent(input$btn_edit, {
      
      if(is.null(ids_selected())) {
        
        showToast("warning", "Please select a user on the table",
                  .options = list(positionClass = "toast-top-center")
        )
        
      } else {
        
        # Edit the user
        user <- as.list(tbl_users()[ids_selected(), ])
        
        showModal(
          entry_form(session, edit = TRUE, user = user)
        )
        
        updateTextInput(session, "user_name", value = user$user_name)
        updateTextInput(session, "password", value = user$password)
        
        rv$user_to_edit = user
      }
      
      
    })
    
    observeEvent(input$submit, {
      
      tryCatch(
        
        expr = {
          
          if(is.null(rv$user_to_edit)) {
            
            # new user/ password
            append_data(conn, "users", form_data())
            
            showToast("success",
                      paste0("user: ",  form_data()$user_name, " was added in the database"),
                      .options = list(positionClass = "toast-top-center")
            )
            
          } else {
            
            # edit user
            DBI::dbExecute(
              conn,
              "UPDATE users SET user_name=$user_name, password=$password WHERE uid=$uid",
              params = as.list(form_data())
              
            )
            
            showToast("success",
                      glue("user: {form_data()$user_name} was edited in the database"),
                      .options = list(positionClass = "toast-top-center")
            )
            
            rv$user_to_edit = NULL
            
          }
          
          removeModal()
          
          rv$db_trigger <- isolate(rv$db_trigger) + 1
          
        },
        
        error = function(e) {
          
          print(e)
          showModal(modalDialog(title = "Unable to add/ edit user to the database",
                                p("Cannot add/ edit this to the database. Something is wrong"),
                                p("Maybe the connection to the database is lost. Refresh the webage and try again")
          )
          )
        } 
      )
      
    })
    
    form_data <- reactive({
      
      user <- isolate(rv$user_to_edit)
      
      data.frame(
        uid = if(!is.null(user)) {user$uid} else {uuid::UUIDgenerate()},
        user_name = input$user_name,
        password      = input$password, 
        stringsAsFactors = FALSE)
      
    })
    
    
    
    observeEvent(input$btn_delete, {
      
      if(is.null(ids_selected())) {
        
        showToast("warning", "Please select a row on the table",
                  .options = list(positionClass = "toast-top-center")
        )
        
      } else {
        
        user <- tbl_users()[ids_selected(), ]
        
        showModal(verify_delete(session, user))
        
      }
      
    })
    
    observeEvent(input$submit_delete, {
      
      removeModal()
      
      uids <- tbl_users()[ids_selected(), ]$uid
      user_name <- tbl_users()[ids_selected(), ]$user_name
      
      tryCatch(
        
        expr = {
          delete_data(conn, "users", "uid", uids)
          
          rv$db_trigger <- isolate(rv$db_trigger) + 1
          
          showToast("success",
                    paste0("user: ",  user_name, " was deleted from the Database"),
                    .options = list(positionClass = "toast-top-center")
          )
        },
        
        error = function(e) {
          
          msg <- "Error Deleting user"
          print(msg)
          print(error)
          showToast("error", msg)
          
        }
      )
      
      
    })
    
    
    
    return(rv)
    
    
  })
}

## To be copied in the UI
# mod_user_management_ui("user_management_ui_1")

## To be copied in the server
# mod_user_management_server("user_management_ui_1")

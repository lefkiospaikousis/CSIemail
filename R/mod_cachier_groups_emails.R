#' cachier_groups_emails UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cachier_groups_emails_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      col_12(
        #box(width = NULL,
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
            DTOutput(ns("city_emails"))
        #)
      )
    )
  )
}
    
#' cachier_groups_emails Server Functions
#'
#' @noRd 
mod_cachier_groups_emails_server <- function(id, conn){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    #city_emails_proxy <- DT::dataTableProxy("city_emails")
    
    rv <- rv(
      db_trigger = 0,
      city_to_edit = NULL
    )
    
    email_regex <- "^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}$"
    email_regex <- "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$"
    
    cashier_groups <- reactive({
      
      rv$db_trigger
      
      conn %>% 
        tbl(db_tables[['cashier_groups']]) %>% 
        collect()
    })
    
    
    city_emails <- reactive({
      
      rv$db_trigger
      
      conn %>% 
        tbl(db_tables[['city_emails']]) %>% 
        collect()
    })
    
    
    output$city_emails <- renderDT({
      
      city_emails() |> 
        select(-uuid) |> 
        datatable(
          options = list()
          , rownames = FALSE
          , colnames = c("City" = "city", "Email" = "email")
          , selection = "single"
          , caption = "The ACS Head stores and associated email(s)"
          , filter = "top"
        )
    })
    
    
    ids_selected <- reactive(input$city_emails_rows_selected)
    
    
    observeEvent(input$btn_add, {
      
      showModal(entry_form_city(session))
      
    })
    
    observeEvent(input$btn_edit, {
      
      if(is.null(ids_selected())) {
        
        showToast("warning", "Please select a row on the table",
                  .options = list(positionClass = "toast-top-center")
        )
        
      } else {
        
        # Edit the city
        city <- as.list(city_emails()[ids_selected(), ])
        
        showModal(
          entry_form_city(session, edit = TRUE, city = city)
        )
        
        updateTextInput(session, "city", value = city$city)
        updateTextInput(session, "email", value = city$email)
        
        rv$city_to_edit = city
      }
      
      
    })
    
    observeEvent(input$submit, {
      
      tryCatch(
        
        expr = {
          
          if(is.null(rv$city_to_edit)) {
            
            # new city/ email
            append_data(conn, db_tables[["city_emails"]], form_data())
            
            showToast("success",
                      paste0("City: ",  form_data()$city, " was added in the database"),
                      .options = list(positionClass = "toast-top-center")
            )
            
          } else {
            
            # edit city
            DBI::dbExecute(
              conn,
              "UPDATE city_emails SET city=$city, email=$email WHERE uuid=$uuid",
              params = as.list(form_data())
            )
            
            showToast("success",
                      glue("City: {form_data()$city} was edited in the database"),
                      .options = list(positionClass = "toast-top-center")
            )
            
            #Nullyfy the edit object in case the next action is an add
            rv$city_to_edit = NULL
            
          }
          
          removeModal()
          
          rv$db_trigger <- isolate(rv$db_trigger) + 1
          
        },
        
        error = function(e) {
          
          print(e)
          showModal(modalDialog(title = "Unable to add/ edit city to the database",
                                p("Cannot add/ edit this to the database. Something is wrong"),
                                p("Maybe the connection to the database is lost. Refresh the webage and try again")
          )
          )
        } 
      )
      
    })
    
    observeEvent(input$email, {
      
      if(!stringr::str_detect(input$email, email_regex)) {
        shinyFeedback::showFeedbackDanger(input$email, text = "Invalid email address")
      }
      
      
    })
    
    form_data <- reactive({
      
      city <- isolate(rv$city_to_edit)
      
      data.frame(
        uuid   = if(!is.null(city)) {city$uuid} else {uuid::UUIDgenerate()},
        city  = input$city,
        email = input$email, 
        stringsAsFactors = FALSE)
      
    })
    
    
    
    observeEvent(input$btn_delete, {
      
      if(is.null(ids_selected())) {
        
        showToast("warning", "Please select a row on the table",
                  .options = list(positionClass = "toast-top-center")
        )
        
      } else {
        
        city <- city_emails()[ids_selected(), ]
        
        showModal(verify_delete_city(session, city))
        
      }
      
    })
    
    observeEvent(input$submit_delete, {
      
      removeModal()
      
      uids <- city_emails()[ids_selected(), ]$uuid
      city <- city_emails()[ids_selected(), ]$city
      
      tryCatch(
        
        expr = {
          delete_data(conn, db_tables[["city_emails"]], "uuid", uids)
          
          rv$db_trigger <- isolate(rv$db_trigger) + 1
          
          showToast("success",
                    paste0("City: ",  city, " was deleted from the Database"),
                    .options = list(positionClass = "toast-top-center")
          )
        },
        
        error = function(e) {
          
          msg <- "Error Deleting City"
          print(msg)
          print(e)
          showToast("error", msg)
          
        }
      )
      
      
    })
    
    return(rv)
    
    
  })
}
    
## To be copied in the UI
# mod_cachier_groups_emails_ui("cachier_groups_emails_1")
    
## To be copied in the server
# mod_cachier_groups_emails_server("cachier_groups_emails_1")

#' cashier_groups UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cashier_groups_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_12(
        buttons_edit(ns),
        DTOutput(ns("cashier_groups"))
      )
    )
  )
}
    
#' cashier_groups Server Functions
#'
#' @noRd 
mod_cashier_groups_server <- function(id, conn){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    rv <- rv(
      db_trigger = 0,
      city_to_edit = NULL
    )
    
    cashier_groups <- reactive({
      
      rv$db_trigger
      
      conn %>% 
        tbl(db_tables[['cashier_groups']]) %>% 
        collect()
    })
    
    
    output$cashier_groups <- renderDT({
      
      cashier_groups() |> 
        select(-uuid) |> 
        datatable(
          options = list()
          , rownames = FALSE
          , colnames = c("City" = "city", "Store" = "store")
          , selection = "single"
          , caption = "The ACS cashier groups"
          , filter = "top"
        )
    })
    
    
    ids_selected <- reactive(input$cashier_groups_rows_selected)
    
    
    observeEvent(input$btn_add, {
      
      showModal(entry_form_cashier_group(session))
      
    })
    
    observeEvent(input$btn_edit, {
      
      if(is.null(ids_selected())) {
        
        showToast("warning", "Please select a row on the table",
                  .options = list(positionClass = "toast-top-center")
        )
        
      } else {
        
        # Edit the city
        city <- as.list(cashier_groups()[ids_selected(), ])
        
        showModal(
          entry_form_cashier_group(session, edit = TRUE, city = city)
        )
        
        updateTextInput(session, "city", value = city$city)
        updateTextInput(session, "store", value = city$store)
        
        rv$city_to_edit = city
      }
      
      
    })
    
    observeEvent(input$submit, {
      
      tryCatch(
        
        expr = {
          
          if(is.null(rv$city_to_edit)) {
            
            # new city/ store
            append_data(conn, db_tables[["cashier_groups"]], form_data())
            
            showToast("success",
                      paste0("City: ",  form_data()$city, " was added in the database"),
                      .options = list(positionClass = "toast-top-center")
            )
            
          } else {
            
            # edit city
            DBI::dbExecute(
              conn,
              "UPDATE cashier_groups SET city=$city, store=$store WHERE uuid=$uuid",
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
          showModal( modal_error_editing_dbase('city') )
          
          return(NULL)
        } 
      )
      
    })
    
    form_data <- reactive({
      
      city <- isolate(rv$city_to_edit)
      
      data.frame(
        uuid   = if(!is.null(city)) {city$uuid} else {uuid::UUIDgenerate()},
        city  = input$city,
        store = input$store, 
        stringsAsFactors = FALSE)
      
    })
    
    
    
    observeEvent(input$btn_delete, {
      
      if(is.null(ids_selected())) {
        
        showToast("warning", "Please select a row on the table",
                  .options = list(positionClass = "toast-top-center")
        )
        
      } else {
        
        city <- cashier_groups()[ids_selected(), ]
        
        showModal(verify_delete_store(session, city))
        
      }
      
    })
    
    observeEvent(input$submit_delete, {
      
      removeModal()
      
      uids <- cashier_groups()[ids_selected(), ]$uuid
      city <- cashier_groups()[ids_selected(), ]$city
      store <- cashier_groups()[ids_selected(), ]$store
      
      tryCatch(
        
        expr = {
          delete_data(conn, db_tables[["cashier_groups"]], "uuid", uids)
          
          rv$db_trigger <- isolate(rv$db_trigger) + 1
          
          showToast("success",
                    glue::glue("Store: {store} was delete from City: {city}" ),
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
# mod_cashier_groups_ui("cashier_groups_1")
    
## To be copied in the server
# mod_cashier_groups_server("cashier_groups_1")

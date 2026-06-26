#' moneygram_stores UI Function
#'
#' @description A shiny Module to maintain the moneygram_stores mapping
#'   (Agent ID -> Acs store code). Full add/ edit/ delete with duplicate
#'   protection on the Agent ID (key) and the Acs store code.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_moneygram_stores_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_12(
        buttons_edit(ns),
        DTOutput(ns("tbl_moneygram"))
      )
    )
  )
}

#' moneygram_stores Server Functions
#'
#' @param conn The connection to the RSQLite
#' @noRd
mod_moneygram_stores_server <- function(id, conn){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    rv <- rv(
      db_trigger = 0,
      store_to_edit = NULL
    )

    moneygram <- reactive({

      rv$db_trigger

      conn %>%
        tbl("moneygram_stores") %>%
        collect()
    })


    output$tbl_moneygram <- renderDT({

      moneygram() |>
        datatable(
          options = list()
          , rownames = FALSE
          , selection = "single"
          , caption = "The moneygram agent to ACS store mapping"
          , filter = "top"
        )
    })


    ids_selected <- reactive(input$tbl_moneygram_rows_selected)


    observeEvent(input$btn_add, {

      showModal(entry_form_moneygram(session))

    })

    observeEvent(input$btn_edit, {

      if(is.null(ids_selected())) {

        showToast("warning", "Please select a row on the table",
                  .options = list(positionClass = "toast-top-center")
        )

      } else {

        # Edit the moneygram store
        store <- as.list(moneygram()[ids_selected(), ])

        showModal(
          entry_form_moneygram(session, edit = TRUE, store = store)
        )

        updateTextInput(session, "agent_id", value = store$`Agent ID`)
        updateTextInput(session, "agent_name", value = store$`Agent name`)
        updateTextInput(session, "store_code", value = store$`Acs store code`)

        rv$store_to_edit = store
      }

    })

    observeEvent(input$submit, {

      tryCatch(

        expr = {

          store_to_edit <- isolate(rv$store_to_edit)
          is_edit <- !is.null(store_to_edit)

          agent_id  <- suppressWarnings(as.numeric(stringr::str_trim(input$agent_id)))
          store_code <- stringr::str_trim(input$store_code)

          # ---- Validation -------------------------------------------------
          # Leave the modal open (return early) so the user can fix the entry

          if(is.na(agent_id)) {
            showToast("error", "Agent ID is required and must be a number",
                      .options = list(positionClass = "toast-top-center"))
            return(NULL)
          }

          if(store_code == "") {
            showToast("error", "Acs store code is required",
                      .options = list(positionClass = "toast-top-center"))
            return(NULL)
          }

          current <- moneygram()

          # Duplicate Agent ID - only relevant when adding (key is locked on edit)
          if(!is_edit && agent_id %in% current$`Agent ID`) {
            showToast("error",
                      glue("Agent ID {agent_id} already exists in the table"),
                      .options = list(positionClass = "toast-top-center"))
            return(NULL)
          }

          # Duplicate Acs store code - check against all OTHER rows
          other_rows <- current
          if(is_edit) {
            other_rows <- current[current$`Agent ID` != store_to_edit$`Agent ID`, ]
          }

          dup_store <- tolower(store_code) %in% tolower(stringr::str_trim(other_rows$`Acs store code`))

          if(dup_store) {
            showToast("error",
                      glue("Acs store code '{store_code}' is already mapped to another agent"),
                      .options = list(positionClass = "toast-top-center"))
            return(NULL)
          }

          # ---- Persist ----------------------------------------------------

          if(!is_edit) {

            # new moneygram store
            append_data(conn, "moneygram_stores", form_data())

            showToast("success",
                      glue("Agent ID {agent_id} was added in the database"),
                      .options = list(positionClass = "toast-top-center")
            )

          } else {

            # edit moneygram store (Agent ID is the key and stays unchanged)
            DBI::dbExecute(
              conn,
              "UPDATE moneygram_stores SET `Agent name`=$name, `Acs store code`=$store WHERE `Agent ID`=$id",
              params = list(
                name  = stringr::str_trim(input$agent_name),
                store = store_code,
                id    = store_to_edit$`Agent ID`
              )
            )

            showToast("success",
                      glue("Agent ID {store_to_edit$`Agent ID`} was edited in the database"),
                      .options = list(positionClass = "toast-top-center")
            )

            rv$store_to_edit = NULL

          }

          removeModal()

          rv$db_trigger <- isolate(rv$db_trigger) + 1

        },

        error = function(e) {

          print(e)
          modal_error_editing_dbase('moneygram store')

          return(NULL)

        }
      )

    })

    form_data <- reactive({

      data.frame(
        `Agent ID`       = as.numeric(stringr::str_trim(input$agent_id)),
        `Agent name`     = stringr::str_trim(input$agent_name),
        `Acs store code` = stringr::str_trim(input$store_code),
        check.names = FALSE,
        stringsAsFactors = FALSE)

    })


    observeEvent(input$btn_delete, {

      if(is.null(ids_selected())) {

        showToast("warning", "Please select a row on the table",
                  .options = list(positionClass = "toast-top-center")
        )

      } else {

        store <- moneygram()[ids_selected(), ]

        showModal(verify_delete_moneygram(session, store))

      }

    })

    observeEvent(input$submit_delete, {

      removeModal()

      agent_id   <- moneygram()[ids_selected(), ]$`Agent ID`
      agent_name <- moneygram()[ids_selected(), ]$`Agent name`

      tryCatch(

        expr = {
          delete_data(conn, "moneygram_stores", "`Agent ID`", agent_id)

          rv$db_trigger <- isolate(rv$db_trigger) + 1

          showToast("success",
                    glue("Moneygram store {agent_name} (Agent ID {agent_id}) was deleted from the Database"),
                    .options = list(positionClass = "toast-top-center")
          )
        },

        error = function(e) {

          msg <- "Error Deleting moneygram store"
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
# mod_moneygram_stores_ui("moneygram_stores_1")

## To be copied in the server
# mod_moneygram_stores_server("moneygram_stores_1")

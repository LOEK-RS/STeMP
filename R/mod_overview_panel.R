#' Overview Panel UI Module
#'
#' Creates UI for the Overview section of the protocol, rendering inputs dynamically
#' based on the protocol data. Also includes an objective radio button selector.
#'
#' @param id Module namespace ID
#' @return UI output container for overview inputs and objective selector
#' @noRd
mod_overview_panel_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Overview dynamic UI (created from protocol CSV)
    shiny::uiOutput(ns("overview_ui")),
    # Objective selector
    shiny::radioButtons(
      ns("o_objective_1"),
      "Objective:",
      choices = c("Model and prediction", "Model only"),
      selected = "Model and prediction"
    )
  )
}


#' Overview Panel Server Module
#'
#' Manages server-side logic for the Overview panel inputs.
#' Renders inputs dynamically from protocol data filtered for the Overview section,
#' supports overriding defaults with uploaded values,
#' and provides reactive values of inputs and the objective selector.
#'
#' @param id Module namespace ID
#' @param protocol_data Reactive data frame containing protocol information
#' @param uploaded_values Reactive data frame (optional) with uploaded element_id/value pairs to override defaults
#' @param hide_optional Reactive boolean to control whether optional inputs should be hidden
#'
#' @return A list of reactive expressions:
#' \itemize{
#'   \item{o_objective_1}{Reactive returning the selected objective option}
#'   \item{overview_inputs}{Data frame of current input values for overview section elements}
#' }
#' @noRd
mod_overview_panel_server <- function(id, protocol_data, uploaded_values = shiny::reactive(NULL),
                                      hide_optional = shiny::reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive filtered Overview data - keep all rows
    overview_data <- shiny::reactive({
      df <- protocol_data()
      shiny::req(df)
      df[df$section == "Overview", ]
    })

    # Render UI inputs dynamically (run once, independent of hide_optional)
    output$overview_ui <- shiny::renderUI({
      df <- overview_data()
      shiny::req(nrow(df) > 0)
      uploaded_df <- uploaded_values()

      ui_list <- lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]
        if (!is.null(uploaded_df) && all(c("element_id","value") %in% names(uploaded_df))) {
          uploaded_val <- uploaded_df$value[uploaded_df$element_id == row$element_id]
          if (length(uploaded_val) == 1 && !is.null(uploaded_val) && nzchar(uploaded_val)) {
            row$value <- uploaded_val
          }
        }

        # Always assign optional_field class if optional
        div_class <- if (!is.null(row$optional) && as.integer(row$optional) == 1) "optional_field" else NULL

        shiny::tags$div(
          class = div_class,
          render_input_field(
            element_type = row$element_type,
            element_id = ns(row$element_id),
            label = row$element,
            o_objective_1 = input$o_objective_1,
            suggestions = row$suggestions,
            info_text = row$info_text,
            ns = ns,
            row = row
          )
        )
      })

      do.call(shiny::tagList, ui_list)
    })

    # Toggle CSS class on body to hide optional fields (does not touch UI)
    shiny::observe({
      if (isTRUE(hide_optional())) {
        shinyjs::addClass(selector = "body", class = "hide_optional")
      } else {
        shinyjs::removeClass(selector = "body", class = "hide_optional")
      }
    })


    # Reactive collection of all Overview input values
    inputs_reactive <- shiny::reactive({
      df <- overview_data()
      vals <- lapply(df$element_id, function(id) {
        val <- input[[id]]
        if (is.null(val) || (is.character(val) && all(val == ""))) {
          NA
        } else if (length(val) > 1) {
          paste(val, collapse = ", ")
        } else {
          val
        }
      })

      data.frame(
        section = df$section,
        subsection = df$subsection,
        element = df$element,
        value = unlist(vals, use.names = FALSE),
        stringsAsFactors = FALSE
      )
    })

    # Return reactive outputs
    list(
      o_objective_1 = shiny::reactive(input$o_objective_1),
      overview_inputs = inputs_reactive
    )
  })
}

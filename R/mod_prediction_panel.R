#' Prediction Panel UI Module
#'
#' Creates UI for the Prediction section of the protocol, rendering inputs grouped
#' by subsections inside collapsible panels. Only shown if the objective is
#' "Model and prediction".
#'
#' @param id Module namespace ID
#' @return UI output container for prediction inputs inside collapsible panels
#' @noRd
mod_prediction_panel_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::uiOutput(ns("prediction_collapse_ui"))
  )
}

#' Prediction Panel Server Module
#'
#' Manages server-side logic for the Prediction panel inputs.
#' Filters protocol data for Prediction section, supports optional uploaded values
#' to override defaults, and renders geo-spatial plots server-side where appropriate.
#' Only active if the selected objective is "Model and prediction".
#'
#' @param id Module namespace ID
#' @param o_objective_1_val Reactive returning selected objective ("Model and prediction" or "Model only")
#' @param protocol_data Reactive data frame containing protocol information
#' @param geo_metadata Spatial metadata reactive list (optional)
#' @param uploaded_values Reactive data frame (optional) with uploaded element_id/value pairs to override defaults
#' @param output_dir temporary output directory
#'
#' @return A list containing:
#' \itemize{
#'   \item{prediction_inputs}{Reactive data.frame with current input values for prediction section elements}
#'   \item{uncertainty_quantification}{Selected uncertainty quantification approach}
#' }
#' @noRd
mod_prediction_panel_server <- function(id, o_objective_1_val, protocol_data, geo_metadata = NULL,
                                        uploaded_values = shiny::reactive(NULL),
                                        output_dir = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Validate spatial metadata for samples and prediction area
    valid_geo_samples_metadata <- validate_geo_metadata(geo_metadata, "has_samples")
    valid_geo_prediction_area_metadata <- validate_geo_metadata(geo_metadata, "has_prediction_area")
    valid_geo_all_metadata <- validate_geo_metadata(geo_metadata, c("has_samples", "has_prediction_area"))

    # Reactive filtered protocol data for Prediction section
    prediction_data <- shiny::reactive({
      shiny::req(protocol_data())
      df <- protocol_data()
      df[df$section == "Prediction", ]
    })

    # Unique subsections within Prediction data
    subsections <- shiny::reactive({
      unique(prediction_data()$subsection)
    })

    # Server-side rendering for geodist_plot_prediction plot, active only if objective is "Model and prediction"
    shiny::observe({
      shiny::req(o_objective_1_val() == "Model and prediction")
      df <- prediction_data()
      meta_geo_all_list <- valid_geo_all_metadata()
      if (is.null(meta_geo_all_list)) return()

      pid <- df$element_id[df$element_type == "geodist_plot_prediction"]
      if (length(pid) == 1) {

        render_geodist_plot_server(
          output = output,
          element_id = ns(pid),
          geo_metadata = meta_geo_all_list,
          objective = "Model and prediction",
          output_dir = output_dir
        )
      }
    })

    # Render UI collapsible panels for each subsection in Prediction data
    output$prediction_collapse_ui <- shiny::renderUI({
      shiny::req(o_objective_1_val() == "Model and prediction")

      df <- prediction_data()
      subs <- subsections()

      if (nrow(df) == 0) {
        return(shiny::tags$p("No prediction data available"))
      }

      meta_geo_prediction_area_list <- valid_geo_prediction_area_metadata()
      if (is.null(meta_geo_prediction_area_list)) meta_geo_prediction_area_list <- list()

      uploaded_df <- uploaded_values()

      panels <- lapply(subs, function(subsec) {
        sub_df <- df[df$subsection == subsec, ]

        inputs <- lapply(seq_len(nrow(sub_df)), function(i) {
          row <- sub_df[i, ]

          # Override default value with uploaded value if present
          if (!is.null(uploaded_df)) {
            uploaded_val <- uploaded_df$value[uploaded_df$element_id == row$element_id]
            if (length(uploaded_val) == 1 && !is.null(uploaded_val) && nzchar(uploaded_val)) {
              row$value <- uploaded_val
            }
          }

          # Render specific plots or inputs based on element_type
          if (row$element_type == "prediction_area_plot") {
            if (!is.null(valid_geo_prediction_area_metadata())) {
              render_prediction_area_plot(
                element_id = ns(row$element_id),
                element = row$element,
                geo_metadata = meta_geo_prediction_area_list,
                ns = ns
              )
            } else {
              NULL
            }
          } else if (row$element_type == "geodist_plot_prediction") {
            if (o_objective_1_val() == "Model and prediction" && !is.null(valid_geo_all_metadata())) {
              render_geodist_plot(
                element_id = ns(row$element_id),
                element = row$element,
                ns = ns
              )
            } else {
              NULL
            }
          } else {
            render_input_field(
              element_type = row$element_type,
              element_id = ns(row$element_id),
              label = row$element,
              o_objective_1 = o_objective_1_val(),
              suggestions = row$suggestions,
              info_text = row$info_text,
              geo_metadata = geo_metadata,
              ns = ns,
              row = row
            )
          }
        })

        shinyBS::bsCollapsePanel(title = subsec, do.call(shiny::tagList, inputs), style = "primary")
      })

      do.call(shinyBS::bsCollapse, panels)
    })

    # Server-side rendering for prediction_area_plot (if any), reactive on objective == "Model and prediction"
    shiny::observe({
      shiny::req(o_objective_1_val() == "Model and prediction")
      df <- prediction_data()
      meta_geo_prediction_area_list <- valid_geo_prediction_area_metadata()
      if (is.null(meta_geo_prediction_area_list)) return()

      pid <- df$element_id[df$element_type == "prediction_area_plot"]
      if (length(pid) == 1) {
        render_prediction_area_plot_server(
          output = output,
          element_id = ns(pid),
          geo_metadata = meta_geo_prediction_area_list,
          what = "prediction_area_sf",
          output_dir = output_dir
        )
      }
    })

    # Reactive collection of prediction input values, with NA for empty or null
    inputs_reactive <- shiny::reactive({
      df <- prediction_data()
      vals <- lapply(df$element_id, function(id) {
        val <- input[[id]]
        if (is.null(val) || (is.character(val) && val == "")) {
          NA
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

    # Reactive getters for selected uncertainty quantification method
    uncertainty_quantification <- shiny::reactive({ input[["uncertainty_quantification"]] })
    
    return(list(
      "prediction_inputs" = shiny::reactive(inputs_reactive()),
      "uncertainty_quantification" = uncertainty_quantification
    ))
  })
}

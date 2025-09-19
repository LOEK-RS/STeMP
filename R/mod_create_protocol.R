#' Create Protocol UI Module
#'
#' @description Provides a tabbed interface with three panels: Overview, Model, and Prediction.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
mod_create_protocol_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tabsetPanel(
      id = ns("tabset"),
      shiny::tabPanel("1. Overview", value = "Overview", mod_overview_panel_ui(ns("overview"))),
      shiny::tabPanel("2. Model", value = "Model", mod_model_panel_ui(ns("model"))),
      shiny::tabPanel("3. Prediction", value = "Prediction", mod_prediction_panel_ui(ns("prediction")))
    )
  )
}

#' Create Protocol Server Module
#'
#' Handles the logic for the protocol creation workflow, coordinating the Overview,
#' Model, and Prediction submodules. It processes uploaded CSV data, extracts values,
#' calculates spatial selections based on geographic metadata, and combines results
#' from submodules into an updated protocol data frame.
#'
#' @param id Module namespace ID
#' @param protocol_data Reactive data frame containing the raw protocol data
#' @param uploaded_csv Reactive expression providing the uploaded CSV data as a data frame
#' @param model_metadata ReactiveValues or reactive providing model metadata
#' @param geo_metadata ReactiveValues or reactive providing geographic metadata
#' @param output_dir temporary output directory
#'
#' @noRd
#'
#' @return A list with:
#' \itemize{
#'   \item{o_objective_1}{Reactive expression returning the selected modeling objective}
#'   \item{protocol_updated}{Reactive data frame of the combined, updated protocol values}
#' }
  mod_create_protocol_server <- function(id, protocol_data, uploaded_csv, model_metadata, geo_metadata, output_dir, model_deleted, csv_deleted, show_warnings, hide_optional = shiny::reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {


    # 1) Extract uploaded CSV values separated by sections Overview, Model, Prediction
    uploaded_values <- shiny::reactive({
      df <- uploaded_csv()
      if (is.null(df)) return(NULL)
      section_names <- c("Overview", "Model", "Prediction")
      sections_list <- lapply(section_names, function(section) {
        section_df <- df[df$section == section, c("element_id", "value")]
        if (nrow(section_df) == 0) return(NULL)
        else return(section_df)
      })
      names(sections_list) <- section_names
      sections_list
    })

    # 2) Initialize Overview panel submodule
    overview <- mod_overview_panel_server(
      "overview",
      protocol_data,
      uploaded_values = shiny::reactive({ uploaded_values()[["Overview"]] }),
      hide_optional = hide_optional   # <- pass here
    )

    # 3) Reactive selection classification based on geographic metadata and modeling objective
    geodist_sel <- shiny::reactive({
      shiny::req(overview$o_objective_1())
      area_sf <- switch(
        overview$o_objective_1(),
        "Model only" = geo_metadata$training_area_sf(),
        "Model and prediction" = geo_metadata$prediction_area_sf(),
        stop("Unsupported objective for geodist calculation")
      )
      samples_sf <- geo_metadata$samples_sf()
      shiny::req(samples_sf, area_sf)
      calculate_geodist_classification(samples_sf, area_sf)
    })

    # 4) Initialize Prediction panel submodule
    prediction_results <- mod_prediction_panel_server(
      "prediction",
      overview$o_objective_1,
      protocol_data,
      geo_metadata = geo_metadata,
      uploaded_values = shiny::reactive({ uploaded_values()[["Prediction"]] }),
      output_dir = output_dir,
      hide_optional = hide_optional   # <- pass here
    )

    # 5) Initialize Model panel submodule, pass protocol_data, metadata, reactive selections and uploaded Model values
    model_results <- mod_model_panel_server(
      "model",
      protocol_data,
      model_metadata = model_metadata,
      geo_metadata = geo_metadata,
      o_objective_1_val = overview$o_objective_1,
      geodist_sel = geodist_sel,
      uploaded_values = shiny::reactive({
        uploaded_values()[["Model"]]
      }),
      output_dir = output_dir,
      model_deleted = model_deleted
    )

    # 6) Combine data frames from Overview, Model, and (conditionally) Prediction panels into updated protocol
    updated_protocol <- shiny::reactive({
      overview_df <- overview$overview_inputs()
      model_df <- model_results$model_inputs()
      if (overview$o_objective_1() == "Model and prediction") {
        prediction_df <- prediction_results$prediction_inputs()
        df <- rbind(overview_df, model_df) |> dplyr::bind_rows(prediction_df)
      } else {
        df <- rbind(overview_df, model_df)
      }
      # Remove plot elements from combined protocol data
      df <- df[!grepl("plot", df$element), ]
      df
    })

    # 7) Run warnings module to check for sampling design, validation method, uncertainty, predictor types
    mod_warnings_server(
      id = "warnings",
      sampling_design = model_results$sampling_design,
      validation_method = model_results$validation_method,
      uncertainty_quantification = prediction_results$uncertainty_quantification,
      predictor_types = model_results$predictor_types,
      show_warnings = show_warnings
    )

    # 8) Re-set fields filled by uploaded protocol if the delete-uploaded-protocol button is activated
    current_model_ids <- shiny::reactiveVal(NULL)

    # Track current Overview input IDs
    current_overview_ids <- shiny::reactiveVal(NULL)

    # Update active IDs whenever a new CSV is uploaded
    shiny::observe({
      vals <- uploaded_values()[["Overview"]]
      if (!is.null(vals)) {
        current_overview_ids(vals$element_id)
      }
    })

    # Observe CSV deletion and reset all current Overview inputs
    shiny::observeEvent(csv_deleted(), {
      if (csv_deleted()) {
        ids <- current_overview_ids()
        if (!is.null(ids)) {
          for (element_id in ids) {
            if (!is.null(input[[element_id]])) {
              shiny::updateTextInput(session, element_id, value = "")
              shiny::updateNumericInput(session, element_id, value = NA)
              shiny::updateSelectInput(session, element_id, selected = "")
            }
          }
        }
        # Clear the tracked IDs
        current_overview_ids(NULL)
      }
    })


    # 9) Return reactive expressions for selected objective and updated protocol data frame
    return(list(
      o_objective_1 = overview$o_objective_1,
      protocol_updated = updated_protocol
    ))
  })
}

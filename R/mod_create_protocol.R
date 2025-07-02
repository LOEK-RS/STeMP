#' Create Protocol UI Module
#'
#' Provides a tabbed interface with three panels: Overview, Model, and Prediction.
#'
#' @param id Module namespace ID
#' @return UI elements for the protocol creation interface
mod_create_protocol_ui <- function(id) {
  ns <- NS(id)
  print(ns("model"))
  tagList(
    tabsetPanel(
      id = ns("tabset"),
      tabPanel("1. Overview", value = "Overview", mod_overview_panel_ui(ns("overview"))),
      tabPanel("2. Model", value = "Model", mod_model_panel_ui(ns("model"))),
      tabPanel("3. Prediction", value = "Prediction", mod_prediction_panel_ui(ns("prediction")))
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
#' 
#' @return A list with:
#' \itemize{
#'   \item{o_objective_1}{Reactive expression returning the selected modeling objective}
#'   \item{protocol_updated}{Reactive data frame of the combined, updated protocol values}
#' }
mod_create_protocol_server <- function(id, protocol_data, uploaded_csv, model_metadata, geo_metadata) {
  moduleServer(id, function(input, output, session) {
    
    # 1) Extract uploaded CSV values separated by sections Overview, Model, Prediction
    uploaded_values <- reactive({
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
    
    # 2) Initialize Overview panel submodule, pass protocol_data and uploaded Overview values
    overview <- mod_overview_panel_server("overview", protocol_data, uploaded_values = reactive({
      uploaded_values()[["Overview"]]
    }))
    
    # 3) Reactive selection classification based on geographic metadata and modeling objective
    geodist_sel <- reactive({
      req(overview$o_objective_1())
      area_sf <- switch(
        overview$o_objective_1(),
        "Model only" = geo_metadata$training_area_sf(),
        "Model and prediction" = geo_metadata$prediction_area_sf(),
        stop("Unsupported objective for geodist calculation")
      )
      samples_sf <- geo_metadata$samples_sf()
      req(samples_sf, area_sf)
      calculate_geodist_classification(samples_sf, area_sf)
    })
    
    # 4) Initialize Prediction panel submodule, pass reactive inputs and uploaded Prediction values
    prediction_results <- mod_prediction_panel_server(
      "prediction",
      overview$o_objective_1,
      protocol_data,
      geo_metadata = geo_metadata,
      uploaded_values = reactive({
        uploaded_values()[["Prediction"]]
      })
    )
    
    # 5) Initialize Model panel submodule, pass protocol_data, metadata, reactive selections and uploaded Model values
    model_results <- mod_model_panel_server(
      "model",
      protocol_data,
      model_metadata = model_metadata,
      geo_metadata = geo_metadata,
      o_objective_1_val = overview$o_objective_1,
      geodist_sel = geodist_sel,
      uploaded_values = reactive({
        uploaded_values()[["Model"]]
      })
    )
    
    # 6) Combine data frames from Overview, Model, and (conditionally) Prediction panels into updated protocol
    updated_protocol <- reactive({
      overview_df <- overview$overview_inputs()
      model_df <- model_results$model_inputs()
      if (overview$o_objective_1() == "Model and prediction") {
        prediction_df <- prediction_results$prediction_inputs()
        df <- rbind(overview_df, model_df) |> rbind(prediction_df)
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
      uncertainty_quantification = model_results$uncertainty_quantification,
      predictor_types = model_results$predictor_types
    )
    
    # 8) Return reactive expressions for selected objective and updated protocol data frame
    return(list(
      o_objective_1 = overview$o_objective_1,
      protocol_updated = updated_protocol
    ))
  })
}

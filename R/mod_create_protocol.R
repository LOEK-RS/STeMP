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

mod_create_protocol_server <- function(id, protocol_data, uploaded_csv, model_metadata, geo_metadata) {
  moduleServer(id, function(input, output, session) {
    
    
    # 1) Extract uploaded values from uploaded_csv
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
      return(sections_list)
      
    })
    
    
    # 2) Overview panel
    overview <- mod_overview_panel_server("overview", protocol_data, uploaded_values = reactive({
      uploaded_values()[["Overview"]]
    }))
    
    
    # 3) reactive geodist_sel
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
    
    
    # 4) Prediction panel
    prediction_results <- mod_prediction_panel_server(
      "prediction",
      overview$o_objective_1,
      protocol_data,
      geo_metadata = geo_metadata,
      uploaded_values = reactive({
        uploaded_values()[["Prediction"]]
      })
    )
    
    
    # 5) Model panel
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
    
    
    # 6) Combine results
    updated_protocol <- reactive({
      overview_df <- overview$overview_inputs()
      model_df <- model_results$model_inputs()
      if (overview$o_objective_1() == "Model and prediction") {
        prediction_df <- prediction_results$prediction_inputs()
        df <- rbind(overview_df, model_df) |> rbind(prediction_df)
      } else {
        df <- rbind(overview_df, model_df)
      }
      df <- df[!grepl("plot", df$element), ]
      df
    })
    
    
    # 7) Warnings
    mod_warnings_server(
      id = "warnings",
      sampling_design = model_results$sampling_design,
      validation_method = model_results$validation_method,
      uncertainty_quantification = model_results$uncertainty_quantification,
      predictor_types = model_results$predictor_types
    )
    
    # 8) Return
    return(list(
      o_objective_1 = overview$o_objective_1,
      protocol_updated = updated_protocol
    ))
  })
}

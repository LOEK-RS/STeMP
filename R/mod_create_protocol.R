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

mod_create_protocol_server <- function(id, protocol_data, model_metadata, geo_metadata) {
  moduleServer(id, function(input, output, session) {
    
    # 1) Overview panel (provides reactive o_objective_1)
    overview <- mod_overview_panel_server("overview", protocol_data)
    
    # Reactive geodist classification, depends on o_objective_1 and geo_metadata
    geodist_sel <- reactive({
      req(overview$o_objective_1())
      
      # Select correct area based on objective
      area_sf <- switch(
        overview$o_objective_1(),
        "Model only" = geo_metadata$training_area_sf(),
        "Model and prediction" = geo_metadata$prediction_area_sf(),
        stop("Unsupported objective for geodist calculation")
      )
      
      samples_sf <- geo_metadata$samples_sf()
      
      req(samples_sf, area_sf)
      
      # Use helper function for geodist classification (defined in utils.R)
      calculate_geodist_classification(samples_sf, area_sf)
    })
    
    # 2) Prediction panel
    prediction_results <- mod_prediction_panel_server(
      "prediction",
      overview$o_objective_1,
      protocol_data,
      geo_metadata = geo_metadata
    )
    
    # 3) Model panel - pass geodist_sel reactive for updating sampling_design field
    model_results <- mod_model_panel_server(
      "model",
      protocol_data,
      model_metadata = model_metadata,
      geo_metadata = geo_metadata,
      o_objective_1_val = overview$o_objective_1,
      geodist_sel = geodist_sel
    )
    
    
    # Extract updated data
    updated_protocol <- reactive({
      overview_df <- overview$overview_inputs()
      model_df <- model_results$model_inputs()
      
      if(overview$o_objective_1() == "Model and prediction") {
        prediction_df <- prediction_results$prediction_inputs()
        df <- rbind(overview_df, model_df) |> 
          rbind(prediction_df)
        
      } else {
        df <- rbind(overview_df, model_df)
      }
      
      df <- df[!grepl("plot", df$element),]
      return(df)
      
    })
    
    
    ## debug
    observe({
      req(model_results$sampling_design(), model_results$validation_method())
      message("Higher-level module sees uncertainty_quantification: ", model_results$uncertainty_quantification())
      message("Higher-level module sees validation_method: ", model_results$validation_method())
    })
    ##
    
    # Warnings
    mod_warnings_server(
      id = "warnings",
      sampling_design = model_results$sampling_design,
      validation_method = model_results$validation_method,
      uncertainty_quantification = model_results$uncertainty_quantification,
      predictor_types = model_results$predictor_types
    )
    
    # Return relevant reactive values
    return(list(
      o_objective_1 = overview$o_objective_1,
      protocol_updated = updated_protocol
    ))
  })
}

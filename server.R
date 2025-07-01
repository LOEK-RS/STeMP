server <- function(input, output, session) {
  
  
  # Cache CSV reading here once
  protocol_data <- reactive({
    req(csv_path)  # make sure csv_path is defined and available
    read.csv(csv_path, stringsAsFactors = FALSE)
  })
  
  
  # Upload csv, RDS and .gpkg data
  upload_mod <- mod_upload_server("upload")
  model_metadata <- mod_model_metadata_server("model_metadata", input_model_object = upload_mod$model)
  geo_metadata <- mod_gpkg_metadata_server("model_metadata",
                                           samples = upload_mod$samples, 
                                           training_area=upload_mod$training_area,
                                           prediction_area=upload_mod$prediction_area)
  
  
  # In server.R, call only the top-level modules that are directly used in your app's UI.
  # Only those that _server functions, others just define UI elements
  protocol <- mod_create_protocol_server("protocol", protocol_data, model_metadata, geo_metadata)  
  
  mod_sidebar_server("sidebar", protocol_data = protocol$protocol_updated, o_objective_1_val=protocol$o_objective_1)

 
}
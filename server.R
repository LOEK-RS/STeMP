server <- function(input, output, session) {
  
  # Reactive expression to read the protocol CSV data once per session
  protocol_data <- reactive({
    req(csv_path)  # Ensure csv_path is available before reading
    read.csv(csv_path, stringsAsFactors = FALSE)
  })
  
  # Initialize upload module to handle file uploads (CSV, RDS, GeoPackage)
  upload_mod <- mod_upload_server("upload")
  
  # Initialize metadata modules for model and geodata
  model_metadata <- mod_model_metadata_server("model_metadata", input_model_object = upload_mod$model)
  geo_metadata <- mod_gpkg_metadata_server("model_metadata",
                                           samples = upload_mod$samples, 
                                           training_area = upload_mod$training_area,
                                           prediction_area = upload_mod$prediction_area)
  
  # 1. Initialize sidebar module with raw protocol data.
  #    Sidebar handles toggling visibility of optional fields and filters protocol data accordingly.
  sidebar <- mod_sidebar_server("sidebar", protocol_data = protocol_data)
  
  # 2. Initialize protocol creation module,
  #    providing the filtered protocol data from the sidebar module along with uploaded data and metadata.
  protocol <- mod_create_protocol_server(
    "protocol",
    protocol_data = sidebar$filtered_protocol_data,
    uploaded_csv = upload_mod$csv,
    model_metadata = model_metadata,
    geo_metadata = geo_metadata
  )
  
  # 3. Pass the updated protocol data to sidebar and viewer modules for UI rendering
  #    Sidebar UI reflects any further changes in the protocol data and toggle state.
  #    Sidebar here handles the download of .csv, .pdf and .zip files/folders.
  mod_sidebar_server("sidebar", protocol_data = protocol$protocol_updated, o_objective_1_val = protocol$o_objective_1)
  
  # Viewer module renders the protocol PDF preview based on the updated protocol data.
  mod_viewer_server("viewer", protocol_data = protocol$protocol_updated, o_objective_1_val = protocol$o_objective_1)
}

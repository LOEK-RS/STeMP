#' Server logic for the main STeMP application
#'
#' Handles file uploads, metadata extraction, protocol data processing,
#' and coordinating modules for sidebar, protocol creation, and viewer.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#'
#' @return None; side effects include reactive outputs and module interactions.
#' @export
server <- function(input, output, session) {
  
  csv_path <- "www/stemp_dict.csv"
  
  # Reactive expression to read the protocol CSV data once per session
  protocol_data <- reactive({
    req(csv_path)  # Ensure csv_path is available before reading
    read.csv(csv_path, stringsAsFactors = FALSE)
  })
  
  # Initialize upload module to handle file uploads (CSV, RDS, GeoPackage)
  upload_mod <- mod_upload_server("upload")
  
  # Initialize metadata modules for model and geodata
  model_metadata <- mod_model_metadata_server("model_metadata", input_model_object = upload_mod$model)
  geo_metadata <- mod_gpkg_metadata_server(
    "model_metadata",
    samples = upload_mod$samples, 
    training_area = upload_mod$training_area,
    prediction_area = upload_mod$prediction_area
  )
  
  # 1. Initialize sidebar module with raw protocol data.
  #    Sidebar manages visibility toggling of optional fields and filters protocol data accordingly.
  sidebar <- mod_sidebar_server("sidebar", protocol_data = protocol_data)
  
  # 2. Initialize protocol creation module,
  #    providing filtered protocol data from the sidebar module plus uploaded data and metadata.
  protocol <- mod_create_protocol_server(
    "protocol",
    protocol_data = sidebar$filtered_protocol_data,
    uploaded_csv = upload_mod$csv,
    model_metadata = model_metadata,
    geo_metadata = geo_metadata
  )
  
  # 3. Pass updated protocol data to sidebar and viewer modules for UI rendering
  #    Sidebar UI reflects changes and handles downloads (.csv, .pdf, .zip).
  mod_sidebar_server(
    "sidebar", 
    protocol_data = protocol$protocol_updated, 
    o_objective_1_val = protocol$o_objective_1
  )
  
  # Viewer module renders the protocol PDF preview based on updated data.
  mod_viewer_server(
    "viewer", 
    protocol_data = protocol$protocol_updated, 
    o_objective_1_val = protocol$o_objective_1
  )
}

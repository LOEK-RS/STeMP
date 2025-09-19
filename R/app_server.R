#' Server logic for the main STeMP application
#'
#' Handles file uploads, metadata extraction, protocol data processing,
#' and coordinating modules for sidebar, protocol creation, and viewer.
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {

  csv_path <- app_sys("app/www/stemp_dict.csv")

  # Reactive expression to read the protocol CSV data once per session
  protocol_data <- reactive({
    req(csv_path)  # Ensure csv_path is available before reading
    utils::read.csv(csv_path, stringsAsFactors = FALSE)
  })

  # Create and expose a temporary folder to serve generated PDF previews and downloads
  temp_dir <- file.path(tempdir(), "temp_stemp")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  shiny::addResourcePath("temp_stemp", temp_dir)

  # Initialize upload module to handle file uploads (CSV, RDS, GeoPackage)
  upload_mod <- mod_upload_server("upload", output_dir = temp_dir)
  model_deleted <- shiny::reactive({ is.null(upload_mod$model()) })
  csv_deleted <- shiny::reactive({ is.null(upload_mod$csv()) })
  
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
  sidebar <- mod_sidebar_server("sidebar", protocol_data = protocol_data,
                                output_dir = temp_dir)

  # 2. Initialize protocol creation module,
  #    providing filtered protocol data from the sidebar module plus uploaded data and metadata.
  protocol <- mod_create_protocol_server(
    "protocol",
    protocol_data = sidebar$filtered_protocol_data,
    uploaded_csv = upload_mod$csv,
    model_metadata = model_metadata,
    geo_metadata = geo_metadata,
    output_dir = temp_dir,
    model_deleted = model_deleted,
    csv_deleted = csv_deleted,
    show_warnings = sidebar$show_warnings
  )

  # 3. Pass updated protocol data to sidebar and viewer modules for UI rendering
  #    Sidebar UI reflects changes and handles downloads (.csv, .pdf, .zip).

  ## needs to be checked for output_dir. Also, remove debug statements from viewer
  mod_sidebar_server(
    "sidebar",
    protocol_data = protocol$protocol_updated,
    o_objective_1_val = protocol$o_objective_1,
    output_dir = temp_dir
  )

  # Viewer module renders the protocol PDF preview based on updated data.
  mod_viewer_server(
    "viewer",
    protocol_data = protocol$protocol_updated,
    o_objective_1_val = protocol$o_objective_1,
    output_dir = temp_dir
  )


}

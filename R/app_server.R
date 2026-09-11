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
		req(csv_path) # Ensure csv_path is available before reading
		utils::read.csv(csv_path, stringsAsFactors = FALSE)
	})

	# Create and expose a temporary folder to serve generated PDF previews and downloads
	temp_dir <- file.path(tempdir(), "temp_stemp")
	dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
	shiny::addResourcePath("temp_stemp", temp_dir)

	# Initialize upload module to handle file uploads (CSV, RDS, GeoPackage)
	upload_mod <- mod_upload_server("upload", output_dir = temp_dir)
	model_deleted <- shiny::reactive({
		is.null(upload_mod$model())
	})

	# Initialize metadata modules for model and geodata
	model_metadata <- mod_model_metadata_server("model_metadata", input_model_object = upload_mod$model)
	geo_metadata <- mod_spatialdata_metadata_server(
		"model_metadata",
		samples = upload_mod$samples,
		training_area = upload_mod$training_area,
		prediction_area = upload_mod$prediction_area
	)

	# forward references: sidebar, protocol and the HTML renderer are mutually dependent.
	# These wrappers defer each lookup until the reactive is read,
	# by which point all three are bound in this environment.
	protocol_updated <- shiny::reactive(protocol$protocol_updated())
	o_objective_1_val <- shiny::reactive(protocol$o_objective_1())
	hide_optional <- shiny::reactive(sidebar$hide_optional())
	show_warnings <- shiny::reactive(sidebar$show_warnings())
	display_mode <- shiny::reactive(sidebar$display_mode())
	uploaded_csv <- shiny::reactive(sidebar$csv())
	uploaded_zip <- shiny::reactive(sidebar$zip())
	csv_deleted <- shiny::reactive(is.null(sidebar$csv()))

	# Render HTML used for downloading a PDF and for previewing the protocol
	render_protocol_html <- make_protocol_html(
		protocol_data = protocol_updated,
		protocol_dict = protocol_data,
		o_objective_1_val = o_objective_1_val,
		output_dir = temp_dir,
		session_token = session$token,
		hide_optional = hide_optional,
		display_mode = display_mode
	)

	# Initialize sidebar module with updated protocol data
	sidebar <- mod_sidebar_server(
		"sidebar",
		protocol_data = protocol_updated,
		protocol_dict = protocol_data,
		o_objective_1_val = o_objective_1_val,
		output_dir = temp_dir,
		generate_html = render_protocol_html
	)

	# Initialize protocol creation module and give it the hide_optional reactive so submodules can toggle visibility
	protocol <- mod_create_protocol_server(
		"protocol",
		protocol_data = protocol_data,
		uploaded_csv = uploaded_csv,
		uploaded_zip = uploaded_zip,
		model_metadata = model_metadata,
		geo_metadata = geo_metadata,
		output_dir = temp_dir,
		model_deleted = model_deleted,
		csv_deleted = csv_deleted,
		show_warnings = show_warnings,
		hide_optional = hide_optional,
		display_mode = display_mode
	)

	# Render viewer from the updated protocol
	mod_viewer_server(
		"viewer",
		generate_html = render_protocol_html,
		temp_dir = temp_dir
	)
}

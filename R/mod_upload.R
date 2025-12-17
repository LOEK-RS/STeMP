#' Upload Module - UI
#'
#' UI for uploading protocol CSV, model RDS, and geospatial data (gpkg),
#' including delete buttons for geospatial uploads.
#'
#' @param id Module namespace ID
#' @return UI elements for file uploads and status messages
#' @noRd
mod_upload_ui <- function(id) {
	ns <- shiny::NS(id)
	shiny::fluidPage(
		shinyjs::useShinyjs(),
		shiny::fluidRow(
			shiny::column(2),
			shiny::column(
				8,
				shiny::p(shiny::tags$b("Upload model (.RDS)")),
				shiny::fileInput(ns("model_upload"), "Upload model object (.RDS)", accept = ".rds"),
				shiny::uiOutput(ns("model_status")),
				shiny::actionButton(ns("delete_model"), "Delete uploaded model", style = "margin-bottom: 15px;"),
				shiny::tags$hr(),

				shiny::p(shiny::tags$b("Upload geospatial data (.gpkg)")),
				mod_gpkg_upload_ui(ns("samples"), label = "Upload sampling locations"),
				shiny::uiOutput(ns("sample_status")),
				shiny::actionButton(ns("delete_samples"), "Delete uploaded samples", style = "margin-bottom: 15px;"),
				mod_gpkg_upload_ui(ns("training_area"), label = "Upload training area"),
				shiny::uiOutput(ns("training_area_status")),
				shiny::actionButton(
					ns("delete_training_area"),
					"Delete uploaded training area",
					style = "margin-bottom: 15px;"
				),
				mod_gpkg_upload_ui(ns("prediction_area"), label = "Upload prediction area"),
				shiny::uiOutput(ns("prediction_area_status")),
				shiny::actionButton(
					ns("delete_prediction_area"),
					"Delete uploaded prediction area",
					style = "margin-bottom: 15px;"
				)
			),
			shiny::column(2)
		)
	)
}

#' Upload Module - Server
#'
#' Handles uploads for protocol CSV, model RDS, and geospatial gpkg files.
#' Provides status feedback and supports deleting uploaded geospatial data.
#'
#' @param id Module namespace ID
#' @return List of reactives for uploaded data objects:
#' \describe{
#'   \item{csv}{Reactive containing the protocol data frame or NULL}
#'   \item{model}{Reactive containing the loaded model object or NULL}
#'   \item{samples}{Reactive list from mod_gpkg_upload_server for sample points}
#'   \item{training_area}{Reactive list from mod_gpkg_upload_server for training area polygons}
#'   \item{prediction_area}{Reactive list from mod_gpkg_upload_server for prediction area polygons}
#' }
#' @noRd
mod_upload_server <- function(id, output_dir) {
	shiny::moduleServer(id, function(input, output, session) {
		ns <- session$ns

		# Reactive for model RDS object
		model_object <- shiny::reactiveVal(NULL)
		model_deleted <- shiny::reactiveVal(FALSE)

		shiny::observeEvent(input$model_upload, {
			shiny::req(input$model_upload)
			tryCatch(
				{
					obj <- readRDS(input$model_upload$datapath)
					model_object(obj)
					model_deleted(FALSE) # model now exists
					output$model_status <- shiny::renderUI({
						shiny::tags$p("Model loaded successfully", style = "color: blue;")
					})
				},
				error = function(e) {
					output$model_status <- shiny::renderUI({
						shiny::tags$p("Error loading RDS file", style = "color: red;")
					})
				}
			)
		})

		# Delete button for model input
		shiny::observeEvent(input$delete_model, {
			model_object(NULL)
			shinyjs::reset("model_upload")
			model_deleted(TRUE) # signal model deletion

			output$model_status <- shiny::renderUI({
				shiny::tags$p("Model upload deleted.", style = "color: orange;")
			})
		})

		# Geospatial uploads via nested modules
		samples <- mod_gpkg_upload_server("samples", geom_types_expected = c("POINT", "MULTIPOINT"))
		training_area <- mod_gpkg_upload_server("training_area", geom_types_expected = c("POLYGON", "MULTIPOLYGON"))
		prediction_area <- mod_gpkg_upload_server("prediction_area", geom_types_expected = c("POLYGON", "MULTIPOLYGON"))

		# Delete buttons to clear geospatial uploads and reset UI
		shiny::observeEvent(input$delete_samples, {
			samples$data(NULL)
			shinyjs::reset("samples-upload")
			delete_plot_png("sampling_locations", output_dir)
			delete_plot_png("geodist_sampling_area", output_dir)
			delete_plot_png("geodist_prediction_area", output_dir)
			output$sample_status <- shiny::renderUI({
				shiny::tags$p("Sample upload deleted.", style = "color: orange;")
			})
		})

		shiny::observeEvent(input$delete_training_area, {
			training_area$data(NULL)
			shinyjs::reset("training_area-upload")
			delete_plot_png("sampling_area", output_dir)
			delete_plot_png("geodist_sampling_area", output_dir)
			output$training_area_status <- shiny::renderUI({
				shiny::tags$p("Training area upload deleted.", style = "color: orange;")
			})
		})

		shiny::observeEvent(input$delete_prediction_area, {
			prediction_area$data(NULL)
			shinyjs::reset("prediction_area-upload")
			delete_plot_png("prediction_area", output_dir)
			delete_plot_png("geodist_prediction_area", output_dir)
			output$prediction_area_status <- shiny::renderUI({
				shiny::tags$p("Prediction area upload deleted.", style = "color: orange;")
			})
		})

		# Return reactives for use outside module
		list(
			model = model_object,
			model_deleted = model_deleted,
			samples = samples,
			training_area = training_area,
			prediction_area = prediction_area
		)
	})
}

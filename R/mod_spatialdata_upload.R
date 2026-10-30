#' GPKG Upload Module UI
#'
#' Provides a file input UI for uploading GeoPackage (.gpkg) files.
#'
#' @param id Module namespace ID
#' @param label Label for the file input button (default "Upload file")
#' @return UI elements for file input
#' @noRd
mod_spatialdata_upload_ui <- function(id, label = "Upload file") {
	ns <- shiny::NS(id)

	shiny::tagList(
		shiny::fileInput(ns("upload"), label, accept = c(".gpkg"))
	)
}

#' GPKG Upload Module Server
#'
#' Handles the server logic for uploading and validating a GeoPackage file.
#' Reads the uploaded file using `sf::st_read()` and validates the geometry
#' types against expected types.
#'
#' @param id Module namespace ID
#' @param geom_types_expected Character vector of allowed geometry types.
#'   Default is c("POINT", "MULTIPOINT").
#'
#' @return A list with reactive elements:
#' \itemize{
#'   \item{data}{Reactive returning the sf object read from the uploaded file, or NULL if none}
#'   \item{valid}{Reactive boolean indicating whether the uploaded file is valid}
#' }
#' @noRd
mod_spatialdata_upload_server <- function(id, geom_types_expected = c("POINT", "MULTIPOINT")) {
	shiny::moduleServer(id, function(input, output, session) {
		uploaded_data <- shiny::reactiveVal(NULL)
		valid <- shiny::reactiveVal(FALSE)
		upload_error <- shiny::reactiveVal(FALSE)
		geom_types_msg <- shiny::reactiveVal(NULL)

		shiny::observeEvent(input$upload, {
			shiny::req(input$upload)
			valid(FALSE)
			upload_error(FALSE)
			geom_types_msg(NULL)
			uploaded_data(NULL)

			# Attempt to read the uploaded GeoPackage file quietly
			sp_data <- tryCatch(
				{
					sf::st_read(input$upload$datapath, quiet = TRUE)
				},
				error = function(e) {
					upload_error(TRUE)
					NULL
				}
			)

			# If reading succeeded, check geometry types
			if (!is.null(sp_data)) {
				geom_type <- unique(sf::st_geometry_type(sp_data))
				if (!all(geom_type %in% geom_types_expected)) {
					geom_types_msg(paste0("Geometry must be one of: ", paste(geom_types_expected, collapse = ", "), "."))
					valid(FALSE)
				} else {
					uploaded_data(sp_data)
					valid(TRUE)
				}
			}
		})

		return(list(
			data = uploaded_data,
			valid = valid,
			upload_error = upload_error,
			geom_types_msg = geom_types_msg
		))
	})
}

#' Update Status UI for Spatial Data Upload Modules
#'
#' This helper function evaluates the state of a spatial data upload module
#' (error, geometry validity, or success) and updates the corresponding
#' UI output with a color-coded status message.
#'
#' @param output The Shiny output object from the server function.
#' @param mod_spatialdata A list of reactives returned by the mod_spatialdata_upload_server module.
#' @param output_id The character ID of the uiOutput to be updated.
#' @param msg A character string used to identify the data type in the message
#'   (e.g., "Samples" or "Training Area").
#'
#' @return NULL. This function updates the output object in the
#'   parent environment.
#' @noRd
spatialdata_status <- function(output, mod_spatialdata, output_id, msg) {
	if (mod_spatialdata$upload_error()) {
		output[[output_id]] <- shiny::renderUI({
			shiny::tags$p(paste0("Error loading ", msg, "."), style = "color: red;")
		})
	} else if (!is.null(mod_spatialdata$geom_types_msg())) {
		output[[output_id]] <- shiny::renderUI({
			shiny::tags$p(mod_spatialdata$geom_types_msg(), style = "color: red;")
		})
	} else if (!is.null(mod_spatialdata$data()) && mod_spatialdata$valid()) {
		output[[output_id]] <- shiny::renderUI({
			shiny::tags$p(
				paste0(paste0(toupper(substring(msg, 1, 1)), substring(msg, 2)), " loaded successfully."),
				style = "color: blue;"
			)
		})
	}
}

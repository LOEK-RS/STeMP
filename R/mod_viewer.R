#' Protocol HTML Viewer Module - UI
#'
#' UI for displaying a protocol HTML preview and a button to generate/refresh the HTML
#'
#' @param id Module namespace ID
#' @return UI elements including a header, action button, and HTML preview output
#' @noRd
mod_viewer_ui <- function(id) {
	ns <- shiny::NS(id)
	shiny::tagList(
		shiny::h4("Protocol HTML Preview"),
		shiny::actionButton(ns("preview_html"), "Generate / Refresh HTML Preview"),
		shiny::br(),
		shiny::br(),
		shiny::uiOutput(ns("html_preview_ui"))
	)
}

#' Protocol HTML Viewer Module - Server
#'
#' Generates a HTML preview of the protocol using provided data and plots,
#' manages temporary files, and renders a HTML viewer iframe.
#'
#' @param id Module namespace ID
#' @param protocol_data Reactive returning a data frame with protocol data
#' @param o_objective_1_val Reactive returning a string controlling which plots to include
#' @param output_dir temporary output directory
#' @return None; creates reactive outputs and handles side effects
#' @noRd
mod_viewer_server <- function(id, generate_html, temp_dir) {
	shiny::moduleServer(id, function(input, output, session) {
		ns <- session$ns

		# Register Shiny resource path (HTML previews live in temp dir)
		shiny::addResourcePath("temp_stemp", normalizePath(temp_dir, mustWork = TRUE))

		# ReactiveVal for currently displayed HTML (for iframe)
		html_preview_path <- shiny::reactiveVal(NULL)

		# Observe preview button
		shiny::observeEvent(input$preview_html, {
			# Generate new HTML (or reuse cached one)
			html_file <- generate_html()
			html_preview_path(html_file)
		})

		# Render iframe
		output$html_preview_ui <- shiny::renderUI({
			# Use the last generated HTML if available
			html_file <- html_preview_path()
			shiny::req(html_file)
			shiny::tags$iframe(
				src = paste0("temp_stemp/", basename(html_file)),
				style = "width:100%; height:700px;",
				frameborder = 0
			)
		})

		# Clean up old previews when session ends
		session$onSessionEnded(function() {
			html_file <- isolate(html_preview_path())
			if (!is.null(html_file) && file.exists(html_file)) {
				file.remove(html_file)
			}
		})
	})
}

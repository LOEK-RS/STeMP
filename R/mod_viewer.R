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
mod_viewer_server <- function(id, protocol_data, o_objective_1_val, output_dir) {
	shiny::moduleServer(id, function(input, output, session) {
		ns <- session$ns

		# define subdirectory for preview figures
		subdir_preview <- "figures_preview"

		# Cleanup any protocol preview HTMLs in output_dir when session ends
		session$onSessionEnded(function() {
			old_files <- list.files(
				output_dir,
				pattern = paste0("^protocol_preview_", session$token, ".*\\.html$"),
				full.names = TRUE
			)
			if (length(old_files) > 0) file.remove(old_files)
		})

		html_preview_path <- shiny::reactiveVal(NULL)

		# Register Shiny resource path
		shiny::addResourcePath("temp_stemp", normalizePath(output_dir, mustWork = TRUE))

		shiny::observeEvent(input$preview_html, {
			shiny::req(protocol_data())

			# Select and copy plot files based on objective
			allowed_ids <- get_allowed_element_ids(o_objective_1_val())
			plot_files_rel <- get_selected_plot_files(
				output_dir = output_dir,
				allowed_ids = allowed_ids,
				copy_subdir = subdir_preview,
				return_relative = TRUE
			)
			# Convert to absolute paths so Quarto can find them
			plot_files_abs <- normalizePath(file.path(output_dir, plot_files_rel), mustWork = TRUE)

			# Copy template QMD to temp_dir
			temp_qmd <- file.path(output_dir, "protocol_temp.qmd")
			template_path <- app_sys("app/www/protocol_template.qmd")
			file.copy(template_path, temp_qmd, overwrite = TRUE)

			# Remove old preview HTMLs
			old_htmls <- list.files(
				output_dir,
				pattern = paste0("^protocol_preview_", session$token, "_.*\\.html$"),
				full.names = TRUE
			)
			if (length(old_htmls) > 0) {
				file.remove(old_htmls)
			}

			html_filename <- paste0("protocol_preview_", session$token, "_", as.integer(Sys.time()), ".html")

			# load protocol data
			df <- protocol_data()
			df_sanitized <- df |>
				dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.x), "", .x))) |>
				dplyr::mutate(dplyr::across(dplyr::everything(), sanitize_latex)) |>
				dplyr::select(-"subsection", -"element_id")

			# Render HTML
			quarto::quarto_render(
				input = temp_qmd,
				output_file = html_filename,
				execute_params = list(
					data = df_sanitized,
					plot_files = plot_files_abs
				),
				execute_dir = output_dir
			)

			html_preview_path(html_filename)

			# Optional: Clean up temp plot files after render
			temp_figures_dir <- file.path(output_dir, subdir_preview)
			if (dir.exists(temp_figures_dir)) {
				unlink(temp_figures_dir, recursive = TRUE)
			}
		})

		# Render iframe
		output$html_preview_ui <- shiny::renderUI({
			shiny::req(html_preview_path())
			preview_filename <- basename(html_preview_path())
			shiny::tags$iframe(
				src = paste0("temp_stemp/", preview_filename),
				style = "width:100%; height:700px;",
				frameborder = 0
			)
		})
	})
}

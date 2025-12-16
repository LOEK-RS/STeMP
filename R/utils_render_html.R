#' Render Protocol HTML
#'
#' Returns a function that generates protocol HTML when called.
#'
#' @param protocol_data reactive returning the protocol data.frame
#' @param o_objective_1_val reactive returning the objective value
#' @param output_dir temporary output directory
#' @param session_token unique token for the session
#' @return a function() that generates and returns the HTML file path
#' @noRd
make_protocol_html <- function(protocol_data, o_objective_1_val, output_dir, session_token) {
	function() {
		shiny::req(protocol_data())

		subdir_preview <- "figures_preview"

		allowed_ids <- get_allowed_element_ids(o_objective_1_val())
		plot_files_rel <- get_selected_plot_files(
			output_dir = output_dir,
			allowed_ids = allowed_ids,
			copy_subdir = subdir_preview,
			return_relative = TRUE
		)

		plot_files_abs <- normalizePath(
			file.path(output_dir, plot_files_rel),
			mustWork = TRUE
		)

		temp_qmd <- file.path(output_dir, "protocol_temp.qmd")
		file.copy(
			app_sys("app/www/protocol_template.qmd"),
			temp_qmd,
			overwrite = TRUE
		)

		html_filename <- paste0(
			"protocol_preview_",
			session_token,
			"_",
			as.integer(Sys.time()),
			".html"
		)

		df_sanitized <- protocol_data() |>
			dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.x), "", .x))) |>
			dplyr::mutate(dplyr::across(dplyr::everything(), sanitize_text)) |>
			dplyr::select(-dplyr::all_of(c("subsection", "element_id")))

		quarto::quarto_render(
			input = temp_qmd,
			output_file = html_filename,
			execute_params = list(
				data = df_sanitized,
				plot_files = plot_files_abs
			),
			execute_dir = output_dir,
			quiet = FALSE
		)

		unlink(file.path(output_dir, subdir_preview), recursive = TRUE)

		file.path(output_dir, html_filename)
	}
}

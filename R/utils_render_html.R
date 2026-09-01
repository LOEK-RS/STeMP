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
make_protocol_html <- function(
	protocol_data,
	protocol_dict,
	o_objective_1_val,
	output_dir,
	session_token,
	hide_optional = shiny::reactive(FALSE)
) {
	function() {
		shiny::req(protocol_data())

		subdir_preview <- "figures_preview"

		allowed_ids <- get_allowed_element_ids(
			o_objective_1_val(),
			uploaded_figure_ids = visible_uploaded_figure_ids(
				protocol_dict = protocol_dict(),
				output_dir = output_dir,
				hide_optional = hide_optional()
			)
		)

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

		plot_captions <- build_figure_captions(
			plot_files = plot_files_abs,
			protocol_dict = protocol_dict(),
			protocol_values = protocol_data()
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
				plot_files = plot_files_abs,
				plot_captions = plot_captions
			),
			execute_dir = output_dir,
			quiet = FALSE
		)

		unlink(file.path(output_dir, subdir_preview), recursive = TRUE)

		file.path(output_dir, html_filename)
	}
}


#' Build captions for the protocol figures
#'
#' Precedence: the user's caption row (<element_id>_caption), then the
#' dictionary label for that element, then the file stem. Returns a character
#' vector parallel to `plot_files`, so the template needs no filename matching.
#'
#' @param plot_files Character vector of figure paths
#' @param protocol_dict The dictionary data frame
#' @param protocol_values The filled protocol (must still contain element_id)
#' @noRd
build_figure_captions <- function(plot_files, protocol_dict, protocol_values) {
	stems <- tools::file_path_sans_ext(basename(as.character(plot_files)))

	lookup <- function(df, element_id, column) {
		if (is.null(df) || !all(c("element_id", column) %in% names(df))) {
			return("")
		}
		hit <- df[[column]][df$element_id == element_id]
		hit <- hit[!is.na(hit)]
		if (length(hit) != 1 || !nzchar(trimws(hit))) "" else trimws(hit)
	}

	captions <- vapply(
		stems,
		function(stem) {
			user_caption <- lookup(protocol_values, paste0(stem, "_caption"), "value")
			if (nzchar(user_caption)) {
				return(user_caption)
			}

			label <- lookup(protocol_dict, stem, "element")
			if (nzchar(label)) {
				return(label)
			}

			stem
		},
		character(1),
		USE.NAMES = FALSE
	)

	vapply(captions, sanitize_text, character(1), USE.NAMES = FALSE)
}

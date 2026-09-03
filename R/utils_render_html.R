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
	function(layout = c("sections", "table")) {
		layout <- match.arg(layout)
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
			winslash = "/",
			mustWork = FALSE
		)
		plot_files_abs <- plot_files_abs[file.exists(plot_files_abs)]

		plot_captions <- build_figure_captions(
			plot_files = plot_files_abs,
			protocol_dict = protocol_dict(),
			protocol_values = protocol_data()
		)

		figures_df <- data.frame(
			element_id = tools::file_path_sans_ext(basename(plot_files_abs)),
			path = plot_files_abs,
			caption = plot_captions,
			stringsAsFactors = FALSE
		)

		structure_df <- build_report_structure(
			protocol_values = protocol_data(),
			protocol_dict = protocol_dict(),
			figures_df = figures_df
		)

		# Unchanged flat table, still used by the "table" layout
		caption_ids <- report_excluded_element_ids(protocol_dict())
		table_df <- protocol_data()
		table_df <- table_df[!table_df$element_id %in% caption_ids, , drop = FALSE]

		df_sanitized <- table_df |>
			dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.x), "", .x))) |>
			dplyr::mutate(dplyr::across(dplyr::everything(), sanitize_text)) |>
			dplyr::select(-dplyr::all_of(c("subsection", "element_id")))

		temp_qmd <- file.path(output_dir, "protocol_temp.qmd")
		file.copy(app_sys("app/www/protocol_template.qmd"), temp_qmd, overwrite = TRUE)

		# The SCSS must sit next to the qmd, because execute_dir is output_dir
		file.copy(app_sys("app/www/stemp_report.scss"), file.path(output_dir, "stemp_report.scss"), overwrite = TRUE)

		html_filename <- paste0(
			"protocol_preview_",
			session_token,
			"_",
			as.integer(Sys.time()),
			".html"
		)

		quarto::quarto_render(
			input = temp_qmd,
			output_file = html_filename,
			execute_params = list(
				layout = layout,
				structure = structure_df,
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


#' Element types that are rendered as figures
#' @noRd
report_figure_types <- function() {
	c(
		"training_plot",
		"training_area_plot",
		"geodist_plot_training",
		"prediction_area_plot",
		"geodist_plot_prediction",
		"uploaded_figure"
	)
}

#' Assemble the ordered list of items to render in the report
#'
#' Walks the dictionary in file order, so the report follows the same sequence
#' as the web app. Fields are kept only if they survived the upstream filtering
#' in updated_protocol() (hide_optional, classification/regression, objective);
#' figures only if their PNG was selected for this report.
#'
#' @param protocol_values The filled protocol (needs section, element_id, value)
#' @param protocol_dict The dictionary data frame
#' @param figures_df Data frame with element_id, path, caption
#' @param hide_empty Drop fields with no value
#' @return Data frame: section, subsection, element, element_id, kind,
#'   value, path, caption
#' @noRd
build_report_structure <- function(protocol_values, protocol_dict, figures_df, hide_empty = TRUE) {
	dict <- protocol_dict

	# Captions are rendered under their figure, never as an item
	dict <- dict[!dict$element_id %in% report_excluded_element_ids(dict), , drop = FALSE]

	dict$kind <- ifelse(dict$element_type %in% report_figure_types(), "figure", "field")
	dict$order <- seq_len(nrow(dict))

	# element_id is unique per section but not globally (performance_metrics
	# exists in both Model and Prediction), so join on both
	values <- protocol_values[, c("section", "element_id", "value"), drop = FALSE]
	values$value <- as.character(values$value)
	values$value[is.na(values$value)] <- ""

	items <- merge(dict, values, by = c("section", "element_id"), all.x = TRUE)

	figures_df <- figures_df[, c("element_id", "path", "caption"), drop = FALSE]
	items <- merge(items, figures_df, by = "element_id", all.x = TRUE)

	items <- items[order(items$order), , drop = FALSE]

	keep_field <- items$kind == "field" &
		items$element_id %in% protocol_values$element_id &
		(items$optional == 0 | !hide_empty | (!is.na(items$value) & nzchar(items$value)))

	keep_figure <- items$kind == "figure" & !is.na(items$path)

	items <- items[keep_field | keep_figure, , drop = FALSE]

	report_columns <- c("section", "subsection", "element", "element_id", "kind", "value", "path", "caption")

	if (nrow(items) == 0) {
		return(items[, report_columns, drop = FALSE])
	}

	# Values go into raw HTML; captions were sanitised in build_figure_captions()
	for (column in c("section", "subsection", "element", "value")) {
		items[[column]] <- vapply(items[[column]], sanitize_text, character(1), USE.NAMES = FALSE)
	}

	# Quarto's YAML serialiser rejects NA, and merge() leaves NA in path/caption
	# for field rows
	for (column in c("element_id", "kind", "path", "caption")) {
		items[[column]] <- as.character(items[[column]])
		items[[column]][is.na(items[[column]])] <- ""
	}

	items[, report_columns, drop = FALSE]
}

#' Element IDs that are report annotations rather than protocol content
#'
#' Figure captions are rendered under their figure, so they are excluded from
#' the report table and the progress bars. They stay in the CSV download so a
#' downloaded protocol can be re-uploaded without losing them.
#' @noRd
report_excluded_element_ids <- function(protocol_dict) {
	if (is.null(protocol_dict) || !all(c("element_id", "element_type") %in% names(protocol_dict))) {
		return(character(0))
	}
	protocol_dict$element_id[protocol_dict$element_type == "figure_caption"]
}

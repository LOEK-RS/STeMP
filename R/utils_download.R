#' @param uploaded_figure_ids Character vector of user-uploaded figure IDs that
#'   are currently visible in the protocol
#' @noRd
get_allowed_element_ids <- function(objective, uploaded_figure_ids = character(0)) {
	derived <- switch(
		objective,
		"Model and prediction" = c(
			"training_locations",
			"training_area",
			"prediction_area",
			"geodist_prediction_area"
		),
		"Model only" = c(
			"training_locations",
			"training_area",
			"geodist_training_area"
		),
		character(0)
	)

	c(derived, uploaded_figure_ids)
}


#' Select and copy allowed plot PNGs based on objective
#'
#' Filters and copies allowed .png files to a temporary directory.
#'
#' @param output_dir The directory where .png files are located
#' @param allowed_ids Character vector of allowed element IDs
#' @param copy_subdir Subdirectory (relative to output_dir) to copy selected figures into
#' @param return_relative Logical; if TRUE, return relative paths; if FALSE, return full paths
#'
#' @return Character vector of plot file paths
#' @noRd
get_selected_plot_files <- function(output_dir, allowed_ids, copy_subdir = "figures", return_relative = TRUE) {
	all_plot_files <- list.files(output_dir, pattern = "\\.png$", full.names = TRUE)

	file_names <- basename(all_plot_files)
	ids_no_ext <- trimws(sub("\\.png$", "", tolower(file_names)))
	allowed_ids_lc <- trimws(tolower(allowed_ids))

	match_idx <- match(allowed_ids_lc, ids_no_ext)
	valid_idx <- which(!is.na(match_idx))
	selected_files <- all_plot_files[match_idx[valid_idx]]

	target_dir <- file.path(output_dir, copy_subdir)
	if (!dir.exists(target_dir)) {
		dir.create(target_dir, recursive = TRUE)
	}

	file.copy(selected_files, target_dir, overwrite = TRUE)

	if (return_relative) {
		file.path(copy_subdir, basename(selected_files))
	} else {
		file.path(target_dir, basename(selected_files))
	}
}


#' IDs of uploaded figures that exist on disk and are currently visible
#'
#' Visibility cannot be read from the filtered protocol, because figure rows are
#' deliberately removed from it before rendering. It is therefore derived from
#' the dictionary's `optional` flag and the "hide optional fields" toggle.
#'
#' @param protocol_dict The dictionary data frame
#' @param output_dir Directory holding the figure PNGs
#' @param hide_optional Logical; TRUE when optional fields are hidden
#' @noRd
visible_uploaded_figure_ids <- function(protocol_dict, output_dir, hide_optional = FALSE) {
	rows <- protocol_dict[protocol_dict$element_type == "uploaded_figure", , drop = FALSE]

	if (isTRUE(hide_optional)) {
		rows <- rows[rows$optional == 0, , drop = FALSE]
	}

	figure_ids <- rows$element_id
	figure_ids[file.exists(file.path(output_dir, paste0(figure_ids, ".png")))]
}

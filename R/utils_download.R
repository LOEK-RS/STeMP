#' Get Allowed Plot Element IDs Based on Objective
#'
#' Returns a character vector of allowed plot element IDs depending on the selected objective.
#' Used to determine which plots are relevant for rendering or deletion in the app.
#'
#' @param objective A character string, either `"Model and prediction"` or `"Model only"`.
#'
#' @return A character vector of element IDs relevant to the specified objective.
#' If the objective does not match a known case, an empty character vector is returned.
#'
#' @examples
#' get_allowed_element_ids("Model only")
#'
#' @noRd
get_allowed_element_ids <- function(objective) {
	switch(
		objective,
		"Model and prediction" = c(
			"sampling_locations",
			"sampling_area",
			"prediction_area",
			"geodist_prediction_area"
		),
		"Model only" = c(
			"sampling_locations",
			"sampling_area",
			"geodist_sampling_area"
		),
		character(0)
	)
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

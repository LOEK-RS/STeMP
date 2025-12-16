#' Capitalize First Letter of a String
#'
#' Converts the first character of a string to uppercase.
#'
#' @param x A character vector.
#' @return Character vector with first letter capitalized.
#' @noRd
firstup <- function(x) {
	substr(x, 1, 1) <- toupper(substr(x, 1, 1))
	x
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
"%||%" <- function(x, y) {
	if (is.null(x)) {
		y
	} else {
		x
	}
}

#' Normalize Text to ID Format
#'
#' Converts text to lowercase, removes asterisks, and replaces punctuation and spaces with underscores.
#'
#' @param x Character string to normalize.
#' @return Normalized character string suitable for IDs.
#' @noRd
normalize_id <- function(x) {
	gsub("[^a-zA-Z0-9]", "_", tolower(gsub("\\*", "", x)))
}


#' Get Value from Uploaded Input or Fallback
#'
#' @param uploaded_value Value from uploaded input.
#' @param fallback_fn Function to call if uploaded_value is NULL.
#' @return The uploaded value, fallback function result, or NULL.
#' @noRd
get_value <- function(uploaded_value, fallback_fn) {
	if (!is.null(uploaded_value)) {
		return(uploaded_value)
	} else if (!is.null(fallback_fn)) {
		return(fallback_fn())
	} else {
		return(NULL)
	}
}


#' Save ggplot Figure to File
#'
#' @param figure ggplot object.
#' @param element_id Character ID to determine filename.
#' @noRd
save_figure <- function(figure, element_id, temp_dir = NULL) {
	fig_dir <- file.path(temp_dir)
	if (!dir.exists(fig_dir)) {
		dir.create(fig_dir, recursive = TRUE)
	}

	fig_name <- switch(
		element_id,
		"protocol-prediction-geodistance_plot_prediction_area" = "geodist_prediction_area",
		"protocol-prediction-prediction_map" = "prediction_area",
		"protocol-model-sampling_area_map" = "sampling_area",
		"protocol-model-sampling_locations" = "sampling_locations",
		"protocol-model-geodistance_plot_sampling_area" = "geodist_sampling_area",
		element_id
	)

	plot_path <- file.path(fig_dir, paste0(fig_name, ".png"))

	ggplot2::ggsave(plot_path, plot = figure, width = 6, height = 4, dpi = 300)

	return(plot_path)
}

#' Delete plot PNG by element ID
#'
#' Deletes a PNG file matching the element ID from a specified directory.
#'
#' @param element_id The ID of the element (e.g., "sampling_locations")
#' @param output_dir Directory where plot PNGs are stored
#' @noRd
delete_plot_png <- function(element_id, output_dir) {
	file_path <- file.path(output_dir, paste0(element_id, ".png"))
	if (file.exists(file_path)) {
		file.remove(file_path)
	}
}


#' Sanitize Text for HTML / PDF
#'
#' Replaces newlines with spaces. No LaTeX escaping needed because HTML is converted to PDF.
#'
#' @param x A character string.
#' @return A character string safe for HTML / PDF output.
#' @export
sanitize_text <- function(x) {
	if (is.null(x)) {
		return("")
	}
	x <- as.character(x)
	# Replace NA with empty string
	x[is.na(x)] <- ""
	# Escape HTML special characters
	x <- gsub("&", "&amp;", x, fixed = TRUE)
	x <- gsub("<", "&lt;", x, fixed = TRUE)
	x <- gsub(">", "&gt;", x, fixed = TRUE)
	x <- gsub('"', "&quot;", x, fixed = TRUE)
	# Replace newlines with spaces
	x <- gsub("\n", " ", x)
	x
}

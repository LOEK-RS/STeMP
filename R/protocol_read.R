#' Read a protocol object
#'
#' @param protocol_path Path to the protocol file. Currently only CSV files are supported.
#' @return A data frame of the protocol which can be used by [protocol_analyze()].
#' @export
protocol_read <- function(protocol_path) {
	# Checks
	if (!is.character(protocol_path) || length(protocol_path) != 1L) {
		stop("`protocol_path` must be a single character string.", call. = FALSE)
	}

	if (!file.exists(protocol_path)) {
		stop("File does not exist: ", protocol_path, call. = FALSE)
	}

	if (!grepl("\\.csv$", protocol_path, ignore.case = TRUE)) {
		stop("Only CSV protocol files are currently supported.", call. = FALSE)
	}

	# Read protocol
	out <- utils::read.csv(protocol_path)

	# Check expected columns
	required_cols <- c("section", "subsection", "element", "element_id", "value")
	missing_cols <- setdiff(required_cols, names(out))

	if (length(missing_cols) > 0) {
		stop(
			"The protocol file is missing required columns: ",
			paste(missing_cols, collapse = ", "),
			call. = FALSE
		)
	}

	class(out) <- c("stemp_data", class(out))
	out
}

#' Column name carrying time information in uploaded spatial data
#' @noRd
stemp_time_column <- function() "time"

#' Does an sf object carry the expected time column?
#' @noRd
has_time_column <- function(x) {
	inherits(x, "sf") && stemp_time_column() %in% names(x)
}

#' Does an sf object carry a parseable time column?
#' @noRd
has_usable_time <- function(x) {
	!is.null(parse_time_column(x))
}

#' Pull one sf element out of validated geo metadata, or NULL
#'
#' The elements are reactives and may be absent entirely, so both the
#' missing-name and the failing-call cases have to be swallowed.
#' @noRd
geo_sf <- function(meta, what) {
	if (is.null(meta) || is.null(meta[[what]])) {
		return(NULL)
	}
	tryCatch(meta[[what]](), error = function(e) NULL)
}

#' Parse the time column into Date / POSIXct / numeric
#'
#' GeoPackage DATE and DATETIME fields come back as Date / POSIXct, but a
#' plain TEXT column does not, so character input is parsed here. Returns NULL
#' when the column is missing or unparseable, which callers treat as
#' "no temporal information available".
#'
#' @param x An sf object
#' @return A vector of times, or NULL
#' @noRd
parse_time_column <- function(x) {
	if (!has_time_column(x)) {
		return(NULL)
	}

	values <- x[[stemp_time_column()]]

	if (inherits(values, c("Date", "POSIXct"))) {
		return(values)
	}
	if (is.numeric(values)) {
		return(values)
	}

	values <- as.character(values)

	parsed <- suppressWarnings(as.POSIXct(values, tz = "UTC"))
	if (all(is.na(parsed))) {
		parsed <- suppressWarnings(as.Date(values))
	}
	if (all(is.na(parsed))) {
		return(NULL)
	}

	parsed
}

#' One-column data frame of times, shaped for CAST::geodist()
#'
#' The column must be named "time" because that name is passed on as the
#' time variable.
#' @noRd
time_table <- function(x) {
	parsed <- parse_time_column(x)
	if (is.null(parsed)) {
		return(NULL)
	}
	parsed <- parsed[!is.na(parsed)]
	if (length(parsed) == 0) {
		return(NULL)
	}
	stats::setNames(data.frame(parsed), stemp_time_column())
}

#' Human-readable time span
#' @noRd
format_time_extent <- function(times) {
	if (is.null(times) || length(times) == 0) {
		return(NULL)
	}
	paste(format(range(times, na.rm = TRUE)), collapse = " to ")
}

#' Number of distinct time stamps
#' @noRd
count_timesteps <- function(times) {
	if (is.null(times)) {
		return(NULL)
	}
	length(unique(times))
}

#' Median spacing between distinct time stamps, as a readable string
#' @noRd
format_time_resolution <- function(times) {
	if (is.null(times)) {
		return(NULL)
	}
	unique_times <- sort(unique(times))
	if (length(unique_times) < 2) {
		return(NULL)
	}

	if (inherits(unique_times, "Date")) {
		step_seconds <- stats::median(as.numeric(diff(unique_times), units = "days")) * 86400
	} else if (inherits(unique_times, "POSIXct")) {
		step_seconds <- stats::median(as.numeric(diff(unique_times), units = "secs"))
	} else {
		return(paste(stats::median(diff(unique_times)), "(unitless)"))
	}

	breaks <- c(1, 60, 3600, 86400, 86400 * 7, 86400 * 30, 86400 * 365)
	labels <- c("second", "minute", "hour", "day", "week", "month", "year")
	idx <- max(which(step_seconds >= breaks * 0.9))
	value <- round(step_seconds / breaks[idx], 1)

	paste0(value, " ", labels[idx], if (value != 1) "s" else "")
}

#' Distinct geometries of an sf object
#'
#' With repeated observations at the same location, sample-to-sample
#' distances would otherwise be dominated by zeros.
#' @noRd
unique_geometries <- function(x) {
	if (!inherits(x, "sf")) {
		return(NULL)
	}
	geom <- sf::st_geometry(x)
	sf::st_as_sf(geom[!duplicated(sf::st_as_binary(geom, hex = TRUE))])
}

#' Count observations per distinct location
#'
#' @param samples_sf sf object of sample locations, possibly with repeats
#' @return sf object of distinct locations with an integer column `n`
#' @noRd
count_sample_repetitions <- function(samples_sf) {
	if (!inherits(samples_sf, "sf") || nrow(samples_sf) == 0) {
		return(NULL)
	}

	geom <- sf::st_geometry(samples_sf)
	geom_key <- sf::st_as_binary(geom, hex = TRUE)
	keep <- !duplicated(geom_key)
	counts <- table(geom_key)

	sf::st_sf(
		n = as.integer(counts[geom_key[keep]]),
		geometry = geom[keep]
	)
}

#' Column name carrying time information in uploaded spatial data
#' @noRd
stemp_time_column <- function() "time"

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

#' Extract the time column as POSIXct
#'
#' STeMP accepts only date-time information. Typed Date and POSIXct columns
#' are used directly, character columns are parsed, and everything else is
#' rejected -- notably numeric, which has no origin and so cannot be turned
#' into a duration.
#'
#' @param x An sf object or data frame
#' @return A POSIXct vector, or NULL when no usable time information exists
#' @noRd
parse_time_column <- function(x) {
	col <- stemp_time_column()
	if (is.null(x) || !col %in% names(x)) {
		return(NULL)
	}

	values <- x[[col]]

	parsed <- if (inherits(values, "POSIXt")) {
		as.POSIXct(values)
	} else if (inherits(values, "Date")) {
		# Explicit rather than as.POSIXct.Date, whose tz default has moved
		# between R versions.
		.POSIXct(unclass(values) * 86400, tz = "UTC")
	} else if (is.character(values) || is.factor(values)) {
		parse_time_strings(as.character(values))
	} else {
		NULL
	}

	if (is.null(parsed) || all(is.na(parsed))) {
		return(NULL)
	}

	parsed
}

#' Parse character timestamps without signalling
#'
#' as.POSIXct.character stop()s unless one standard format matches every
#' non-NA element, so the call is wrapped. NA entries are permitted and
#' survive into the result.
#'
#' @param values A character vector
#' @return A POSIXct vector, or NULL when no format matches
#' @noRd
parse_time_strings <- function(values) {
	values[!nzchar(trimws(values))] <- NA_character_

	# strptime() ignores trailing characters, so "%Y-%m-%d" happily matches
	# "2020-03-01T09:30:00" and discards the clock time without complaint.
	# The ISO 8601 variants therefore have to be tried before as.POSIXct()
	# reaches its own, more permissive, format list.
	iso_formats <- c(
		"%Y-%m-%dT%H:%M:%OSZ",
		"%Y-%m-%dT%H:%M:%OS",
		"%Y-%m-%dT%H:%M"
	)

	for (fmt in iso_formats) {
		iso <- suppressWarnings(as.POSIXct(values, tz = "UTC", format = fmt))
		if (any(!is.na(iso)) && all(is.na(iso) == is.na(values))) {
			return(iso)
		}
	}

	tryCatch(
		suppressWarnings(as.POSIXct(values, tz = "UTC")),
		error = function(e) NULL
	)
}

#' @noRd
has_usable_time <- function(x) {
	!is.null(parse_time_column(x))
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
	if (is.null(times) || length(times) == 0) {
		return(NULL)
	}
	unique_times <- sort(unique(times))
	if (length(unique_times) < 2) {
		return(NULL)
	}

	step_seconds <- stats::median(as.numeric(diff(unique_times), units = "secs"))
	if (!is.finite(step_seconds) || step_seconds <= 0) {
		return(NULL)
	}

	breaks <- c(1, 60, 3600, 86400, 86400 * 7, 86400 * 30, 86400 * 365)
	labels <- c("second", "minute", "hour", "day", "week", "month", "year")
	idx <- which(step_seconds >= breaks * 0.9)
	idx <- if (length(idx) == 0) 1L else max(idx)

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

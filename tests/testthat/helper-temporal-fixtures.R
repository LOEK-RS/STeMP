#' Build a point sf carrying a `time` column of arbitrary type
#'
#' The time vector drives the row count. Coordinates come from a deterministic
#' lattice rather than an RNG, so fixtures are reproducible without a set.seed()
#' call leaking into the calling test.
#'
#' @param times Vector of any type, written verbatim to the `time` column
#' @noRd
make_time_sf <- function(times, crs = 4326, x0 = 8, y0 = 50) {
	k <- seq_along(times)
	df <- data.frame(
		id = k,
		x = x0 + (k * 0.017) %% 0.5,
		y = y0 + (k * 0.029) %% 0.5,
		stringsAsFactors = FALSE
	)
	df[[stemp_time_column()]] <- times

	sf::st_as_sf(df, coords = c("x", "y"), crs = crs)
}

#' One square polygon per timestep, standing in for a prediction area
#' @noRd
make_time_area <- function(times, crs = 4326, x0 = 8, y0 = 50) {
	k <- seq_along(times)
	polys <- lapply(k, function(i) {
		dx <- (i - 1) * 0.05
		sf::st_polygon(list(cbind(
			c(x0 + dx, x0 + dx + 0.4, x0 + dx + 0.4, x0 + dx, x0 + dx),
			c(y0, y0, y0 + 0.4, y0 + 0.4, y0)
		)))
	})

	out <- sf::st_sf(id = k, geometry = sf::st_sfc(polys, crs = crs))
	out[[stemp_time_column()]] <- times
	out
}

#' Named list of temporal fixtures, all in memory
#'
#' Keys are referenced by name in the temporal test files. Every element is an
#' sf object differing only in its `time` column.
#' @noRd
temporal_fixtures <- function() {
	days <- as.Date("2020-03-01") + seq(0, by = 7, length.out = 12)
	stamps <- as.POSIXct(paste(days, "09:30:00"), tz = "UTC")
	minutes <- as.POSIXct("2020-03-01 09:00:00", tz = "UTC") +
		as.difftime(seq(0, by = 5, length.out = 12), units = "mins")

	no_time <- make_time_sf(days)
	no_time[[stemp_time_column()]] <- NULL

	list(
		posix = make_time_sf(stamps),
		date = make_time_sf(days),
		text_iso = make_time_sf(format(days, "%Y-%m-%d")),
		text_datetime = make_time_sf(format(stamps, "%Y-%m-%d %H:%M:%S")),
		text_iso8601 = make_time_sf(format(stamps, "%Y-%m-%dT%H:%M:%SZ")),
		factor_iso = make_time_sf(factor(format(days, "%Y-%m-%d"))),
		blank_text = make_time_sf(c(format(days[1:6], "%Y-%m-%d"), rep("  ", 6))),

		# Rejected by the POSIXct-only contract
		numeric = make_time_sf(as.numeric(seq_along(days))),
		integer_year = make_time_sf(seq(2005L, length.out = length(days))),
		double_year = make_time_sf(as.numeric(seq(2005, length.out = length(days)))),

		# Rejected because nothing parses
		partial = make_time_sf(c(format(days[1:6], "%Y-%m-%d"), rep("not a date", 6))),
		unparseable = make_time_sf(rep("n/a", length(days))),
		mixed_formats = make_time_sf(c(
			format(stamps[1:6], "%Y-%m-%dT%H:%M:%SZ"),
			format(days[7:12], "%Y-%m-%d")
		)),
		all_na = make_time_sf(rep(NA_character_, length(days))),
		no_time = no_time,

		# Parse, but degenerate downstream
		partial_na = make_time_sf(c(format(days[1:6], "%Y-%m-%d"), rep(NA_character_, 6))),
		one_usable = make_time_sf(c(format(days[1], "%Y-%m-%d"), rep(NA_character_, 11))),
		two_usable = make_time_sf(c(format(days[1:2], "%Y-%m-%d"), rep(NA_character_, 10))),
		constant = make_time_sf(rep(stamps[1], length(days))),
		single = make_time_sf(stamps[1]),
		minutes = make_time_sf(minutes),

		area_posix = make_time_area(stamps),
		area_numeric = make_time_area(as.numeric(seq_along(days))),
		area_no_time = make_time_area(stamps)[, "id"]
	)
}

#' Fake one layer of the spatialdata metadata module's input contract
#' @noRd
fake_layer <- function(x) {
	list(
		data = shiny::reactive(x),
		valid = shiny::reactive(!is.null(x))
	)
}

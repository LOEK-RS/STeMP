#' Point sf with independently controlled numbers of distinct locations and times
#'
#' Row count is `max(n_locations, n_times)`; locations and timestamps are each
#' cycled across the rows, so `unique_geometries()` yields exactly
#' `n_locations` rows and `unique_times()` exactly `n_times`. Either axis can
#' therefore be pushed past `geodist_max_n()` on its own. Deliberately not part
#' of `temporal_fixtures()`: that list is rebuilt per test and these objects are
#' comparatively expensive.
#'
#' @param n_locations Number of distinct locations
#' @param n_times Number of distinct timestamps
#' @noRd
make_size_sf <- function(n_locations, n_times, crs = 4326, x0 = 8, y0 = 50) {
	n_rows <- max(n_locations, n_times)

	# Square lattice: exactly n_locations distinct points, no modulo collisions.
	side <- ceiling(sqrt(n_locations))
	loc <- rep_len(seq_len(n_locations), n_rows)
	df <- data.frame(
		id = seq_len(n_rows),
		x = x0 + ((loc - 1L) %% side) * 0.01,
		y = y0 + ((loc - 1L) %/% side) * 0.01,
		stringsAsFactors = FALSE
	)

	times <- as.POSIXct("2022-02-05 00:00:00", tz = "UTC") +
		as.difftime(seq_len(n_times), units = "mins")
	df[[stemp_time_column()]] <- rep_len(times, n_rows)

	sf::st_as_sf(df, coords = c("x", "y"), crs = crs)
}

#' Prediction area for `make_size_sf()`, small on both axes
#'
#' Polygons, so the geographic size check (POINT/MULTIPOINT only) never fires
#' on the area; `n_steps` keeps the temporal side well under any limit.
#' @noRd
make_size_area <- function(n_steps = 8L, crs = 4326, x0 = 8, y0 = 50) {
	times <- as.POSIXct("2022-02-05 00:00:00", tz = "UTC") +
		as.difftime(round(seq(1, 12000, length.out = n_steps)), units = "mins")
	make_time_area(times, crs = crs, x0 = x0, y0 = y0)
}

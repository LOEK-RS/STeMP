#' Calculate Geodistance Classification
#'
#' @param samples_sf sf object of sample locations.
#' @param area_sf sf object of spatial area.
#' @return Character classification "random" or "clustered".
#' @noRd
calculate_geodist_classification <- function(samples_sf, area_sf) {
	samples_sf <- sf::st_transform(samples_sf, sf::st_crs(area_sf))

	dist_fun <- infer_distfun(samples_sf)
	geod <- CAST::geodist(samples_sf, modeldomain = area_sf, dist_fun = dist_fun)

	Gj <- geod[geod$what == "sample-to-sample", ]$dist
	Gij <- geod[geod$what == "prediction-to-sample", ]$dist

	testks <- suppressWarnings(stats::ks.test(Gj, Gij, alternative = "greater"))
	if (testks$p.value >= 0.05) "random" else "clustered"
}


#' Infer distance function from input.
#' Can be removed when PR #167 is merged in CAST
#'
#' @param x sf object.
#' @return Character dist_fun "great_circle" (longlat) or "euclidean" (projected coordinates).
#' @noRd
infer_distfun <- function(x) {
	islonglat <- if (is.na(sf::st_crs(x))) {
		FALSE
	} else {
		sf::st_is_longlat(sf::st_crs(x))
	}
	if (islonglat) {
		dist_fun <- "great_circle"
	} else {
		dist_fun <- "euclidean"
	}
}

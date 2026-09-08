#' Classify a geodist result as "random" or "clustered"
#'
#' @param geod A CAST::geodist data frame, or a reason string when the
#'   distances could not be computed.
#' @return "random", "clustered", or NULL.
#' @noRd
classify_geodist <- function(geod) {
	if (!inherits(geod, "data.frame")) {
		return(NULL)
	}

	Gj <- geod[geod$what == "sample-to-sample", ]$dist
	Gij <- geod[geod$what == "prediction-to-sample", ]$dist

	if (length(Gj) < 2 || length(Gij) < 2) {
		return(NULL)
	}

	testks <- suppressWarnings(stats::ks.test(Gj, Gij, alternative = "greater"))
	if (testks$p.value >= 0.05) "random" else "clustered"
}

calculate_geodist_classification <- function(samples_sf, area_sf) {
	classify_geodist(geodist_geographic_data(samples_sf, area_sf))
}

calculate_temporal_geodist_classification <- function(samples_sf, area_sf) {
	classify_geodist(geodist_temporal_data(samples_sf, area_sf))
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

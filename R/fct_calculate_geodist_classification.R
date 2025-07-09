#' Calculate Geodistance Classification
#'
#' @param samples_sf sf object of sample locations.
#' @param area_sf sf object of spatial area.
#' @return Character classification "random" or "clustered".
#' @noRd
calculate_geodist_classification <- function(samples_sf, area_sf) {
  samples_sf <- sf::st_transform(samples_sf, sf::st_crs(area_sf))
  geod <- CAST::geodist(samples_sf, modeldomain = area_sf)

  Gj <- geod[geod$what == "sample-to-sample", ]$dist
  Gij <- geod[geod$what == "prediction-to-sample", ]$dist

  testks <- suppressWarnings(stats::ks.test(Gj, Gij, alternative = "greater"))
  if (testks$p.value >= 0.05) "random" else "clustered"
}

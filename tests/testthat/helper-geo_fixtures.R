# Fixtures and inspection helpers for the geographic overview maps.

geo_points <- function(x, y, crs = 4326, ...) {
	sf::st_as_sf(data.frame(x = x, y = y, ...), coords = c("x", "y"), crs = crs)
}

geo_bbox_poly <- function(xmin, ymin, xmax, ymax, crs = 4326) {
	bb <- sf::st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), crs = crs)
	sf::st_sf(geometry = sf::st_as_sfc(bb))
}

# st_as_sf() on an empty data frame warns from min/max while computing a bbox
# over no points; st_sfc() short-circuits to an NA bbox instead.
geo_empty_points <- function(crs = 4326) sf::st_sf(geometry = sf::st_sfc(crs = crs))

# Wide enough for the coarse Natural Earth scale, and straddling the
# antimeridian region where the Fiji artefact appeared.
geo_global_points <- function() {
	geo_points(c(-170, -60, 20, 150, 100), c(60, -30, 10, -40, -5))
}

# On land, wide enough for context, narrow enough for the fine scale.
geo_regional_points <- function() geo_points(c(7, 9, 11), c(51, 52, 53))

# Below min_span_deg: context must decline.
geo_local_points <- function() geo_points(c(7.600, 7.601, 7.602), c(51.960, 51.961, 51.962))

# Mid-South-Atlantic: passes the span check, contains no land.
geo_ocean_points <- function() geo_points(c(-30, -25), c(-20, -15))

geo_counts <- function(n) {
	k <- length(n)
	geo_points(seq(5, 15, length.out = k), seq(48, 55, length.out = k), n = n)
}

geo_timestep_area <- function(n_steps) {
	steps <- as.POSIXct("2020-01-01", tz = "UTC") + (seq_len(n_steps) - 1) * 86400
	do.call(
		rbind,
		lapply(steps, function(s) {
			poly <- geo_bbox_poly(5, 50, 12, 55)
			poly$time <- s
			poly
		})
	)
}

#' Widest single polygon part, in degrees of longitude
#'
#' Feature-level bboxes are useless for the antimeridian question: Natural
#' Earth ships Russia and Fiji already split, so each *feature* legitimately
#' spans -180..180. The artefact is a single *part* that does. Antarctica is
#' excluded: it is one polygon closed along -90 and spans the full range by
#' construction.
widest_part <- function(g, exclude_below = -85) {
	parts <- suppressWarnings(sf::st_cast(sf::st_geometry(g), "POLYGON"))
	widths <- vapply(
		parts,
		function(p) {
			bb <- sf::st_bbox(p)
			if (bb[["ymin"]] <= exclude_below) 0 else as.numeric(bb[["xmax"]] - bb[["xmin"]])
		},
		numeric(1)
	)
	if (length(widths) == 0) 0 else max(widths)
}

clear_ne_cache <- function() {
	rm(list = ls(envir = .ne_state, all.names = TRUE), envir = .ne_state)
}

# .ne_state is process-global: a leaked cache, or a leaked failure flag, makes
# every later test pass silently against NULL.
local_ne_cache <- function(env = parent.frame()) {
	clear_ne_cache()
	withr::defer(clear_ne_cache(), envir = env)
}

local_ne_failure <- function(env = parent.frame()) {
	local_ne_cache(env)
	testthat::local_mocked_bindings(
		ne_countries = function(...) stop("no data package"),
		.package = "rnaturalearth",
		.env = env
	)
}

#' The Layer inside what a geom_ function returns
#'
#' geom_sf() returns list(<Layer>, <CoordSf default = TRUE>) rather than a bare
#' layer, so that a plot built from it alone still picks up a map projection.
#' Tests have to unwrap before inspecting `aes_params` or `data`.
as_layer <- function(x) {
	if (!is.list(x)) {
		return(x)
	}
	rest <- Filter(function(e) !inherits(e, "Coord"), x)
	if (length(rest) != 1) {
		stop("expected exactly one layer, found ", length(rest))
	}
	rest[[1]]
}

skip_if_no_naturalearth <- function() {
	testthat::skip_if_not_installed("rnaturalearth")
	testthat::skip_if_not_installed("rnaturalearthdata")
	testthat::skip_if_not_installed("testthat", "3.2.0")
}

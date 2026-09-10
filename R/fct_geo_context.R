# Offline geographic context for the overview maps using rnaturalearth.
#
# Every geometry operation here runs with s2 switched off. Natural Earth's
# polygons are distributed already split at the antimeridian, and that split
# survives only under planar semantics: with s2 enabled, st_make_valid()
# rebuilds the two halves of Fiji into one spherical polygon whose planar
# footprint spans the whole map, drawing as a band across a global figure.

.ne_state <- new.env(parent = emptyenv())

#' Evaluate an expression with spherical geometry switched off
#' @noRd
with_planar_s2 <- function(expr) {
	old <- suppressMessages(sf::sf_use_s2(FALSE))
	on.exit(suppressMessages(sf::sf_use_s2(old)), add = TRUE)
	force(expr)
}

#' Pad a bbox, clamping to the valid lon/lat domain where that applies
#' @noRd
pad_bbox <- function(bb, frac) {
	dx <- bb[["xmax"]] - bb[["xmin"]]
	dy <- bb[["ymax"]] - bb[["ymin"]]
	out <- c(
		xmin = bb[["xmin"]] - dx * frac,
		ymin = bb[["ymin"]] - dy * frac,
		xmax = bb[["xmax"]] + dx * frac,
		ymax = bb[["ymax"]] + dy * frac
	)

	if (isTRUE(sf::st_is_longlat(sf::st_crs(bb)))) {
		out[["xmin"]] <- max(out[["xmin"]], -180)
		out[["xmax"]] <- min(out[["xmax"]], 180)
		out[["ymin"]] <- max(out[["ymin"]], -89.9)
		out[["ymax"]] <- min(out[["ymax"]], 89.9)
	}

	sf::st_bbox(out, crs = sf::st_crs(bb))
}

#' Natural Earth land and boundaries, prepared once per process and scale
#'
#' @param scale "small" for wide extents, "medium" otherwise.
#'   A global panel is a few hundred pixels wide, so the finer geometry costs
#'   vertices and adds nothing visible.
#' @noRd
ne_context_data <- function(scale = c("small", "medium")) {
	scale <- match.arg(scale)
	key <- paste0("ne_", scale)

	if (!is.null(.ne_state[[key]])) {
		return(.ne_state[[key]])
	}
	if (isTRUE(.ne_state[[paste0(key, "_failed")]])) {
		return(NULL)
	}

	if (!requireNamespace("rnaturalearthdata", quietly = TRUE)) {
		.ne_state[[paste0(key, "_failed")]] <- TRUE
		return(NULL)
	}

	raw <- tryCatch(
		rnaturalearth::ne_countries(scale = scale, returnclass = "sf"),
		error = function(e) NULL
	)
	if (is.null(raw)) {
		.ne_state[[paste0(key, "_failed")]] <- TRUE
		return(NULL)
	}

	out <- with_planar_s2({
		land <- sf::st_sf(geometry = sf::st_geometry(raw))
		land <- suppressWarnings(sf::st_make_valid(land))
		list(
			land = land,
			lines = suppressWarnings(sf::st_boundary(land))
		)
	})

	.ne_state[[key]] <- out
	out
}

#' Data extent in lon/lat
#'
#' A projected bounding box is not a lon/lat rectangle -- its straight edges
#' are curves after transformation -- so the rectangle is densified before it
#' is transformed.
#' @noRd
context_bbox_lonlat <- function(x) {
	bb <- tryCatch(sf::st_bbox(x), error = function(e) NULL)
	if (is.null(bb) || !all(is.finite(as.numeric(bb)))) {
		return(NULL)
	}

	if (isTRUE(sf::st_is_longlat(x))) {
		return(bb)
	}

	span <- max(bb[["xmax"]] - bb[["xmin"]], bb[["ymax"]] - bb[["ymin"]])
	rect <- sf::st_as_sfc(bb)
	if (is.finite(span) && span > 0) {
		rect <- sf::st_segmentize(rect, dfMaxLength = span / 50)
	}

	out <- tryCatch(sf::st_bbox(sf::st_transform(rect, 4326)), error = function(e) NULL)
	if (is.null(out) || !all(is.finite(as.numeric(out)))) NULL else out
}

#' Panel limits for a map that carries a context backdrop
#'
#' Context layers train the scales like any other layer, so the panel is pinned
#' explicitly and the backdrop is cropped slightly wider than the pin.
#' @noRd
context_limits <- function(x) {
	bb <- tryCatch(sf::st_bbox(x), error = function(e) NULL)
	if (is.null(bb)) NULL else pad_bbox(bb, 0.05)
}

#' Background geographic context layers
#'
#' Returns NULL when the extent is too small for coastlines and borders to add anything,
#' or when the data carry no CRS.
#'
#' @param x An sf object whose extent the context should cover.
#' @param min_span_deg Below this extent Natural Earth has nothing to
#'   contribute, so nothing is drawn.
#' @noRd
context_layer <- function(x, min_span_deg = 0.5) {
	if (!inherits(x, "sf") || nrow(x) == 0 || is.na(sf::st_crs(x))) {
		return(NULL)
	}

	bb_ll <- context_bbox_lonlat(x)
	if (is.null(bb_ll)) {
		return(NULL)
	}

	span <- max(bb_ll[["xmax"]] - bb_ll[["xmin"]], bb_ll[["ymax"]] - bb_ll[["ymin"]])
	if (!is.finite(span) || span < min_span_deg) {
		return(NULL)
	}

	ne <- ne_context_data(if (span > 60) "small" else "medium")
	if (is.null(ne)) {
		return(NULL)
	}

	target <- sf::st_crs(x)
	crop_ll <- pad_bbox(bb_ll, 0.10)

	clip <- tryCatch(
		suppressMessages(with_planar_s2({
			land <- suppressWarnings(sf::st_crop(ne$land, crop_ll))
			lines <- suppressWarnings(sf::st_crop(ne$lines, crop_ll))

			if (target != sf::st_crs(4326)) {
				land <- sf::st_transform(land, target)
				lines <- sf::st_transform(lines, target)

				keep <- pad_bbox(sf::st_bbox(x), 0.10)
				land <- suppressWarnings(sf::st_crop(land, keep))
				lines <- suppressWarnings(sf::st_crop(lines, keep))
			}

			list(land = land, lines = lines)
		})),
		error = function(e) NULL
	)

	if (is.null(clip) || nrow(clip$land) == 0) {
		return(NULL)
	}

	list(
		ggplot2::geom_sf(data = clip$land, fill = "#f4f4f1", colour = NA),
		ggplot2::geom_sf(data = clip$lines, colour = "#c9d1d6", linewidth = 0.25)
	)
}

#' Data layer styled to stay legible on top of the context
#' @noRd
sf_overlay <- function(x, colour = "#d35400") {
	if (all(as.character(sf::st_geometry_type(x)) %in% c("POINT", "MULTIPOINT"))) {
		ggplot2::geom_sf(data = x, shape = 21, size = 1.6, stroke = 0.4, colour = "white", fill = colour)
	} else {
		ggplot2::geom_sf(data = x, fill = ggplot2::alpha(colour, 0.15), colour = colour, linewidth = 0.45)
	}
}

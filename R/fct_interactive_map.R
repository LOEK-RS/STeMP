#' Persist a widget for later embedding
#'
#' An RDS rather than standalone HTML: the report inlines the widget so that
#' knitr can deduplicate leaflet's JS and CSS across figures, and writing one
#' self-contained HTML per figure would cost a pandoc call on every re-render.
#'
#' @return The written path, or NULL on failure.
#' @noRd
save_widget <- function(widget, element_id, output_dir) {
	if (is.null(widget)) {
		return(invisible(NULL))
	}

	target <- file.path(output_dir, paste0(element_id, ".rds"))

	ok <- tryCatch(
		{
			saveRDS(widget, target)
			TRUE
		},
		error = function(e) FALSE
	)

	if (!ok) {
		return(invisible(NULL))
	}
	invisible(target)
}

#' Counterpart of `clear_figure()`
#' @noRd
clear_widget <- function(element_id, output_dir) {
	for (ext in c(".rds", ".html")) {
		f <- file.path(output_dir, paste0(element_id, ext))
		if (file.exists(f)) {
			unlink(f)
		}
	}
	invisible(NULL)
}

#' Write a stored widget out as one self-contained HTML file
#'
#' Only the ZIP needs this, and only at download time.
#'
#' @return The written path, or NULL on failure.
#' @noRd
export_widget_html <- function(element_id, output_dir) {
	src <- file.path(output_dir, paste0(element_id, ".rds"))
	if (!file.exists(src) || !rmarkdown::pandoc_available()) {
		return(invisible(NULL))
	}

	target <- file.path(output_dir, paste0(element_id, ".html"))

	ok <- tryCatch(
		{
			tmp_dir <- tempfile("stemp_widget_")
			dir.create(tmp_dir, recursive = TRUE)
			on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

			tmp <- file.path(tmp_dir, "widget.html")
			htmltools::save_html(htmltools::tagList(readRDS(src)), file = tmp, libdir = "lib")
			rmarkdown::pandoc_self_contained_html(tmp, target)
			TRUE
		},
		error = function(e) FALSE
	)

	if (!ok) invisible(NULL) else invisible(target)
}

#' Reproject for leaflet, or refuse
#'
#' Leaflet only speaks EPSG:4326, so data without a CRS cannot be shown
#' interactively; the caller falls back to the static ggplot.
#' @noRd
to_leaflet_crs <- function(x) {
	if (!inherits(x, "sf") || nrow(x) == 0 || is.na(sf::st_crs(x))) {
		return(NULL)
	}
	tryCatch(sf::st_transform(x, 4326), error = function(e) NULL)
}

#' Upper bound on features handed to the browser
#'
#' A 400,000-point layer freezes the tab. Mirrors `geodist_max_n()`.
#' @noRd
interactive_max_n <- function() {
	n <- tryCatch(get_golem_config("max_interactive_features"), error = function(e) NULL)
	if (!is.numeric(n) || length(n) != 1L || is.na(n)) 5000L else as.integer(n)
}

#' @noRd
#' @noRd
#' @noRd
leaflet_base <- function(offline_context = NULL) {
	# Provider choice is constrained by the exported report, not by the app:
	# OSM's volunteer servers reject requests without a Referer, and CARTO now
	# returns a watermarked "API KEY REQUIRED" tile. Esri's basemaps are keyless
	# and do not require a Referer.
	m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
	m <- leaflet::addProviderTiles(m, "Esri.WorldGrayCanvas", group = "Light")
	m <- leaflet::addProviderTiles(m, "Esri.WorldStreetMap", group = "Map")
	m <- leaflet::addProviderTiles(m, "Esri.WorldImagery", group = "Satellite")
	m <- leaflet::addProviderTiles(m, "Esri.WorldTopoMap", group = "Topographic")

	if (!is.null(offline_context)) {
		m <- leaflet::addPolygons(
			m,
			data = offline_context,
			group = "Offline",
			fill = TRUE,
			fillColor = "#f4f4f1",
			fillOpacity = 1,
			color = "#c9d1d6",
			weight = 0.6
		)
	}

	leaflet::addLayersControl(
		m,
		baseGroups = c("Light", "Map", "Satellite", "Topographic", if (!is.null(offline_context)) "Offline"),
		options = leaflet::layersControlOptions(collapsed = TRUE)
	)
}
#' Attribute table popup
#' @noRd
popup_table <- function(x, cols = NULL, max_cols = 10) {
	df <- sf::st_drop_geometry(x)
	if (!is.null(cols)) {
		df <- df[, intersect(cols, names(df)), drop = FALSE]
	}
	if (ncol(df) == 0) {
		return(NULL)
	}
	df <- df[, seq_len(min(ncol(df), max_cols)), drop = FALSE]

	labels <- sanitize_text(names(df))
	vapply(
		seq_len(nrow(df)),
		function(i) {
			vals <- sanitize_text(as.character(unlist(lapply(df[i, , drop = FALSE], as.character))))
			paste0(
				"<table class='stemp-popup'>",
				paste0("<tr><th>", labels, "</th><td>", vals, "</td></tr>", collapse = ""),
				"</table>"
			)
		},
		character(1)
	)
}

#' @noRd
add_sf_layer <- function(map, x, popup = NULL, colour = "#d35400", group = "Data") {
	types <- unique(as.character(sf::st_geometry_type(x)))

	if (all(types %in% c("POINT", "MULTIPOINT"))) {
		leaflet::addCircleMarkers(
			map,
			data = x,
			radius = 5,
			weight = 1,
			color = "white",
			fillColor = colour,
			fillOpacity = 0.9,
			popup = popup,
			group = group
		)
	} else if (all(types %in% c("LINESTRING", "MULTILINESTRING"))) {
		leaflet::addPolylines(map, data = x, color = colour, weight = 2, popup = popup, group = group)
	} else {
		leaflet::addPolygons(
			map,
			data = x,
			color = colour,
			weight = 1,
			fillColor = colour,
			fillOpacity = 0.25,
			popup = popup,
			group = group,
			highlightOptions = leaflet::highlightOptions(weight = 3, fillOpacity = 0.45, bringToFront = TRUE)
		)
	}
}

#' Plain layer map (training area, prediction area in spatial mode)
#' @noRd
geo_map_leaflet <- function(x) {
	x <- to_leaflet_crs(x)
	if (is.null(x)) {
		return(NULL)
	}
	if (all(sf::st_geometry_type(x) == "POINT")) {
		x <- unique_geometries(x)
	}
	if (nrow(x) > interactive_max_n()) {
		return(NULL) # caller keeps the static figure
	}

	leaflet_base(ne_offline_polygons(x)) |>
		add_sf_layer(x, popup = popup_table(x))
}

#' Sampling locations coloured by number of observations
#'
#' Interactive counterpart of `location_repetition_plot()`. Clicking a location
#' lists the timestamps it carries, which is what the static map can only
#' encode as a colour.
#' @noRd
sample_locations_leaflet <- function(samples_sf, temporal = FALSE) {
	counts <- count_sample_repetitions(samples_sf)
	counts <- to_leaflet_crs(counts)
	if (is.null(counts) || nrow(counts) > interactive_max_n()) {
		return(NULL)
	}

	times <- if (isTRUE(temporal)) parse_time_column(samples_sf) else NULL
	pal <- leaflet::colorNumeric("viridis", counts$n)

	popup <- if (is.null(times)) {
		paste0("<b>Observations:</b> ", counts$n)
	} else {
		key_all <- sf::st_as_binary(sf::st_geometry(sf::st_transform(samples_sf, 4326)), hex = TRUE)
		key_uni <- sf::st_as_binary(sf::st_geometry(counts), hex = TRUE)
		vapply(
			seq_len(nrow(counts)),
			function(i) {
				tt <- sort(unique(times[key_all == key_uni[i] & !is.na(times)]))
				shown <- format(utils::head(tt, 25))
				paste0(
					"<b>Observations:</b> ",
					counts$n[i],
					"<br/>",
					"<b>Time steps:</b> ",
					length(tt),
					"<br/>",
					"<div class='stemp-times'>",
					paste(shown, collapse = "<br/>"),
					"</div>",
					if (length(tt) > 25) sprintf("<em>\u2026 and %d more</em>", length(tt) - 25)
				)
			},
			character(1)
		)
	}

	leaflet_base(ne_offline_polygons(counts)) |>
		leaflet::addCircleMarkers(
			data = counts,
			radius = 5,
			weight = 1,
			color = "white",
			fillColor = ~ pal(n),
			fillOpacity = 0.9,
			popup = popup,
			group = "Sampling locations"
		) |>
		leaflet::addLegend("bottomright", pal = pal, values = counts$n, title = "Observations<br/>per location")
}

#' Interactive counterpart of the combined sampling-locations figure
#'
#' Returns the map alone when there is no usable time column or plotly is
#' unavailable, which mirrors the static function's fallback.
#' @noRd
sample_repetitions_widget <- function(samples_sf, temporal = TRUE) {
	m <- sample_locations_leaflet(samples_sf, temporal = temporal)
	if (is.null(m)) {
		return(NULL)
	}

	counts <- if (isTRUE(temporal)) count_time_repetitions(samples_sf) else NULL
	if (is.null(counts) || !requireNamespace("plotly", quietly = TRUE)) {
		return(m)
	}

	htmltools::tagList(m, plotly::ggplotly(time_frequency_plot(counts)))
}

#' Prediction domain in ONE panel, time carried as a clickable attribute
#'
#' Replaces `geo_map_timesteps_plot()`'s facet grid in interactive mode:
#' geometries are deduplicated, and each feature's popup reports how many time
#' steps it is applied at, the covered span, the median spacing and the stamps
#' themselves.
#' @noRd
prediction_domain_leaflet <- function(area_sf, max_listed = 25) {
	x <- to_leaflet_crs(area_sf)
	if (is.null(x)) {
		return(NULL)
	}

	times <- parse_time_column(area_sf)

	if (is.null(times)) {
		x <- unique_geometries(x)
		if (nrow(x) > interactive_max_n()) {
			return(NULL)
		}
		return(leaflet_base(ne_offline_polygons(x)) |> add_sf_layer(x, popup = popup_table(x)))
	}

	key <- sf::st_as_binary(sf::st_geometry(x), hex = TRUE)
	keep <- !duplicated(key)
	geom <- sf::st_geometry(x)[keep]

	if (length(geom) > interactive_max_n()) {
		return(NULL)
	}

	steps <- lapply(key[keep], function(k) sort(unique(times[key == k & !is.na(times)])))

	feat <- sf::st_sf(
		n_timesteps = lengths(steps),
		geometry = geom
	)

	popup <- vapply(
		steps,
		function(tt) {
			shown <- format(utils::head(tt, max_listed))
			paste0(
				"<b>Prediction domain</b><br/>",
				"<b>Time steps:</b> ",
				length(tt),
				"<br/>",
				"<b>Extent:</b> ",
				format_time_extent(tt) %||% "\u2014",
				"<br/>",
				"<b>Resolution:</b> ",
				format_time_resolution(tt) %||% "\u2014",
				"<div class='stemp-times'>",
				paste(shown, collapse = "<br/>"),
				"</div>",
				if (length(tt) > max_listed) sprintf("<em>\u2026 and %d more</em>", length(tt) - max_listed)
			)
		},
		character(1)
	)

	leaflet_base(ne_offline_polygons(feat)) |>
		add_sf_layer(feat, popup = popup, group = "Prediction domain") |>
		leaflet::addLabelOnlyMarkers(
			data = sf::st_point_on_surface(sf::st_geometry(feat)),
			label = paste0(feat$n_timesteps, " time steps"),
			labelOptions = leaflet::labelOptions(noHide = FALSE, direction = "auto")
		)
}

#' Natural Earth land clipped to a layer's extent, for the leaflet "Offline" base
#' @noRd
ne_offline_polygons <- function(x) {
	bb <- context_bbox_lonlat(x)
	if (is.null(bb)) {
		return(NULL)
	}
	span <- max(bb[["xmax"]] - bb[["xmin"]], bb[["ymax"]] - bb[["ymin"]])
	ne <- ne_context_data(if (span > 60) "small" else "medium")
	if (is.null(ne)) {
		return(NULL)
	}
	tryCatch(
		with_planar_s2(suppressWarnings(sf::st_crop(ne$land, pad_bbox(bb, 0.10)))),
		error = function(e) NULL
	)
}

#' @noRd
geodist_widget <- function(p_geo, p_time) {
	if (!requireNamespace("plotly", quietly = TRUE)) {
		return(NULL)
	}
	if (is.null(p_time)) {
		return(plotly::ggplotly(p_geo))
	}
	if (is.null(p_geo)) {
		return(plotly::ggplotly(p_time))
	}
	plotly::subplot(
		plotly::ggplotly(p_geo + ggplot2::ggtitle("Geographic space")),
		plotly::ggplotly(p_time + ggplot2::ggtitle("Temporal space")),
		nrows = 1,
		shareY = TRUE,
		titleX = TRUE
	)
}

#' Assign both output slots for one figure element
#'
#' `build` is a function returning either NULL (nothing to draw) or a list with
#' `static` (a ggplot), optionally `widget`, `width` and `height`.
#'
#' The artefacts are written from the `_plot_ui` renderer rather than from
#' `renderPlot()`, so a browser resize no longer rewrites the PNG. Both slots
#' are plain outputs, so re-invoking this function replaces the previous renderers.
#' @noRd
emit_figure <- function(output, element_id, output_dir, ns, interactive, build) {
	interactive <- isTRUE(interactive)
	fig <- shiny::reactive(build())

	output[[element_id]] <- shiny::renderPlot({
		res <- fig()
		shiny::req(res, res$static)
		res$static
	})

	output[[paste0(element_id, "_plot_ui")]] <- shiny::renderUI({
		res <- fig()

		if (is.null(res) || is.null(res$static)) {
			clear_figure(element_id, output_dir)
			clear_widget(element_id, output_dir)
			return(NULL)
		}

		save_figure(
			res$static,
			element_id,
			output_dir,
			width = res$width %||% 7,
			height = res$height %||% 5
		)

		if (isTRUE(interactive) && !is.null(res$widget)) {
			save_widget(res$widget, element_id, output_dir)
			return(htmltools::tagList(res$widget))
		}

		clear_widget(element_id, output_dir)
		shiny::plotOutput(outputId = ns(element_id), height = "300px")
	})

	invisible(NULL)
}

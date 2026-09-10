#' Build the overview map for one sf layer
#' @noRd
geo_map_plot <- function(x) {
	# Repeated observations at one location would otherwise overplot
	if (all(sf::st_geometry_type(x) == "POINT")) {
		x <- unique_geometries(x)
	}

	ctx <- context_layer(x)
	ggplot2::ggplot() +
		ctx +
		sf_overlay(x) +
		map_axis_style(1, limits = if (is.null(ctx)) NULL else context_limits(x))
}

#' Render a geographical map plot to Output
#'
#' @param output Shiny output object.
#' @param element_id Output ID for the plot.
#' @param geo_metadata Reactive list containing spatial data.
#' @param what Character specifying which spatial data to use.
#' @noRd
geo_map <- function(
	output,
	element_id,
	geo_metadata = NULL,
	what = c("samples_sf", "training_area_sf", "prediction_area_sf"),
	output_dir
) {
	what <- match.arg(what)

	output[[element_id]] <- shiny::renderPlot({
		d <- tryCatch(geo_metadata[[what]](), error = function(e) NULL)
		if (is.null(d) || !inherits(d, "sf") || nrow(d) == 0) {
			return(NULL)
		}

		p <- geo_map_plot(d)
		save_figure(p, element_id, output_dir)
		p
	})
}

#' Map of Distinct Locations Coloured by Observation Count
#'
#' Few distinct counts become discrete classes, a heavily skewed range a log
#' scale, anything else a linear scale with integer breaks.
#'
#' @param counts sf object as returned by `count_sample_repetitions()`.
#' @noRd
location_repetition_plot <- function(counts) {
	colour_value <- NULL # silence R CMD check on the aes() NSE

	legend_name <- "Observations per location"
	observed <- sort(unique(counts$n))

	if (length(observed) <= 8) {
		counts$colour_value <- factor(counts$n, levels = observed)
		colour_scale <- ggplot2::scale_colour_viridis_d(name = legend_name)
	} else if (max(observed) / min(observed) > 50) {
		counts$colour_value <- counts$n
		colour_scale <- ggplot2::scale_colour_viridis_c(name = legend_name, trans = "log10")
	} else {
		counts$colour_value <- counts$n
		colour_scale <- ggplot2::scale_colour_viridis_c(name = legend_name, breaks = integer_breaks())
	}

	ctx <- context_layer(counts)
	ggplot2::ggplot() +
		ctx +
		ggplot2::geom_sf(data = counts, ggplot2::aes(colour = colour_value), size = 1.6) +
		colour_scale +
		map_axis_style(1, limits = if (is.null(ctx)) NULL else context_limits(counts)) +
		ggplot2::theme(
			legend.position = "bottom",
			legend.title.position = "top",
			legend.key.height = ggplot2::unit(2, "mm")
		)
}

#' Frequency of Observations over Time
#'
#' Temporal counterpart of `location_repetition_plot()`: bar height is the
#' number of observations at a time stamp.
#'
#' @param counts Data frame as returned by `count_time_repetitions()`.
#' @noRd
time_frequency_plot <- function(counts) {
	time <- n_obs <- NULL # silence R CMD check on the aes() NSE

	step <- if (nrow(counts) > 1) stats::median(diff(as.numeric(counts$time))) else 86400
	bar_width <- max(step * 0.8, 1)

	ggplot2::ggplot(counts, ggplot2::aes(x = time, y = n_obs)) +
		# mid-viridis, so the bars share the map's palette family
		ggplot2::geom_col(width = bar_width, fill = "#2C728E") +
		ggplot2::scale_y_continuous(breaks = integer_breaks()) +
		ggplot2::labs(x = NULL, y = "Observations") +
		ggplot2::theme_minimal() +
		ggplot2::theme(
			axis.text = ggplot2::element_text(size = 8),
			axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
			panel.grid.major.x = ggplot2::element_line(linewidth = 0.2),
			plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 12)
		)
}

#' Render Sampling Locations and their Temporal Frequency
#'
#' Spatio-Temporal counterpart of `geo_map()`. The map shows distinct locations
#' coloured by how many observations they carry; the companion panel shows how
#' many observations fall on each time stamp. The two are combined into a single
#' image so that the preview, report and ZIP pipelines still see one PNG per
#' element ID.
#'
#' @param output Shiny output object.
#' @param element_id Output ID for the plot (also the PNG stem).
#' @param geo_metadata Reactive list containing spatial data.
#' @param output_dir Temporary output directory.
#' @param temporal Logical; render the frequency panel alongside the map. The
#'   caller only reaches this renderer in Spatio-Temporal mode, so the panel is
#'   dropped only when the samples carry no usable `time` column.
#' @noRd
geo_map_repetitions <- function(
	output,
	element_id,
	geo_metadata = NULL,
	output_dir,
	temporal = TRUE
) {
	output[[element_id]] <- shiny::renderPlot({
		samples_data <- tryCatch(geo_metadata$samples_sf(), error = function(e) NULL)

		if (is.null(samples_data) || !inherits(samples_data, "sf") || nrow(samples_data) == 0) {
			return(NULL)
		}

		counts <- count_sample_repetitions(samples_data)
		if (is.null(counts)) {
			return(NULL)
		}

		p_map <- location_repetition_plot(counts) +
			ggplot2::ggtitle("Sampling locations")

		time_counts <- if (isTRUE(temporal)) count_time_repetitions(samples_data) else NULL

		# No parseable timestamps: keep the map alone, as in spatial mode
		if (is.null(time_counts)) {
			save_figure(p_map, element_id, output_dir)
			return(p_map)
		}

		aspect <- map_panel_aspect(counts) %||% 0.8

		p_freq <- time_frequency_plot(time_counts) +
			ggplot2::ggtitle("Observations over time") +
			ggplot2::theme(aspect.ratio = aspect)

		legend <- cowplot::get_legend(p_map)

		panels <- cowplot::plot_grid(
			p_map + ggplot2::theme(legend.position = "none"),
			p_freq,
			ncol = 2
		)

		p <- if (is.null(legend)) {
			panels
		} else {
			cowplot::plot_grid(
				panels,
				cowplot::plot_grid(legend, NULL, ncol = 2),
				ncol = 1,
				rel_heights = c(1, 0.14)
			)
		}

		fig_width <- 9
		panel_width <- fig_width / 2 - 0.9
		fig_height <- min(max(panel_width * aspect + 1.9, 3.4), 8)

		save_figure(p, element_id, output_dir, width = fig_width, height = fig_height)
		p
	})
}

#' Build the faceted prediction-domain map
#'
#' @param area_data sf object of the prediction domain, one set of features per
#'   time step.
#' @param times POSIXct vector as returned by `parse_time_column(area_data)`,
#'   aligned row-for-row with `area_data`.
#' @param max_facets Facets beyond this are dropped, keeping an evenly spaced
#'   subset. Long daily series would otherwise produce an unreadable grid and a
#'   very large PNG.
#' @return A list of the plot and the canvas size it needs.
#' @noRd
geo_map_timesteps_plot <- function(area_data, times, max_facets = 9) {
	all_steps <- sort(unique(times))

	subtitle <- NULL
	keep_steps <- all_steps
	if (length(all_steps) > max_facets) {
		keep_steps <- all_steps[round(seq(1, length(all_steps), length.out = max_facets))]
		subtitle <- sprintf("Showing %d of %d time steps", max_facets, length(all_steps))
	}

	keep <- times %in% keep_steps
	area_data <- area_data[keep, , drop = FALSE]
	area_data$time_step <- factor(format(times[keep]), levels = format(keep_steps))

	n_facets <- nlevels(area_data$time_step)
	n_col <- min(3L, ceiling(sqrt(n_facets)))
	n_row <- ceiling(n_facets / n_col)

	ctx <- context_layer(area_data)
	p <- ggplot2::ggplot() +
		ctx +
		ggplot2::geom_sf(data = area_data) +
		ggplot2::facet_wrap(~time_step, ncol = n_col) +
		ggplot2::labs(subtitle = subtitle) +
		map_axis_style(n_col, limits = if (is.null(ctx)) NULL else context_limits(area_data))

	width <- 7
	list(
		plot = p,
		width = width,
		height = min(width * (n_row / n_col) + 0.6, 1.25 * width)
	)
}

#' Render the Prediction Domain, One Facet per Time Step
#'
#' @param output Shiny output object.
#' @param element_id Output ID for the plot (also the PNG stem).
#' @param geo_metadata Reactive list containing spatial data.
#' @param output_dir Temporary output directory.
#' @param max_facets Passed to `geo_map_timesteps_plot()`.
#' @noRd
geo_map_timesteps <- function(output, element_id, geo_metadata = NULL, output_dir, max_facets = 9) {
	output[[element_id]] <- shiny::renderPlot({
		area_data <- tryCatch(geo_metadata$prediction_area_sf(), error = function(e) NULL)

		if (is.null(area_data) || !inherits(area_data, "sf") || nrow(area_data) == 0) {
			return(NULL)
		}

		times <- parse_time_column(area_data)
		if (is.null(times)) {
			return(NULL)
		}

		res <- geo_map_timesteps_plot(area_data, times, max_facets = max_facets)

		save_figure(res$plot, element_id, output_dir, width = res$width, height = res$height)
		res$plot
	})
}

#' Graticule Breaks Placed Inside the Panel
#'
#' `n.breaks` only hints at a count and routinely returns values outside the
#' panel, which `coord_sf()` drops together with their labels. Placing breaks
#' at fixed fractions of the range guarantees they are drawn.
#'
#' @param n Number of breaks per axis.
#' @param pad Fraction of the range kept clear at each end, so a label is not
#'   half off the panel edge.
#' @noRd
interior_breaks <- function(n = 2, pad = 0.15) {
	function(limits) {
		limits <- range(limits, na.rm = TRUE)

		if (!all(is.finite(limits)) || diff(limits) <= 0) {
			return(numeric(0))
		}

		inset <- diff(limits) * pad
		seq(limits[1] + inset, limits[2] - inset, length.out = n)
	}
}

#' Short Coordinate Labels
#'
#' Digits follow the extent: a continental map gets whole degrees, a field
#' site gets three decimals. Returns one label per break, including empties
#' for any NA the scale passes through.
#'
#' @noRd
map_axis_labels <- function(x) {
	out <- rep("", length(x))
	finite <- is.finite(x)

	if (!any(finite)) {
		return(out)
	}

	span <- diff(range(x[finite]))
	digits <- if (span <= 0) 2L else max(0L, as.integer(ceiling(-log10(span))) + 1L)
	out[finite] <- format(round(x[finite], digits), trim = TRUE)
	out
}

#' Axis Styling for Map Panels
#'
#' Panel width is held constant by capping the column count, so the label size
#' does not vary with the number of panels. It is set for legibility after the
#' HTML preview scales the PNG down to the report column width.
#'
#' @noRd
map_axis_theme <- function() {
	ggplot2::theme(
		axis.text = ggplot2::element_text(size = 8),
		axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
		panel.grid.major = ggplot2::element_line(linewidth = 0.2),
		plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 12)
	)
}

#' Shared Axis Treatment for Every Geographic Plot
#'
#' @param n_col Number of facet columns. Panels get narrower as this rises, so
#'   it drives the break count; one column means an unfaceted plot.
#' @param datum CRS for the graticule. Omit to label in degrees regardless of
#'   the data CRS; pass `sf::st_crs(data)` to label in the data's own units.
#' @noRd
#' @param limits Optional bbox in the data CRS. Context layers train the scales
#'   like any other layer, so a backdrop cropped wider than the data would
#'   widen the panel unless the frame is pinned here. `expand = FALSE` because
#'   the padding is already in the bbox.
map_axis_style <- function(n_col = 1, datum = NULL, limits = NULL) {
	n <- if (n_col > 1) 2 else 3

	coord_args <- list(label_graticule = "SW")
	if (!is.null(datum)) {
		coord_args$datum <- datum
	}
	if (!is.null(limits)) {
		coord_args$xlim <- c(limits[["xmin"]], limits[["xmax"]])
		coord_args$ylim <- c(limits[["ymin"]], limits[["ymax"]])
		coord_args$expand <- FALSE
	}

	list(
		do.call(ggplot2::coord_sf, coord_args),
		ggplot2::scale_x_continuous(breaks = interior_breaks(n), labels = map_axis_labels),
		ggplot2::scale_y_continuous(breaks = interior_breaks(n), labels = map_axis_labels),
		ggplot2::theme_minimal(),
		map_axis_theme()
	)
}

#' @noRd
integer_breaks <- function(n = 5) {
	function(limits) {
		brk <- pretty(limits, n = n)
		brk[brk == as.integer(brk) & brk >= 1]
	}
}

#' Aspect Ratio of a Map Panel, as Height / Width
#'
#' `coord_sf()` pins the map panel's shape to the data extent, so the panel
#' cannot be stretched to match a neighbour. Rather than distort the map, the
#' companion panel is given the same ratio; equal panel widths then produce
#' equal panel heights.
#'
#' @param x An sf object.
#' @param limits Clamp, so that a very elongated extent does not force an
#'   unreadable bar chart.
#' @return A ratio, or NULL for a degenerate extent.
#' @noRd
map_panel_aspect <- function(x, limits = c(0.45, 1.6)) {
	bb <- tryCatch(sf::st_bbox(x), error = function(e) NULL)
	if (is.null(bb)) {
		return(NULL)
	}

	# Both scales are expanded by 5% per side, which cancels in the ratio,
	# so only the raw extent matters.
	dx <- as.numeric(bb["xmax"] - bb["xmin"])
	dy <- as.numeric(bb["ymax"] - bb["ymin"])

	# coord_sf() scales longitude by the cosine of the mid-latitude
	if (isTRUE(sf::st_is_longlat(x))) {
		dx <- dx * cos(mean(c(bb["ymin"], bb["ymax"])) * pi / 180)
	}

	if (!is.finite(dx) || !is.finite(dy) || dx <= 0 || dy <= 0) {
		return(NULL)
	}

	max(limits[1], min(limits[2], dy / dx))
}

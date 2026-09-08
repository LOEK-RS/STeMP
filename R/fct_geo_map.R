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
		samples_data <- tryCatch(
			geo_metadata[[what]](),
			error = function(e) NULL
		)

		# Skip plot if no data
		if (is.null(samples_data) || !inherits(samples_data, "sf") || nrow(samples_data) == 0) {
			return(NULL)
		}

		# Remove duplicated geometries
		if (all(sf::st_geometry_type(samples_data) == "POINT")) {
			samples_data <- unique_geometries(samples_data)
		}

		p <- ggplot2::ggplot() +
			ggplot2::geom_sf(data = samples_data) +
			map_axis_style(1)

		save_figure(p, element_id, output_dir)
		p
	})
}


#' Render a Map of Sample Locations Coloured by Repetition Count
#'
#' Spatio-Temporal counterpart of geo_map(): distinct locations, coloured by how many
#' observations they carry.
#'
#' @param output Shiny output object.
#' @param element_id Output ID for the plot (also the PNG stem).
#' @param geo_metadata Reactive list containing spatial data.
#' @param output_dir temporary output directory
#' @noRd
geo_map_repetitions <- function(output, element_id, geo_metadata = NULL, output_dir) {
	n <- n_class <- NULL # silence R CMD check on the aes() NSE

	output[[element_id]] <- shiny::renderPlot({
		samples_data <- tryCatch(geo_metadata$samples_sf(), error = function(e) NULL)

		if (is.null(samples_data) || !inherits(samples_data, "sf") || nrow(samples_data) == 0) {
			return(NULL)
		}

		counts <- count_sample_repetitions(samples_data)
		if (is.null(counts)) {
			return(NULL)
		}

		observed <- sort(unique(counts$n))
		legend_name <- "Observations\nper location"

		if (length(observed) <= 8) {
			counts$n_class <- factor(counts$n, levels = observed)
			mapping <- ggplot2::aes(colour = n_class)
			scale <- ggplot2::scale_colour_viridis_d(name = legend_name)
		} else if (max(observed) / min(observed) > 50) {
			mapping <- ggplot2::aes(colour = n)
			scale <- ggplot2::scale_colour_viridis_c(name = legend_name, trans = "log10")
		} else {
			mapping <- ggplot2::aes(colour = n)
			scale <- ggplot2::scale_colour_viridis_c(name = legend_name, breaks = integer_breaks())
		}

		p <- ggplot2::ggplot() +
			ggplot2::geom_sf(data = counts, mapping) +
			scale +
			map_axis_style(1)

		save_figure(p, element_id, output_dir)
		p
	})
}


#' Render the Prediction Domain, One Facet per Time Step
#'
#' @param max_facets Facets beyond this are dropped, keeping an evenly spaced
#'   subset. Long daily series would otherwise produce an unreadable grid and a
#'   very large PNG.
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

		all_steps <- sort(unique(times))

		subtitle <- NULL
		keep_steps <- all_steps
		if (length(all_steps) > max_facets) {
			keep_steps <- all_steps[round(seq(1, length(all_steps), length.out = max_facets))]
			subtitle <- sprintf("Showing %d of %d time steps", max_facets, length(all_steps))
		}

		area_data <- area_data[times %in% keep_steps, , drop = FALSE]
		area_data$time_step <- factor(format(times[times %in% keep_steps]), levels = format(keep_steps))

		n_facets <- nlevels(area_data$time_step)
		n_col <- min(3L, ceiling(sqrt(n_facets)))
		n_row <- ceiling(n_facets / n_col)

		fig_width <- 7
		fig_height <- min(fig_width * (n_row / n_col) + 0.6, 1.25 * fig_width)

		p <- ggplot2::ggplot(area_data) +
			ggplot2::geom_sf() +
			ggplot2::facet_wrap(~time_step, ncol = n_col) +
			ggplot2::labs(subtitle = subtitle) +
			map_axis_style(n_col)

		save_figure(p, element_id, output_dir, width = fig_width, height = fig_height)
		p
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
map_axis_style <- function(n_col = 1, datum = NULL) {
	n <- if (n_col > 1) 2 else 3

	coord <- if (is.null(datum)) {
		ggplot2::coord_sf(label_graticule = "SW")
	} else {
		ggplot2::coord_sf(label_graticule = "SW", datum = datum)
	}

	list(
		coord,
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

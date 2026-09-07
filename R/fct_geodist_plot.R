#' Render Geodistance Plot(s) to Output
#'
#' In spatial mode a single geographic plot is written. In spatio-temporal mode
#' the geographic and temporal plots are combined into one image, so that the
#' download, report and ZIP pipelines still see exactly one PNG per element ID.
#  @param output Shiny output object.
#' @param element_id Output ID for the plot.
#' @param geo_metadata Reactive list containing spatial data.
#' @param objective Character indicating which area to use.
#' @param output_dir temporary output directory
#' @param temporal Logical; render the temporal panel alongside the geographic one
#' @noRd
geodist_plot <- function(
	output,
	element_id,
	geo_metadata = NULL,
	objective = c("Model and prediction", "Model only"),
	output_dir,
	temporal = FALSE
) {
	objective <- match.arg(objective)
	temporal <- isTRUE(temporal)
	what <- if (objective == "Model and prediction") "prediction_area_sf" else "training_area_sf"

	output[[element_id]] <- shiny::renderPlot({
		samples_data <- tryCatch(geo_metadata$samples_sf(), error = function(e) NULL)
		area_data <- tryCatch(geo_metadata[[what]](), error = function(e) NULL)

		if (is.null(samples_data) || is.null(area_data) || !inherits(samples_data, "sf") || !inherits(area_data, "sf")) {
			return(NULL)
		}

		geod_geo <- geodist_geographic_data(samples_data, area_data)

		if (!inherits(geod_geo, "data.frame")) {
			clear_figure(element_id, output_dir)
			set_plot_visible(element_id, FALSE)
			return(NULL)
		}

		set_plot_visible(element_id, TRUE)
		p_geo <- add_log_scale_if_needed(plot(geod_geo), geod_geo)

		if (!temporal) {
			p <- p_geo + ggplot2::theme(aspect.ratio = 0.8)
			save_figure(p, element_id, output_dir)
			return(p)
		}

		geod_time <- geodist_temporal_data(samples_data, area_data)

		if (!inherits(geod_time, "data.frame")) {
			# Geographic panel is still valid; the temporal refusal is reported
			# through the warnings module.
			p <- p_geo + ggplot2::theme(aspect.ratio = 0.8)
			save_figure(p, element_id, output_dir)
			return(p)
		}

		p_time <- add_log_scale_if_needed(plot(geod_time), geod_time)

		legend <- cowplot::get_legend(p_time)

		body <- cowplot::plot_grid(
			p_geo +
				ggplot2::ggtitle("Geographic space") +
				ggplot2::theme(legend.position = "none"),
			p_time +
				ggplot2::ggtitle("Temporal space") +
				ggplot2::theme(legend.position = "none", axis.title.y = ggplot2::element_blank()),
			ncol = 2,
			align = "h",
			axis = "tr"
		)

		p <- if (is.null(legend)) {
			body
		} else {
			cowplot::plot_grid(body, legend, ncol = 1, rel_heights = c(1, NULL, 0.2))
		}

		save_figure(p, element_id, output_dir, width = 8, height = 4)
		p
	})
}


#' Geographic geodistance for one samples / area pair
#'
#' Geometries are deduplicated first: with repeated observations at the same
#' location the sample-to-sample distribution would otherwise collapse onto zero.
#' @noRd
geodist_geographic_data <- function(samples_sf, area_sf, max_n = geodist_max_n()) {
	samples_geo <- unique_geometries(samples_sf)
	area_geo <- unique_geometries(area_sf)

	if (nrow(samples_geo) < 2 || nrow(area_geo) < 1) {
		# one polygon area is valid
		return("Not enough distinct locations to compute distance distributions.")
	}

	reason <- geodist_size_reason(samples_geo, max_n, "sample locations")
	if (is.null(reason) && all(sf::st_geometry_type(area_geo) %in% c("POINT", "MULTIPOINT"))) {
		reason <- geodist_size_reason(area_geo, max_n, "prediction locations")
	}
	if (!is.null(reason)) {
		return(reason)
	}

	samples_geo <- sf::st_transform(samples_geo, sf::st_crs(area_geo))
	set.seed(100)
	CAST::geodist(
		samples_geo,
		modeldomain = area_geo,
		dist_fun = infer_distfun(samples_geo)
	)
}

#' Temporal geodistance for one samples / area pair
#'
#' Returns NULL when either side lacks at least two parseable timestamps.
#' @noRd
geodist_temporal_data <- function(samples_sf, area_sf, max_n = geodist_max_n()) {
	samples_time <- coerce_time_column(samples_sf)
	area_time <- coerce_time_column(area_sf)

	if (is.null(samples_time) || is.null(area_time)) {
		return("No usable 'time' column found.")
	}

	samples_time <- unique_times(samples_time)
	area_time <- unique_times(area_time)

	if (nrow(samples_time) < 2 || nrow(area_time) < 2) {
		return("Not enough distinct timestamps to compute distance distributions.")
	}

	reason <- geodist_size_reason(samples_time, max_n, "sample timestamps")
	if (is.null(reason)) {
		reason <- geodist_size_reason(area_time, max_n, "prediction timestamps")
	}
	if (!is.null(reason)) {
		return(reason)
	}

	set.seed(100)
	CAST::geodist(
		samples_time,
		preddata = area_time,
		dist_space = "time",
		time_var = stemp_time_column()
	)
}

#' Maximum feature count CAST::geodist can handle
#'
#' CAST computes nearest-neighbour distances from a full N x N matrix, so
#' memory grows as N^2: 10,000 features need ~800 MB, 419,000 need ~1.3 TB.
#' @noRd
geodist_max_n <- function() {
	n <- tryCatch(get_golem_config("max_geodist_n"), error = function(e) NULL)
	if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 2) 10000L else as.integer(n)
}

#' Refuse a geodist calculation that would exhaust memory
#'
#' Returns NULL when the layer fits, or a reason string when it does not.
#' @noRd
geodist_size_reason <- function(x, max_n, what) {
	if (is.null(x) || nrow(x) <= max_n) {
		return(NULL)
	}
	sprintf(
		"Too many %s (%s distinct, limit %s) to compute distance distributions.",
		what,
		format(nrow(x), big.mark = ","),
		format(max_n, big.mark = ",")
	)
}

#' Remove a previously written figure
#'
#' A refused calculation must not leave the PNG from an earlier upload on
#' disk: the report and ZIP pipelines pick up whatever file is there.
#' @noRd
clear_figure <- function(element_id, output_dir) {
	f <- file.path(output_dir, paste0(element_id, ".png"))
	if (file.exists(f)) {
		unlink(f)
	}
	invisible(NULL)
}

#' Show or hide a plot's container
#'
#' renderPlot() returning NULL still leaves an empty panel, so the wrapper
#' created by render_plot() is hidden outright.
#' @noRd
set_plot_visible <- function(element_id, visible) {
	domain <- shiny::getDefaultReactiveDomain()
	if (is.null(domain)) {
		return(invisible(NULL))
	}
	sel <- paste0("#", domain$ns(paste0(element_id, "_field")))
	if (isTRUE(visible)) {
		shinyjs::removeClass(selector = sel, class = "hide_plot_field")
	} else {
		shinyjs::addClass(selector = sel, class = "hide_plot_field")
	}
	invisible(NULL)
}

#' Apply a log x-scale when the two distributions are orders of magnitude apart
#' @noRd
add_log_scale_if_needed <- function(p, geod) {
	dist_samples <- geod[geod$what == "sample-to-sample", ]$dist
	dist_pred_samples <- geod[geod$what == "prediction-to-sample", ]$dist

	medians <- c(stats::median(dist_samples, na.rm = TRUE), stats::median(dist_pred_samples, na.rm = TRUE))
	if (any(!is.finite(medians)) || any(medians <= 0)) {
		return(p)
	}

	# 10x difference between distribution medians
	if (abs(log10(medians[1]) - log10(medians[2])) >= 1) {
		p <- p + ggplot2::scale_x_log10()
	}
	p
}

#' @noRd
coerce_time_column <- function(x, min_rows = 2) {
	parsed <- parse_time_column(x)
	if (is.null(parsed)) {
		return(NULL)
	}

	keep <- !is.na(parsed)
	if (sum(keep) < min_rows) {
		return(NULL)
	}

	out <- x[keep, , drop = FALSE]
	out[[stemp_time_column()]] <- parsed[keep]
	out
}

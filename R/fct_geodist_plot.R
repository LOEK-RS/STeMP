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
		p_geo <- add_log_scale_if_needed(plot(geod_geo), geod_geo)

		if (!temporal) {
			p <- p_geo + ggplot2::theme(aspect.ratio = 0.8)
			save_figure(p, element_id, output_dir)
			return(p)
		}

		geod_time <- geodist_temporal_data(samples_data, area_data)

		if (is.null(geod_time)) {
			p <- p_geo +
				ggplot2::theme(aspect.ratio = 0.8) +
				ggplot2::labs(caption = "No usable 'time' column found, temporal panel omitted.")
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
geodist_geographic_data <- function(samples_sf, area_sf) {
	samples_sf <- sf::st_transform(samples_sf, sf::st_crs(area_sf))

	samples_geo <- unique_geometries(samples_sf)
	area_geo <- unique_geometries(area_sf)

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
geodist_temporal_data <- function(samples_sf, area_sf) {
	samples_time <- coerce_time_column(samples_sf)
	area_time <- coerce_time_column(area_sf)

	if (is.null(samples_time) || is.null(area_time)) {
		return(NULL)
	}

	CAST::geodist(
		samples_time,
		preddata = area_time,
		dist_space = "time",
		time_var = stemp_time_column()
	)
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

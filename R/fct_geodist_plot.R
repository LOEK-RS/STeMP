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
		p_geo <- plot(geod_geo) + ggplot2::theme(aspect.ratio = 0.8)
		p_geo <- add_log_scale_if_needed(p_geo, geod_geo)

		if (!isTRUE(temporal)) {
			save_figure(p_geo, element_id, output_dir)
			return(p_geo)
		}

		geod_time <- geodist_temporal_data(samples_data, area_data)

		if (is.null(geod_time)) {
			p <- p_geo +
				ggplot2::labs(caption = "No usable 'time' column found - temporal panel omitted.")
			save_figure(p, element_id, output_dir)
			return(p)
		}

		p_time <- plot(geod_time) + ggplot2::theme(aspect.ratio = 0.8)
		p_time <- add_log_scale_if_needed(p_time, geod_time)

		p <- cowplot::plot_grid(
			p_geo + ggplot2::ggtitle("Geographic space"),
			p_time + ggplot2::ggtitle("Time"),
			nrow = 1,
			align = "h",
			axis = "tb"
		)

		save_figure(p, element_id, output_dir, width = 11, height = 4)
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
#' Returns NULL when either side lacks a usable time column.
#' @noRd
geodist_temporal_data <- function(samples_sf, area_sf) {
	if (!has_usable_time(samples_sf) || !has_usable_time(area_sf)) {
		return(NULL)
	}

	CAST::geodist(
		coerce_time_column(samples_sf),
		preddata = coerce_time_column(area_sf),
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

#' Replace the time column with its parsed form
#'
#' parse_time_column() only reads; CAST::geodist() needs the column itself to
#' be Date / POSIXct / numeric, which a GeoPackage TEXT field is not.
#' @noRd
coerce_time_column <- function(x) {
	parsed <- parse_time_column(x)
	if (is.null(parsed)) {
		return(x)
	}
	x[[stemp_time_column()]] <- parsed
	x
}

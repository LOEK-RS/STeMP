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

		p <- ggplot2::ggplot() +
			ggplot2::geom_sf(data = samples_data) +
			ggplot2::theme_minimal()

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
	n <- NULL # silence R CMD check on the aes() NSE

	output[[element_id]] <- shiny::renderPlot({
		samples_data <- tryCatch(geo_metadata$samples_sf(), error = function(e) NULL)

		if (is.null(samples_data) || !inherits(samples_data, "sf") || nrow(samples_data) == 0) {
			return(NULL)
		}

		counts <- count_sample_repetitions(samples_data)
		if (is.null(counts)) {
			return(NULL)
		}

		p <- ggplot2::ggplot() +
			ggplot2::geom_sf(data = counts, ggplot2::aes(colour = n)) +
			ggplot2::theme_minimal()

		p <- p +
			if (max(counts$n) > 1) {
				ggplot2::scale_colour_viridis_c(name = "Observations\nper location", trans = "log10")
			} else {
				ggplot2::scale_colour_viridis_c(name = "Observations\nper location")
			}

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
geo_map_timesteps <- function(output, element_id, geo_metadata = NULL, output_dir, max_facets = 16) {
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
		area_data$time_step <- factor(format(times), levels = format(all_steps))

		subtitle <- NULL
		if (length(all_steps) > max_facets) {
			kept <- format(all_steps[round(seq(1, length(all_steps), length.out = max_facets))])
			area_data <- area_data[as.character(area_data$time_step) %in% kept, , drop = FALSE]
			area_data$time_step <- droplevels(area_data$time_step)
			subtitle <- sprintf("Showing %d of %d time steps", max_facets, length(all_steps))
		}

		n_facets <- nlevels(area_data$time_step)
		n_col <- ceiling(sqrt(n_facets))
		n_row <- ceiling(n_facets / n_col)

		p <- ggplot2::ggplot(area_data) +
			ggplot2::geom_sf(fill = "transparent") +
			ggplot2::facet_wrap(~time_step, ncol = n_col) +
			ggplot2::labs(subtitle = subtitle) +
			ggplot2::theme_minimal() +
			ggplot2::theme(axis.text = ggplot2::element_text(size = 6))

		save_figure(
			p,
			element_id,
			output_dir,
			width = min(3 * n_col, 12),
			height = min(2.6 * n_row, 12)
		)
		p
	})
}

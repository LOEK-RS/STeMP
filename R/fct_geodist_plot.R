#' Render Geodistance Plot to Output
#'
#' @param output Shiny output object.
#' @param element_id Output ID for the plot.
#' @param geo_metadata Reactive list containing spatial data.
#' @param objective Character indicating which area to use.
#' @param output_dir temporary output directory
#' @noRd
geodist_plot <- function(output, element_id, geo_metadata = NULL,
                         objective = c("Model and prediction", "Model only"),
                         output_dir) {

  objective <- match.arg(objective)
  what <- if (objective == "Model and prediction") "prediction_area_sf" else "training_area_sf"

  output[[element_id]] <- shiny::renderPlot({
    samples_data <- tryCatch(geo_metadata$samples_sf(), error = function(e) NULL)
    area_data <- tryCatch(geo_metadata[[what]](), error = function(e) NULL)

    if (is.null(samples_data) || is.null(area_data) ||
        !inherits(samples_data, "sf") || !inherits(area_data, "sf")) {
      return(NULL)
    }

    samples_data <- sf::st_transform(samples_data, sf::st_crs(area_data))
    geod <- CAST::geodist(samples_data, modeldomain = area_data)
    p <- plot(geod) +
      ggplot2::theme(aspect.ratio = 0.8)

    save_figure(p, element_id, output_dir)
    p
  })
}


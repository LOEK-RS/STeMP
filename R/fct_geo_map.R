#' Render a Geo Map Plot to Output
#'
#' @param output Shiny output object.
#' @param element_id Output ID for the plot.
#' @param geo_metadata Reactive list containing spatial data.
#' @param what Character specifying which spatial data to use.
#' @noRd
geo_map <- function(output, element_id, geo_metadata = NULL,
                    what = c("samples_sf", "training_area_sf", "prediction_area_sf"),
                    output_dir) {

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


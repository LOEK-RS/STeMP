#' Capitalize First Letter of a String
#'
#' Converts the first character of a string to uppercase.
#'
#' @param x A character vector.
#' @return Character vector with first letter capitalized.
#' @export
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#' Normalize Text to ID Format
#'
#' Converts text to lowercase, removes asterisks, and replaces punctuation and spaces with underscores.
#'
#' @param x Character string to normalize.
#' @return Normalized character string suitable for IDs.
#' @export
normalize_id <- function(x) {
  gsub("[^a-zA-Z0-9]", "_", tolower(gsub("\\*", "", x)))
}

#' Get Value from Uploaded Input or Fallback
#'
#' @param uploaded_value Value from uploaded input.
#' @param fallback_fn Function to call if uploaded_value is NULL.
#' @return The uploaded value, fallback function result, or NULL.
#' @export
get_value <- function(uploaded_value, fallback_fn) {
  if (!is.null(uploaded_value)) {
    return(uploaded_value)
  } else if (!is.null(fallback_fn)) {
    return(fallback_fn())
  } else {
    return(NULL)
  }
}

#' Add Tooltip Info to Shiny Input Tag
#'
#' @param inputTag A Shiny input tag.
#' @param info_text Text to show in tooltip.
#' @return Modified input tag with tooltip added.
#' @export
inputWithHoverInfo <- function(inputTag, info_text) {
  children <- inputTag$children
  if (is.null(children) || !is.list(children)) {
    return(inputTag)
  }
  
  labelIndex <- which(sapply(children, function(x) inherits(x, "shiny.tag") && x$name == "label"))
  
  if (length(labelIndex) == 1 && !is.null(info_text) && nchar(info_text) > 0) {
    originalLabel <- children[[labelIndex]]
    
    wrappedLabel <- htmltools::tags$div(class = "input-label-icon",
                                        originalLabel,
                                        htmltools::tags$span(
                                          shiny::icon("info-circle"),
                                          class = "info-hover-icon",
                                          `data-toggle` = "tooltip",
                                          `data-placement` = "right",
                                          `data-html` = "true",
                                          title = htmltools::HTML(info_text)
                                        )
    )
    
    inputTag$children[[labelIndex]] <- wrappedLabel
  }
  
  inputTag
}

#' Add Tooltip to Shiny Input if Info Text Provided
#'
#' @param inputTag A Shiny input tag.
#' @param info_text Optional tooltip text.
#' @return Shiny input tag with tooltip if info_text is not NULL/empty.
#' @export
with_tooltip <- function(inputTag, info_text = NULL) {
  if (!is.null(info_text) && nchar(info_text) > 0) {
    inputWithHoverInfo(inputTag, info_text)
  } else {
    inputTag
  }
}

#' Unwrap Reactive Values in Metadata List
#'
#' @param metadata_list List containing reactive or non-reactive elements.
#' @return List with all elements unwrapped (reactive evaluated).
#' @export
unwrap_metadata <- function(metadata_list) {
  lapply(metadata_list, function(x) {
    if (shiny::is.reactive(x)) x() else x
  })
}

#' Validate Model Metadata Reactive List
#'
#' @param model_metadata Reactive or non-reactive model metadata list.
#' @param required_fields Character vector of required field names.
#' @return Reactive expression returning validated metadata list or NULL.
#' @export
validate_model_metadata <- function(model_metadata, required_fields = character()) {
  shiny::reactive({
    if (is.null(model_metadata)) return(NULL)
    
    for (field in required_fields) {
      if (!is.null(model_metadata[[field]]) && shiny::is.reactive(model_metadata[[field]])) {
        if (!model_metadata[[field]]()) return(NULL)
      }
    }
    
    if (inherits(model_metadata, "reactivevalues")) {
      shiny::reactiveValuesToList(model_metadata)
    } else if (shiny::is.reactive(model_metadata)) {
      model_metadata()
    } else {
      model_metadata
    }
  })
}

#' Validate Geo Metadata Reactive List
#'
#' @param geo_metadata Reactive or non-reactive geospatial metadata list.
#' @param required_fields Character vector of required field names.
#' @return Reactive expression returning validated metadata list or NULL.
#' @export
validate_geo_metadata <- function(geo_metadata, required_fields = character()) {
  shiny::reactive({
    if (is.null(geo_metadata)) return(NULL)
    
    for (field in required_fields) {
      if (!is.null(geo_metadata[[field]]) && shiny::is.reactive(geo_metadata[[field]])) {
        if (!geo_metadata[[field]]()) return(NULL)
      }
    }
    
    if (inherits(geo_metadata, "reactivevalues")) {
      shiny::reactiveValuesToList(geo_metadata)
    } else if (shiny::is.reactive(geo_metadata)) {
      geo_metadata()
    } else {
      geo_metadata
    }
  })
}

#' Save ggplot Figure to File
#'
#' @param figure ggplot object.
#' @param element_id Character ID to determine filename.
#' @export
save_figure <- function(figure, element_id) {
  fig_dir <- file.path("www", "figures")
  if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
  fig_name <- switch(element_id,
                     "protocol-prediction-geodistance_plot_prediction_area" = "geodist_prediction_area",
                     "protocol-prediction-prediction_map" = "prediction_area",
                     "protocol-model-sampling_area_map" = "sampling_area",
                     "protocol-model-sampling_locations" = "sampling_locations",
                     "protocol-model-geodistance_plot_sampling_area" = "geodist_sampling_area",
                     element_id)
  plot_path <- file.path(fig_dir, paste0(fig_name, ".png"))
  ggplot2::ggsave(plot_path, plot = figure, width = 6, height = 4, dpi = 300)
}

#' Render a Geo Map Plot to Output
#'
#' @param output Shiny output object.
#' @param element_id Output ID for the plot.
#' @param geo_metadata Reactive list containing spatial data.
#' @param what Character specifying which spatial data to use.
#' @export
geo_map <- function(output, element_id, geo_metadata = NULL,
                    what = c("samples_sf", "training_area_sf", "prediction_area_sf")) {
  what <- match.arg(what)
  
  output[[element_id]] <- shiny::renderPlot({
    title <- switch(what,
                    "samples_sf" = "Sampling locations",
                    "training_area_sf" = "Training area",
                    "prediction_area_sf" = "Prediction area")
    
    samples_data <- tryCatch(
      geo_metadata[[what]](),
      error = function(e) NULL
    )
    
    if (!is.null(samples_data) && inherits(samples_data, "sf") && nrow(samples_data) > 0) {
      p <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = samples_data) +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = title)
    } else {
      p <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = paste("No", title, "uploaded yet"),
                          size = 6, hjust = 0.5, vjust = 0.5) +
        ggplot2::theme_void()
    }
    
    save_figure(p, element_id)
    p
  })
}

#' Render Geodistance Plot to Output
#'
#' @param output Shiny output object.
#' @param element_id Output ID for the plot.
#' @param geo_metadata Reactive list containing spatial data.
#' @param objective Character indicating which area to use.
#' @export
geodist_plot <- function(output, element_id, geo_metadata = NULL,
                         objective = c("Model and prediction", "Model only")) {
  sel_val <- shiny::reactiveVal(NULL)
  objective <- match.arg(objective)
  what <- if (objective == "Model and prediction") "prediction_area_sf" else "training_area_sf"
  
  output[[element_id]] <- shiny::renderPlot({
    samples_data <- tryCatch(geo_metadata$samples_sf(), error = function(e) NULL)
    area_data <- tryCatch(geo_metadata[[what]](), error = function(e) NULL)
    
    if (!is.null(samples_data) && !is.null(area_data) &&
        inherits(samples_data, "sf") && inherits(area_data, "sf")) {
      
      samples_data <- sf::st_transform(samples_data, sf::st_crs(area_data))
      geod <- CAST::geodist(samples_data, modeldomain = area_data)
      
      Gj <- geod[geod$what == "sample-to-sample", ]$dist
      Gij <- geod[geod$what == "prediction-to-sample", ]$dist
      
      testks <- suppressWarnings(stats::ks.test(Gj, Gij, alternative = "greater"))
      sel_val(if (testks$p.value >= 0.05) "random" else "clustered")
      
      p <- plot(geod) + ggplot2::labs(title = "geodistance plot")
    } else {
      p <- ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "No geodist plot yet",
                          size = 6, hjust = 0.5, vjust = 0.5) +
        ggplot2::theme_void()
    }
    
    save_figure(p, element_id)
    p
  })
}

#' Calculate Geodistance Classification
#'
#' @param samples_sf sf object of sample locations.
#' @param area_sf sf object of spatial area.
#' @return Character classification "random" or "clustered".
#' @export
calculate_geodist_classification <- function(samples_sf, area_sf) {
  samples_sf <- sf::st_transform(samples_sf, sf::st_crs(area_sf))
  geod <- CAST::geodist(samples_sf, modeldomain = area_sf)
  
  Gj <- geod[geod$what == "sample-to-sample", ]$dist
  Gij <- geod[geod$what == "prediction-to-sample", ]$dist
  
  testks <- suppressWarnings(stats::ks.test(Gj, Gij, alternative = "greater"))
  if (testks$p.value >= 0.05) "random" else "clustered"
}

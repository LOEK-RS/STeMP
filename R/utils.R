#' Capitalize First Letter of a String
#'
#' Converts the first character of a string to uppercase.
#'
#' @param x A character vector.
#' @return Character vector with first letter capitalized.
#' @examples
#' firstup("hello")  # returns "Hello"
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
normalize_id <- function(x) {
  gsub("[^a-zA-Z0-9]", "_", tolower(gsub("\\*", "", x)))
}

#' Get Value from Uploaded Input or Fallback
#'
#' Returns the uploaded value if not NULL, otherwise calls a fallback function if provided.
#'
#' @param uploaded_value Value from uploaded input.
#' @param fallback_fn Function to call if uploaded_value is NULL.
#' @return The uploaded value, fallback function result, or NULL.
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
#' Wraps a Shiny input tag label with a tooltip icon showing additional info text on hover.
#'
#' @param inputTag A Shiny input tag.
#' @param info_text Text to show in tooltip.
#' @return Modified input tag with tooltip added.
inputWithHoverInfo <- function(inputTag, info_text) {
  children <- inputTag$children
  if (is.null(children) || !is.list(children)) {
    return(inputTag)
  }
  
  labelIndex <- which(sapply(children, function(x) inherits(x, "shiny.tag") && x$name == "label"))
  
  if (length(labelIndex) == 1 && !is.null(info_text) && nchar(info_text) > 0) {
    originalLabel <- children[[labelIndex]]
    
    wrappedLabel <- tags$div(class = "input-label-icon",
                             originalLabel,
                             tags$span(
                               icon("info-circle"),
                               class = "info-hover-icon",
                               `data-toggle` = "tooltip",
                               `data-placement` = "right",
                               `data-html` = "true",
                               title = HTML(info_text)
                             )
    )
    
    inputTag$children[[labelIndex]] <- wrappedLabel
  }
  
  inputTag
}

#' Add Tooltip to Shiny Input if Info Text Provided
#'
#' Conditionally adds a tooltip icon to a Shiny input tag if info text is given.
#'
#' @param inputTag A Shiny input tag.
#' @param info_text Optional tooltip text.
#' @return Shiny input tag with tooltip if info_text is not NULL/empty.
with_tooltip <- function(inputTag, info_text = NULL) {
  if (!is.null(info_text) && nchar(info_text) > 0) {
    inputWithHoverInfo(inputTag, info_text)
  } else {
    inputTag
  }
}

#' Unwrap Reactive Values in Metadata List
#'
#' Extracts values from reactive or non-reactive list elements.
#'
#' @param metadata_list List containing reactive or non-reactive elements.
#' @return List with all elements unwrapped (reactive evaluated).
unwrap_metadata <- function(metadata_list) {
  lapply(metadata_list, function(x) {
    if (is.reactive(x)) x() else x
  })
}

#' Validate Model Metadata Reactive List
#'
#' Checks that required reactive fields are non-null and returns a reactive list.
#'
#' @param model_metadata Reactive or non-reactive model metadata list.
#' @param required_fields Character vector of required field names.
#' @return Reactive expression returning validated metadata list or NULL.
validate_model_metadata <- function(model_metadata, required_fields = character()) {
  reactive({
    if (is.null(model_metadata)) return(NULL)
    
    for (field in required_fields) {
      if (!is.null(model_metadata[[field]]) && is.reactive(model_metadata[[field]])) {
        if (!model_metadata[[field]]()) return(NULL)
      }
    }
    
    if (inherits(model_metadata, "reactivevalues")) {
      reactiveValuesToList(model_metadata)
    } else if (is.reactive(model_metadata)) {
      model_metadata()
    } else {
      model_metadata
    }
  })
}

#' Validate Geo Metadata Reactive List
#'
#' Checks that required reactive fields are non-null and returns a reactive list.
#'
#' @param geo_metadata Reactive or non-reactive geospatial metadata list.
#' @param required_fields Character vector of required field names.
#' @return Reactive expression returning validated metadata list or NULL.
validate_geo_metadata <- function(geo_metadata, required_fields = character()) {
  reactive({
    if (is.null(geo_metadata)) return(NULL)
    
    for (field in required_fields) {
      if (!is.null(geo_metadata[[field]]) && is.reactive(geo_metadata[[field]])) {
        if (!geo_metadata[[field]]()) return(NULL)
      }
    }
    
    if (inherits(geo_metadata, "reactivevalues")) {
      reactiveValuesToList(geo_metadata)
    } else if (is.reactive(geo_metadata)) {
      geo_metadata()
    } else {
      geo_metadata
    }
  })
}

#' Save ggplot Figure to File
#'
#' Saves a ggplot object to a PNG file under www/figures folder with a name based on element ID.
#'
#' @param figure ggplot object.
#' @param element_id Character ID to determine filename.
#' @return NULL (called for side effect of saving file).
save_figure <- function(figure, element_id) {
  fig_dir <- file.path("www", "figures")
  if (!dir.exists(fig_dir)) dir.create(fig_dir)
  if(element_id == "protocol-prediction-geodistance_plot_prediction_area") fig_name <- "geodist_prediction_area"
  else if(element_id == "protocol-prediction-prediction_map") fig_name <- "prediction_area"
  else if(element_id == "protocol-model-sampling_area_map") fig_name <- "sampling_area"
  else if(element_id == "protocol-model-sampling_locations") fig_name <- "sampling_locations"
  else if(element_id == "protocol-model-geodistance_plot_sampling_area") fig_name <- "geodist_sampling_area"
  else fig_name <- element_id
  plot_path <- file.path(fig_dir, paste0(fig_name, ".png"))
  ggsave(plot_path, plot = figure, width = 6, height = 4, dpi = 300)
}

#' Render a Geo Map Plot to Output
#'
#' Creates a ggplot spatial map from reactive geo metadata and renders it to Shiny output.
#'
#' @param output Shiny output object.
#' @param element_id Output ID for the plot.
#' @param geo_metadata Reactive list containing spatial data.
#' @param what Character specifying which spatial data to use (samples_sf, training_area_sf, prediction_area_sf).
#' @return NULL (side effect: renders plot and saves figure).
geo_map <- function(output, element_id, geo_metadata = NULL, what=c("samples_sf", "training_area_sf", "prediction_area_sf")) {
  output[[element_id]] <- renderPlot({
    if(what == "samples_sf") {
      title <- "Sampling locations"
    } else if(what == "training_area_sf") {
      title <- "Training area"
    } else if(what == "prediction_area_sf") {
      title <- "Prediction area"
    }
    
    samples_data <- NULL
    
    if (!is.null(geo_metadata) &&
        !is.null(geo_metadata[[what]]) &&
        is.reactive(geo_metadata[[what]])) {
      samples_data <- tryCatch({
        geo_metadata[[what]]()
      }, error = function(e) {
        NULL
      })
    } else {
      message("geo_metadata$samples_sf not available or not reactive")
    }
    
    if (!is.null(samples_data) && inherits(samples_data, "sf") && nrow(samples_data) > 0) {
      p <- ggplot() +
        tryCatch(
          geom_sf(data = samples_data),
          error = function(e) {
            annotate("text", x = 0.5, y = 0.5, label = "geom_sf error")
          }
        ) +
        theme_minimal() +
        labs(title = title)
    } else {
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = paste("No", title, "uploaded yet"), size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void()
    }
    
    save_figure(figure=p, element_id=element_id)
    return(p)
  })
}

#' Render Geodistance Plot to Output
#'
#' Computes geodistance statistics from geo metadata and renders a comparison plot.
#'
#' @param output Shiny output object.
#' @param element_id Output ID for the plot.
#' @param geo_metadata Reactive list containing spatial data.
#' @param objective Character indicating which area to use ("Model and prediction" or "Model only").
#' @return NULL (side effect: renders plot and saves figure).
geodist_plot <- function(output, element_id, geo_metadata = NULL, objective = c("Model and prediction", "Model only")) {
  sel_val <- reactiveVal(NULL)  # reactive container to store the selection
  
  what <- if (objective == "Model and prediction") {
    "prediction_area_sf"
  } else {
    "training_area_sf"
  }
  
  output[[element_id]] <- renderPlot({
    samples_data <- NULL
    prediction_area_data <- NULL
    
    if (!is.null(geo_metadata) &&
        !is.null(geo_metadata$samples_sf) &&
        !is.null(geo_metadata[[what]])) {
      
      samples_data <- tryCatch(geo_metadata$samples_sf(), error = function(e) {
        NULL
      })
      prediction_area_data <- tryCatch(geo_metadata[[what]](), error = function(e) {
        NULL
      })
    } else {
      message("geo_metadata not available or not reactive")
    }
    
    if (!is.null(samples_data) && inherits(samples_data, "sf") && nrow(samples_data) > 0 &&
        !is.null(prediction_area_data) && inherits(prediction_area_data, "sf") && nrow(prediction_area_data) > 0) {
      
      samples_data <- st_transform(samples_data, st_crs(prediction_area_data))
      geod <- CAST::geodist(samples_data, modeldomain = prediction_area_data)
      
      Gj <- geod[geod$what == "sample-to-sample", ]$dist
      Gij <- geod[geod$what == "prediction-to-sample", ]$dist
      
      testks <- suppressWarnings(stats::ks.test(Gj, Gij, alternative = "greater"))
      sel <- if (testks$p.value >= 0.05) "random" else "clustered"
      
      sel_val(sel)
      p <- plot(geod) +
        labs(title = "geodistance plot")
      
    } else {
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "No geodist plot yet", size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void()
    }
    
    save_figure(figure=p, element_id=element_id)
    return(p)
  })
}

#' Calculate Geodistance Classification
#'
#' Computes geodistance classification ("random" or "clustered") based on spatial samples and area.
#'
#' @param samples_sf sf object of sample locations.
#' @param area_sf sf object of spatial area.
#' @return Character classification "random" or "clustered".
calculate_geodist_classification <- function(samples_sf, area_sf) {
  samples_sf <- sf::st_transform(samples_sf, sf::st_crs(area_sf))
  geod <- CAST::geodist(samples_sf, modeldomain = area_sf)
  
  Gj <- geod[geod$what == "sample-to-sample", ]$dist
  Gij <- geod[geod$what == "prediction-to-sample", ]$dist
  
  testks <- suppressWarnings(stats::ks.test(Gj, Gij, alternative = "greater"))
  
  if (testks$p.value >= 0.05) "random" else "clustered"
}

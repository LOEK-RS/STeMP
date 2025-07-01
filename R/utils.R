## helper function for tooltips
inputWithHoverInfo <- function(inputTag, info_text) {
  children <- inputTag$children
  if (is.null(children) || !is.list(children)) {
    return(inputTag)
  }
  
  labelIndex <- which(sapply(children, function(x) inherits(x, "shiny.tag") && x$name == "label"))
  
  if (length(labelIndex) == 1 && !is.null(info_text) && nchar(info_text) > 0) {
    # Extract and wrap the label
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
    
    # Replace the label with the wrapped version
    inputTag$children[[labelIndex]] <- wrappedLabel
  }
  
  inputTag
}

# Tooltip helper
with_tooltip <- function(inputTag, info_text = NULL) {
  if (!is.null(info_text) && nchar(info_text) > 0) {
    inputWithHoverInfo(inputTag, info_text)
  } else {
    inputTag
  }
}



unwrap_metadata <- function(metadata_list) {
  lapply(metadata_list, function(x) {
    if (is.reactive(x)) x() else x
  })
}


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


save_figure <- function(figure, element_id) {
  fig_dir <- file.path("www", "figures")
  if (!dir.exists(fig_dir)) dir.create(fig_dir)
  if(element_id == "protocol-prediction-p_pred_2") fig_name <- "geodist_prediction_area"
  else if(element_id == "protocol-prediction-p_pred_1") fig_name <- "prediction_area"
  else if(element_id == "protocol-model-d_response_2") fig_name <- "sampling_area"
  else if(element_id == "protocol-model-d_response_1") fig_name <- "sampling_locations"
  else if(element_id == "protocol-model-d_response_3") fig_name <- "geodist_sampling_area"
  else fig_name <- element_id
  plot_path <- file.path(fig_dir, paste0("protocol_plot_", fig_name, ".png"))
  ggsave(plot_path, plot = figure, width = 6, height = 4, dpi = 300)
}


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
            #message("❌ Error in geom_sf(): ", e$message)
            annotate("text", x = 0.5, y = 0.5, label = "geom_sf error")
          }
        ) +
        theme_minimal() +
        labs(title = title)
    } else {
      #message("⚠️ No samples data, rendering placeholder")
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = paste("No", title, "uploaded yet"), size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void()
    }
    
    # Save to disk for report generation
    save_figure(figure=p, element_id=element_id)
    return(p)
  })
}

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
    # Save to disk for report generation
    save_figure(figure=p, element_id=element_id)
    return(p)
  })
  
}


# geodist calculation
calculate_geodist_classification <- function(samples_sf, area_sf) {
  samples_sf <- sf::st_transform(samples_sf, sf::st_crs(area_sf))
  geod <- CAST::geodist(samples_sf, modeldomain = area_sf)
  
  Gj <- geod[geod$what == "sample-to-sample", ]$dist
  Gij <- geod[geod$what == "prediction-to-sample", ]$dist
  
  testks <- suppressWarnings(stats::ks.test(Gj, Gij, alternative = "greater"))
  
  if (testks$p.value >= 0.05) "random" else "clustered"
}

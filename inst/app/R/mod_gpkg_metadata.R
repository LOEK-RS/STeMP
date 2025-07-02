#' GPKG Metadata Module UI
#'
#' This module does not render any UI components. It purely provides reactive
#' metadata extraction and validation flags from provided spatial inputs.
#'
#' @param id Module namespace ID
#' @return An empty tagList (no UI)
mod_gpkg_metadata_ui <- function(id) {
  ns <- NS(id)
  tagList()  # no UI elements required for this module
}

#' GPKG Metadata Module Server
#'
#' Processes and validates input spatial objects representing samples,
#' training area, and prediction area. Extracts CRS information and returns
#' reactive flags indicating presence and validity of each spatial input.
#'
#' @param id Module namespace ID
#' @param samples A reactiveValues or list containing:
#'   - data(): reactive returning sf object for samples
#'   - valid(): reactive returning TRUE/FALSE validity of samples data
#' @param training_area Similar reactiveValues/list with data() and valid() for training area sf object
#' @param prediction_area Similar reactiveValues/list with data() and valid() for prediction area sf object
#'
#' @return A list of reactives:
#' \itemize{
#'   \item{has_samples}{Reactive boolean: TRUE if samples data exists and is valid}
#'   \item{has_training_area}{Reactive boolean: TRUE if training area exists and is valid}
#'   \item{has_prediction_area}{Reactive boolean: TRUE if prediction area exists and is valid}
#'   \item{samples_crs}{Reactive string: CRS of samples spatial data, as EPSG code or proj4 string}
#'   \item{samples_sf}{Reactive sf object for samples}
#'   \item{training_area_sf}{Reactive sf object for training area}
#'   \item{prediction_area_sf}{Reactive sf object for prediction area}
#' }
mod_gpkg_metadata_server <- function(id, samples, training_area, prediction_area) {
  moduleServer(id, function(input, output, session) {
    
    # Check if samples data exists and passes validation
    has_samples <- reactive({
      !is.null(samples$data()) && samples$valid()
    })
    
    # Check if training area data exists and passes validation
    has_training_area <- reactive({
      !is.null(training_area$data()) && training_area$valid()
    })
    
    # Check if prediction area data exists and passes validation
    has_prediction_area <- reactive({
      !is.null(prediction_area$data()) && prediction_area$valid()
    })
    
    # Extract CRS info from samples data:
    # Prefer EPSG code if available, otherwise return proj4 string
    samples_crs <- reactive({
      req(has_samples())
      crs_obj <- sf::st_crs(samples$data())
      if (!is.na(crs_obj$epsg)) {
        paste0("EPSG:", crs_obj$epsg)
      } else {
        crs_obj$proj4string
      }
    })
    
    # Reactive for the samples spatial object (sf)
    samples_sf <- reactive({
      req(has_samples())
      samples$data()
    })
    
    # Reactive for the training area spatial object (sf)
    training_area_sf <- reactive({
      req(has_training_area())
      training_area$data()
    })
    
    # Reactive for the prediction area spatial object (sf)
    prediction_area_sf <- reactive({
      req(has_prediction_area())
      prediction_area$data()
    })
    
    # Return a list of reactives for use elsewhere in app
    return(list(
      has_samples = has_samples,
      has_training_area = has_training_area,
      has_prediction_area = has_prediction_area,
      samples_crs = samples_crs,
      samples_sf = samples_sf,
      training_area_sf = training_area_sf,
      prediction_area_sf = prediction_area_sf
    ))
  })
}

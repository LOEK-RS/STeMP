mod_gpkg_metadata_ui <- function(id) {
  ns <- NS(id)
  tagList()  # no UI needed here
}

mod_gpkg_metadata_server <- function(id, samples, training_area, prediction_area) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive flags if inputs exist and are valid
    has_samples <- reactive({ !is.null(samples$data()) && samples$valid() })
    has_training_area <- reactive({ !is.null(training_area$data()) && training_area$valid() })
    has_prediction_area <- reactive({ !is.null(prediction_area$data()) && prediction_area$valid() })
    
    
    # Extract CRS as EPSG string or proj4 string
    samples_crs <- reactive({
      req(has_samples())
      crs_obj <- st_crs(samples$data())
      if (!is.na(crs_obj$epsg)) {
        paste0("EPSG:", crs_obj$epsg)
      } else {
        crs_obj$proj4string
      }
    })
    
    samples_sf <- reactive({
      req(has_samples())
      samples$data()
    })
    
    training_area_sf <- reactive({
      req(has_training_area())
      training_area$data()
    })
    
    prediction_area_sf <- reactive({
      req(has_prediction_area())
      prediction_area$data()
    })
    
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

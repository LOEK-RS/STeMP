#' Unwrap Reactive Values in Metadata List
#'
#' @param metadata_list List containing reactive or non-reactive elements.
#' @return List with all elements unwrapped (reactive evaluated).
#' @noRd
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
#' @noRd
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
#' @noRd
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

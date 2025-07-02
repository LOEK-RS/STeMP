#' Warnings Module - UI
#'
#' No visible UI elements. This module dynamically shows warning notifications
#' based on reactive inputs.
#'
#' @param id Module namespace ID
#' @return Empty UI placeholder (notifications appear dynamically)
mod_warnings_ui <- function(id) {
  ns <- NS(id)
  tagList()
}

#' Warnings Module - Server
#'
#' Observes reactive inputs and shows context-specific warning notifications
#' to alert users about potential issues with sampling design, validation,
#' uncertainty quantification, and predictor types.
#'
#' @param id Module namespace ID
#' @param sampling_design Reactive returning current sampling design (e.g., "clustered", "random")
#' @param validation_method Reactive returning current validation method (e.g., "Random Cross-Validation", "Spatial Cross-Validation")
#' @param uncertainty_quantification Reactive returning uncertainty quantification method (e.g., "none")
#' @param predictor_types Reactive returning a vector of predictor types (e.g., contains "Spatial Proxies")
mod_warnings_server <- function(id, sampling_design, validation_method, uncertainty_quantification, predictor_types) {
  moduleServer(id, function(input, output, session) {
    warning_flags <- reactiveValues()
    
    # Utility to check condition and show notification only once
    check_and_warn <- function(condition, message, flag_name) {
      if (isTRUE(condition()) && is.null(warning_flags[[flag_name]])) {
        showNotification(message, type = "warning", duration = 10)
        warning_flags[[flag_name]] <- TRUE
      } else if (!isTRUE(condition()) && !is.null(warning_flags[[flag_name]])) {
        warning_flags[[flag_name]] <- NULL
      }
    }
    
    # Warning: Random CV with clustered samples can be optimistic
    observe({
      req(sampling_design(), validation_method())
      
      is_problematic <- sampling_design() == "clustered" &&
        validation_method() == "Random Cross-Validation"
      
      check_and_warn(
        condition = reactive({ is_problematic }),
        message = "Random CV might yield overly optimistic results with clustered samples",
        flag_name = "clustered_random_cv"
      )
    })
    
    # Warning: Spatial CV with random samples can be pessimistic
    observe({
      req(sampling_design(), validation_method())
      
      is_problematic <- sampling_design() == "random" &&
        validation_method() == "Spatial Cross-Validation"
      
      check_and_warn(
        condition = reactive({ is_problematic }),
        message = "Spatial CV might yield overly pessimistic results with clustered samples",
        flag_name = "random_clustered_cv"
      )
    })
    
    # Warning: Spatial proxies + clustered samples → extrapolation risk
    observe({
      req(sampling_design(), predictor_types())
      
      is_problematic <- "Spatial Proxies" %in% predictor_types() &&
        sampling_design() == "clustered"
      
      check_and_warn(
        condition = reactive({ is_problematic }),
        message = "⚠️ Warning: Using spatial proxies with clustered samples likely leads to extrapolation situations.\nConsider using physically relevant predictors instead.",
        flag_name = "clustered_proxies"
      )
    })
    
    # Warning: Clustered samples + no uncertainty quantification
    observe({
      req(sampling_design(), uncertainty_quantification())
      
      is_problematic <- sampling_design() == "clustered" &&
        uncertainty_quantification() == "none"
      
      check_and_warn(
        condition = reactive({ is_problematic }),
        message = paste0(
          "⚠️ Warning: Clustered samples often lead to extrapolation when the model is applied to feature combinations not present in the training data.\n",
          "Identifying areas of extrapolation/uncertainty and communicating them to the user of the prediction is recommended."
        ),
        flag_name = "clustered_noAssessment"
      )
    })
  })
}

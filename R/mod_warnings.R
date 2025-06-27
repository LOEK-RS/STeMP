# UI function
mod_warnings_ui <- function(id) {
  ns <- NS(id)
  tagList()  # No visible UI, notifications appear dynamically
}

# Server function
mod_warnings_server <- function(id, sampling_design, validation_method, uncertainty_quantification, predictor_types) {
  moduleServer(id, function(input, output, session) {
    warning_flags <- reactiveValues()
    
    check_and_warn <- function(condition, message, flag_name) {
      if (isTRUE(condition()) && is.null(warning_flags[[flag_name]])) {
        showNotification(message, type = "warning", duration = 10)
        warning_flags[[flag_name]] <- TRUE
      } else if (!isTRUE(condition()) && !is.null(warning_flags[[flag_name]])) {
        warning_flags[[flag_name]] <- NULL
      }
    }
    
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
    
    observe({
      req(sampling_design(), predictor_types())
      
      is_problematic <- predictor_types() %in% "Spatial Proxies" &&
        sampling_design() == "clustered"
      
      check_and_warn(
        condition = reactive({ is_problematic }),
        message = "⚠️ Warning: Using spatial proxies with clustered samples likely leads to extrapolation situations.\nYou might
                         consider using physically relevant predictors instead.",
        flag_name = "clustered_proxies"
      )
    })
    
    
    ## to-do ------
    observe({
      req(uncertainty_quantification(), sampling_design())
      
      is_problematic <- sampling_design() == "clustered" &&
        uncertainty_quantification() == "none"
      
      check_and_warn(
        condition = reactive({ is_problematic }),
        message = "⚠️ Warning: Clustered samples often lead to extrapolation when the model is applied to feature combinations not present in the training data.
    Identifying areas of extrapolation/uncertainty and communicating them to the user of the prediction is recommended.",
        flag_name = "clustered_noAssessment"
      )
    })
    
      
  })
}


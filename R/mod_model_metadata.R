# mod_model_metadata.R
# Module to extract metadata from a trained model object

mod_model_metadata_ui <- function(id) {
  ns <- NS(id)
  tagList()  # No UI needed unless you want to expose values
}

mod_model_metadata_server <- function(id, input_model_object) {
  moduleServer(id, function(input, output, session) {
    
    # ReactiveVals to store metadata
    model_object <- reactiveVal(NULL)
    num_training_samples <- reactiveVal(NULL)
    num_predictors <- reactiveVal(NULL)
    names_predictors <- reactiveVal(NULL)
    model_algorithm <- reactiveVal(NULL)
    model_type <- reactiveVal(NULL)
    model_hyperparams <- reactiveVal(NULL)
    num_classes <- reactiveVal(NULL)
    num_samples_per_class <- reactiveVal(NULL)
    interpolation_range <- reactiveVal(NULL)
    
    has_model <- reactive({
      !is.null(model_object())
    })
    
    observeEvent(input_model_object(), {
      model <- input_model_object()
      req(model)
      
      # Reset metadata
      model_object(NULL)
      num_training_samples(NULL)
      num_predictors(NULL)
      names_predictors(NULL)
      model_algorithm(NULL)
      model_type(NULL)
      model_hyperparams(NULL)
      num_classes(NULL)
      num_samples_per_class(NULL)
      interpolation_range(NULL)
      
      model_object(model)
      
      # --- Caret model ---
      if ("train" %in% class(model)) {
        data <- model$trainingData
        if (!is.null(data)) {
          response <- data$.outcome
          predictors <- data[, setdiff(names(data), ".outcome"), drop = FALSE]
          
          num_training_samples(nrow(data))
          num_predictors(ncol(predictors))
          names_predictors(paste(names(predictors), collapse = ", "))
          
          model_type(model$modelType)
          
          model_algorithm(model$method %||% "")
          
          if (!is.null(model$bestTune)) {
            model_hyperparams(paste0(names(model$bestTune), "=", model$bestTune, collapse = ", "))
          }
          
          if (model_type() == "Classification") {
            tab <- table(response)
            num_classes(length(tab))
            num_samples_per_class(paste0(names(tab), ": ", as.integer(tab), collapse = ", "))
          } else {
            interpolation_range(paste(round(range(response), 3), collapse = " to "))
          }
        }
        
        # --- Tidymodels model ---
      } else if ("workflow" %in% class(model) || "model_fit" %in% class(model)) {
        fit <- if ("workflow" %in% class(model)) {
          parsnip::extract_fit_parsnip(model)
        } else model
        
        data <- tryCatch({
          if ("workflow" %in% class(model)) workflows::extract_mold(model) else NULL
        }, error = function(e) NULL)
        
        model_algorithm(fit$spec$engine %||% "")
        model_type(fit$spec$mode %||% "")
        
        if (!is.null(fit$spec$args)) {
          args <- lapply(fit$spec$args, function(x) if (is.symbol(x)) NA else x)
          model_hyperparams(paste0(names(args), "=", args, collapse = ", "))
        }
        
        if (!is.null(data)) {
          outcomes <- data$outcomes
          predictors <- data$predictors
          
          num_training_samples(nrow(outcomes))
          num_predictors(ncol(predictors))
          names_predictors(paste(names(predictors), collapse = ", "))
          
          response <- outcomes[[1]]
          if (is.factor(response)) {
            tab <- table(response)
            num_classes(length(tab))
            num_samples_per_class(paste0(names(tab), ": ", as.integer(tab), collapse = ", "))
          } else {
            interpolation_range(paste(round(range(response), 3), collapse = " to "))
          }
        }
        
        # --- mlr3 model ---
      } else if ("Learner" %in% class(model) || "GraphLearner" %in% class(model)) {
        model_algorithm(model$label %||% "")
        model_type(if (grepl("classif", model$id)) "Classification" else "Regression")
        
        if (!is.null(model$param_set$values)) {
          model_hyperparams(paste0(names(model$param_set$values), "=", unlist(model$param_set$values), collapse = ", "))
        }
        
        data <- tryCatch({
          if (!is.null(model$state$train_task)) model$state$train_task$data() else model$training_data
        }, error = function(e) NULL)
        
        target_name <- tryCatch({
          model$task$target_names
        }, error = function(e) NULL)
        
        if (!is.null(data) && !is.null(target_name) && target_name %in% names(data)) {
          response <- data[[target_name]]
          predictors <- data[, setdiff(names(data), target_name), drop = FALSE]
          
          num_training_samples(nrow(data))
          num_predictors(ncol(predictors))
          names_predictors(paste(names(predictors), collapse = ", "))
          
          if (is.factor(response)) {
            tab <- table(response)
            num_classes(length(tab))
            num_samples_per_class(paste0(names(tab), ": ", as.integer(tab), collapse = ", "))
          } else {
            interpolation_range(paste(round(range(response), 3), collapse = " to "))
          }
        }
        
        # --- Unsupported model ---
      } else {
        showNotification("Unsupported model type. Supported: caret, tidymodels, mlr3.", type = "error")
      }
    })
    
    return(list(
      has_model = has_model,
      num_training_samples = num_training_samples,
      num_predictors = num_predictors,
      names_predictors = names_predictors,
      model_algorithm = model_algorithm,
      model_type = model_type,
      model_hyperparams = model_hyperparams,
      num_classes = num_classes,
      num_samples_per_class = num_samples_per_class,
      interpolation_range = interpolation_range
    ))
  })
}

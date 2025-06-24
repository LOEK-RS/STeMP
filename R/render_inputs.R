
render_suggestion <- function(element_id, label, suggestions) {
  choices <- sort(trimws(unlist(strsplit(suggestions, ","))))
  selectizeInput(
    inputId = element_id,
    label = label,
    choices = choices,
    multiple = TRUE,
    options = list(create = TRUE, placeholder = "Choose or type")
  )
}

render_suggestion_single <- function(element_id, label, suggestions) {
  choices <- sort(trimws(unlist(strsplit(suggestions, ","))))
  selectizeInput(
    inputId = element_id,
    label = label,
    choices = choices,
    multiple = FALSE,
    options = list(create = TRUE, placeholder = "Choose or type")
  )
}

render_text_input <- function(element_id, element) {
  textInput(inputId = element_id, label = element)
}

render_text_area <- function(element_id, element) {
  textAreaInput(inputId = element_id, label = element, rows = 3)
}

render_n_samples <- function(element_id, element, model_metadata = NULL) {
  value <- if (!is.null(model_metadata) && !is.null(model_metadata$num_training_samples)) {
    model_metadata$num_training_samples()
  } else NULL
  
  tagList(
    numericInput(inputId = element_id, label = element, value = value)
  )
}

render_n_predictors <- function(element_id, element, model_metadata = NULL) {
  value <- if (!is.null(model_metadata) && !is.null(model_metadata$num_predictors)) {
    model_metadata$num_predictors()
  } else NULL
  
  tagList(
    numericInput(inputId = element_id, label = element, value = value)
  )
}

render_n_classes <- function(element_id, element, model_metadata = NULL) {
  value <- if (!is.null(model_metadata) && !is.null(model_metadata$num_classes)) {
    model_metadata$num_classes()
  } else NULL
  
  tagList(
    numericInput(inputId = element_id, label = element, value = value)
  )
}

render_n_samples_class <- function(element_id, element, model_metadata = NULL) {
  value <- if (!is.null(model_metadata) && !is.null(model_metadata$num_samples_per_class)) {
    model_metadata$num_samples_per_class()
  } else NULL
  
  numericInput(inputId = element_id, label = element, value = value)
}

render_range <- function(element_id, element, model_metadata = NULL) {
  value <- if (!is.null(model_metadata) && !is.null(model_metadata$interpolation_range)) {
    model_metadata$interpolation_range()
  } else NULL
  
  textInput(inputId = element_id, label = element, value = value)
}

render_names_predictors <- function(element_id, element, model_metadata = NULL) {
  value <- if (!is.null(model_metadata) && !is.null(model_metadata$names_predictors)) {
    model_metadata$names_predictors()
  } else NULL 
  
  textInput(inputId = element_id, label = element, value = value)
}

render_hyperparameters <- function(element_id, element, model_metadata = NULL) {
  value <- if (!is.null(model_metadata) && !is.null(model_metadata$model_hyperparams)) {
    model_metadata$model_hyperparams()
  } else NULL 
  
  textInput(inputId = element_id, label = element, value = value)
}

render_model_type <- function(element_id, element, model_metadata = NULL) {
  value <- if (!is.null(model_metadata) && !is.null(model_metadata$model_type)) {
    model_metadata$model_type()
  } else "" 
  
  selectInput(
    inputId = element_id, label = element,
    choices = c("", "Classification", "Regression"),
    selected = value
  )
}

render_model_algorithm <- function(element_id, element, model_metadata = NULL) {
  default_algos <- c("rf", "gbm", "glm", "svmRadial", "nnet", "rpart")
  selected_algo <- if (!is.null(model_metadata) && !is.null(model_metadata$model_algorithm)) {
    model_metadata$model_algorithm()
  } else ""
  
  algo_choices <- if (selected_algo != "" && !(selected_algo %in% default_algos)) {
    c(default_algos, selected_algo)
  } else {
    default_algos
  }
  
  selectInput(
    inputId = element_id,
    label = element,
    choices = c("", unique(algo_choices)),
    selected = selected_algo
  )
}

render_crs <- function(element_id, element, geo_metadata = NULL) {
  value <- if (!is.null(geo_metadata) && !is.null(geo_metadata$samples_crs)) {
    geo_metadata$samples_crs()
  } else NULL 
  
  textInput(inputId = element_id, label = element, value = value)
}


render_design <- function(element_id, element, selected = NULL) {
  selectInput(
    inputId = element_id,
    label = element,
    choices = c("", "clustered", "random", "stratified"),
    selected = selected %||% ""
  )
}



render_design_server <- function(input, output, session, element_id, geodist_sel) {
  observeEvent(geodist_sel(), {
    selected_val <- geodist_sel()
    if (is.null(selected_val)) return(NULL)
    
    shinyjs::delay(100, {
      updateSelectInput(session, inputId = element_id, selected = selected_val)
    })
  }, ignoreInit = FALSE)
  
  observe({
    val <- input[[element_id]]
  })
}


# -- UPDATED to return plotOutput for UI --
render_samples_plot <- function(element_id, element, geo_metadata = NULL, ns = identity) {
  plotOutput(outputId = ns(element_id), height = "300px")
}
render_training_area_plot <- function(element_id, element, geo_metadata = NULL, ns = identity) {
  plotOutput(outputId = ns(element_id), height = "300px")
}
render_prediction_area_plot <- function(element_id, element, geo_metadata = NULL, ns = identity) {
  plotOutput(outputId = ns(element_id), height = "300px")
}
render_geodist_plot <- function(element_id, element, ns = identity) {
  plotOutput(outputId = ns(element_id), height = "300px")
}



# -- SERVER function to render the samples plot --
render_samples_plot_server <- function(output, element_id, geo_metadata = NULL, what="samples_sf") {
  geo_map(output, element_id, geo_metadata, what)
}  
render_training_area_plot_server <- function(output, element_id, geo_metadata = NULL, what="training_area_sf") {
  geo_map(output, element_id, geo_metadata, what)
}
render_prediction_area_plot_server <- function(output, element_id, geo_metadata = NULL, what="prediction_area_sf") {
  geo_map(output, element_id, geo_metadata, what)
}
render_geodist_plot_server <- function(output, element_id, geo_metadata = NULL, objective = c("Model and prediction", "Model only")) {
  geodist_plot(output, element_id, geo_metadata, objective)
}


## combine everything
render_input_field <- function(element_type, element_id, label, o_objective_1, suggestions = NULL, 
                               info_text = NULL, model_metadata = NULL, geo_metadata = NULL, ns = identity) {
  
  input_tag <- switch(
    element_type,
    
    "text" = textInput(element_id, label, value = ""),
    "author" = textInput(element_id, label, value = ""),
    "hyperparams" = textAreaInput(element_id, label, value = ""),
    "suggestion" = render_suggestion(element_id, label, suggestions),
    "suggestion_single" = render_suggestion_single(element_id, label, suggestions),
    
    "num_training_samples" = render_n_samples(element_id, label, model_metadata),
    "num_predictors" = render_n_predictors(element_id, label, model_metadata),
    "num_classes" = render_n_classes(element_id, label, model_metadata),
    "num_samples_per_class" = render_n_samples_class(element_id, label, model_metadata),
    "interpolation_range" = render_range(element_id, label, model_metadata),
    "names_predictors" = render_names_predictors(element_id, label, model_metadata),
    "model_hyperparams" = render_hyperparameters(element_id, label, model_metadata),
    "model_type" = render_model_type(element_id, label, model_metadata),
    "model_algorithm" = render_model_algorithm(element_id, label, model_metadata),
    "samples_crs" = render_crs(element_id, label, geo_metadata),
    
    # fallback
    textInput(element_id, label, value = "")
  )
  
  if (!is.null(info_text) && nchar(info_text) > 0) {
    return(inputWithHoverInfo(input_tag, info_text))
  } else {
    return(input_tag)
  }
}

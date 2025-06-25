

render_suggestion <- function(element_id, label, suggestions, info_text = NULL) {
  choices <- sort(trimws(unlist(strsplit(suggestions, ","))))
  input <- selectizeInput(
    inputId = element_id,
    label = label,
    choices = choices,
    multiple = TRUE,
    options = list(create = TRUE, placeholder = "Choose or type")
  )
  with_tooltip(input, info_text)
}

render_suggestion_single <- function(element_id, label, suggestions, info_text = NULL) {
  choices <- sort(trimws(unlist(strsplit(suggestions, ","))))
  input <- selectizeInput(
    inputId = element_id,
    label = label,
    choices = choices,
    multiple = FALSE,
    options = list(create = TRUE, placeholder = "Choose or type")
  )
  with_tooltip(input, info_text)
}

render_text_input <- function(element_id, element, info_text = NULL) {
  input <- textInput(inputId = element_id, label = element)
  with_tooltip(input, info_text)
}

render_text_area <- function(element_id, element, info_text = NULL) {
  input <- textAreaInput(inputId = element_id, label = element, rows = 3)
  with_tooltip(input, info_text)
}

render_n_samples <- function(element_id, element, model_metadata = NULL, info_text = NULL) {
  value <- if (!is.null(model_metadata) && !is.null(model_metadata$num_training_samples)) {
    model_metadata$num_training_samples()
  } else NULL
  
  input <- numericInput(inputId = element_id, label = element, value = value)
  with_tooltip(input, info_text)
}

render_n_predictors <- function(element_id, element, model_metadata = NULL, info_text = NULL) {
  value <- if (!is.null(model_metadata) && !is.null(model_metadata$num_predictors)) {
    model_metadata$num_predictors()
  } else NULL
  
  input <- numericInput(inputId = element_id, label = element, value = value)
  with_tooltip(input, info_text)
}

render_n_classes <- function(element_id, element, model_metadata = NULL, info_text = NULL) {
  value <- if (!is.null(model_metadata) && !is.null(model_metadata$num_classes)) {
    model_metadata$num_classes()
  } else NULL
  
  input <- numericInput(inputId = element_id, label = element, value = value)
  with_tooltip(input, info_text)
}

render_n_samples_class <- function(element_id, element, model_metadata = NULL, info_text = NULL) {
  value <- if (!is.null(model_metadata) && !is.null(model_metadata$num_samples_per_class)) {
    model_metadata$num_samples_per_class()
  } else NULL
  
  input <- numericInput(inputId = element_id, label = element, value = value)
  with_tooltip(input, info_text)
}

render_range <- function(element_id, element, model_metadata = NULL, info_text = NULL) {
  value <- if (!is.null(model_metadata) && !is.null(model_metadata$interpolation_range)) {
    model_metadata$interpolation_range()
  } else NULL
  
  input <- textInput(inputId = element_id, label = element, value = value)
  with_tooltip(input, info_text)
}

render_names_predictors <- function(element_id, element, model_metadata = NULL, info_text = NULL) {
  value <- if (!is.null(model_metadata) && !is.null(model_metadata$names_predictors)) {
    model_metadata$names_predictors()
  } else NULL 
  
  input <- textInput(inputId = element_id, label = element, value = value)
  with_tooltip(input, info_text)
}

render_hyperparameters <- function(element_id, element, model_metadata = NULL, info_text = NULL) {
  value <- if (!is.null(model_metadata) && !is.null(model_metadata$model_hyperparams)) {
    model_metadata$model_hyperparams()
  } else NULL 
  
  input <- textInput(inputId = element_id, label = element, value = value)
  with_tooltip(input, info_text)
}

render_model_type <- function(element_id, element, model_metadata = NULL, info_text = NULL) {
  value <- if (!is.null(model_metadata) && !is.null(model_metadata$model_type)) {
    model_metadata$model_type()
  } else "" 
  
  input <- selectInput(
    inputId = element_id, label = element,
    choices = c("", "Classification", "Regression"),
    selected = value
  )
  with_tooltip(input, info_text)
}

render_model_algorithm <- function(element_id, element, model_metadata = NULL, info_text = NULL) {
  default_algos <- c("rf", "gbm", "glm", "svmRadial", "nnet", "rpart")
  selected_algo <- if (!is.null(model_metadata) && !is.null(model_metadata$model_algorithm)) {
    model_metadata$model_algorithm()
  } else ""
  
  algo_choices <- if (selected_algo != "" && !(selected_algo %in% default_algos)) {
    c(default_algos, selected_algo)
  } else {
    default_algos
  }
  
  input <- selectInput(
    inputId = element_id,
    label = element,
    choices = c("", unique(algo_choices)),
    selected = selected_algo
  )
  with_tooltip(input, info_text)
}

render_crs <- function(element_id, element, geo_metadata = NULL, info_text = NULL) {
  value <- if (!is.null(geo_metadata) && !is.null(geo_metadata$samples_crs)) {
    geo_metadata$samples_crs()
  } else NULL 
  
  input <- textInput(inputId = element_id, label = element, value = value)
  with_tooltip(input, info_text)
}

render_design <- function(element_id, element, selected = NULL, info_text = NULL) {
  input <- selectInput(
    inputId = element_id,
    label = element,
    choices = c("", "clustered", "random", "stratified"),
    selected = selected %||% ""
  )
  with_tooltip(input, info_text)
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

render_input_field <- function(element_type, element_id, label, o_objective_1, suggestions = NULL, 
                               info_text = NULL, model_metadata = NULL, geo_metadata = NULL, ns = identity) {
  
  input_tag <- switch(
    element_type,
    
    "text" = render_text_input(element_id, label, info_text),
    "author" = render_text_input(element_id, label, info_text),
    "hyperparams" = render_text_area(element_id, label, info_text),
    "suggestion" = render_suggestion(element_id, label, suggestions, info_text),
    "suggestion_single" = render_suggestion_single(element_id, label, suggestions, info_text),
    
    "num_training_samples" = render_n_samples(element_id, label, model_metadata, info_text),
    "num_predictors" = render_n_predictors(element_id, label, model_metadata, info_text),
    "num_classes" = render_n_classes(element_id, label, model_metadata, info_text),
    "num_samples_per_class" = render_n_samples_class(element_id, label, model_metadata, info_text),
    "interpolation_range" = render_range(element_id, label, model_metadata, info_text),
    "names_predictors" = render_names_predictors(element_id, label, model_metadata, info_text),
    "model_hyperparams" = render_hyperparameters(element_id, label, model_metadata, info_text),
    "model_type" = render_model_type(element_id, label, model_metadata, info_text),
    "model_algorithm" = render_model_algorithm(element_id, label, model_metadata, info_text),
    "samples_crs" = render_crs(element_id, label, geo_metadata, info_text),
    
    # fallback
    render_text_input(element_id, label, info_text)
  )
  
  return(input_tag)
}

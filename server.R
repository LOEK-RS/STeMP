library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(DT)
library(dplyr)
library(tidyr)
library(markdown)
library(rangeModelMetadata)
library(sf)
library(ggplot2)
library(CAST)
library(parsnip)
library(workflows)
library(mlr3)

odmap_dict = read.csv("www/odmap_dict.csv", header = T, stringsAsFactors = F)
rmm_dict = rmmDataDictionary()

server <- function(input, output, session) {
  # ------------------------------------------------------------------------------------------#
  #                           Rendering functions for UI elements                             # 
  # ------------------------------------------------------------------------------------------#
  render_text = function(element_id, element_placeholder){
    textAreaInput(inputId = element_id, label = element_placeholder, height = "45px", resize = "vertical")
  }
  
  render_authors = function(){
    div(
      dataTableOutput("authors_table", width = "100%"),
      actionButton("add_author", label = NULL, icon = icon("plus")),
      br(), br()
    )
  }
  
  render_objective = function(element_id, element_placeholder){
    selectizeInput(inputId = element_id, label = NULL, multiple = F, options = list(create = T, placeholder = "Choose from list"),
                   choices = list("", "Inference and explanation", "Mapping and interpolation"))
  }
  
  
  ##### Infer information from model objects---------------
  # Initialize reactive values to store the training sample count etc. 
  num_training_samples <- reactiveVal(NULL)
  num_predictors <- reactiveVal(NULL)
  names_predictors <- reactiveVal(NULL)
  model_algorithm <- reactiveVal(NULL)
  model_type <- reactiveVal(NULL)
  model_hyperparams <- reactiveVal(NULL)
  num_classes <- reactiveVal(NULL)
  num_samples_per_class <- reactiveVal(NULL)
  interpolation_range <- reactiveVal(NULL)
  
  # When caret model is uploaded, read it and extract training set size
  observeEvent(input$model_upload, {
    
    req(input$model_upload)
    
    # Reset reactive values if new model object is uploaded
    model_type(NULL)
    model_algorithm(NULL)
    model_hyperparams(NULL)
    num_predictors(NULL)
    names_predictors(NULL)
    num_training_samples(NULL)
    num_classes(NULL)
    num_samples_per_class(NULL)
    interpolation_range(NULL)
    
    
    # read model --------
    model <- tryCatch({
      readRDS(input$model_upload$datapath)
    }, error = function(e) {
      showNotification("Invalid RDS file or failed to load model.", type = "error")
      return(NULL)
    })

    # --------------------------------------
    # Caret model
    # --------------------------------------
    if ("train" %in% class(model)) {
      if (!is.null(model$trainingData)) {
        num_training_samples(nrow(model$trainingData))
        num_predictors(ncol(model$trainingData) - 1)
        names_predictors(paste(names(model$trainingData[, -1]), collapse = ", "))
      }
      
      if (!is.null(model$bestTune)) {
        model_hyperparams(paste0(names(model$bestTune), "=", model$bestTune, collapse = ","))
      }
      
      model_algorithm(model$method)
      
      if (!is.null(model$modelType)) {
        model_type(model$modelType)
      } else {
        response <- model$trainingData[, 1]
        model_type(if (is.factor(response)) "Classification" else "Regression")
      }
      
      if (model_type() == "Classification") {
        num_classes(length(unique(model$trainingData$.outcome)))
        num_samples_per_class(paste0(names(table(model$trainingData$.outcome)), ": ", table(model$trainingData$.outcome), collapse = ", "))
      } else {
        interpolation_range(paste(round(range(model$trainingData$.outcome), 3), collapse = " to "))
      }
      
      # --------------------------------------
      # Tidymodels (workflows, parsnip fits)
      # --------------------------------------
    } else if ("workflow" %in% class(model) || "model_fit" %in% class(model)) {
      fit <- if ("workflow" %in% class(model)) extract_fit_parsnip(model) else model
      data <- tryCatch(extract_mold(model), error = function(e) NULL)
      
      if (!is.null(fit)) {
        model_algorithm(fit$spec$engine)
        model_type(fit$spec$mode)
        
        if (!is.null(fit$spec$args)) {
          model_hyperparams(paste0(names(fit$spec$args), "=", unlist(fit$spec$args), collapse = ","))
        }
      }
      
      if (!is.null(data)) {
        num_training_samples(nrow(data$outcomes))
        num_predictors(ncol(data$predictors))
        names_predictors(paste(names(data$predictors), collapse = ", "))
        if (is.factor(data$outcomes[[1]])) {
          num_classes(length(unique(data$outcomes[[1]])))
          num_samples_per_class(paste0(names(table(data$outcomes[[1]])), ": ", table(data$outcomes[[1]]), collapse = ", "))
        } else {
          interpolation_range(paste(round(range(data[[1]]), 3), collapse = " to "))
        }
      }
      # --------------------------------------
      # mlr3 models
      # --------------------------------------
    } else if ("Learner" %in% class(model) || "GraphLearner" %in% class(model)) {
      if (!is.null(model$predict_type)) {
        model_type(if (grepl("classif", model$id)) "Classification" else "Regression")
      }
      
      model_algorithm(model$label)
      
      if (!is.null(model$param_set$values)) {
        model_hyperparams(paste0(names(model$param_set$values), "=", unlist(model$param_set$values), collapse = ","))
      }
      
      if (!is.null(model$training_data)) {
        target <- model$training_data[[model$task$target_names]]
        num_training_samples(nrow(model$training_data))
        num_predictors(ncol(model$training_data) - 1)
        names_predictors(paste(setdiff(names(model$training_data), model$task$target_names), collapse = ", "))
        
        if (is.factor(target)) {
          num_classes(length(unique(target)))
          num_samples_per_class(paste0(names(table(target)), ": ", table(target), collapse = ", "))
        } else {
          interpolation_range(paste(round(range(target), 3), collapse = " to "))
        }
      } else {
        
        num_training_samples(model$model$num.samples)
        num_predictors(length(model$model$forest$independent.variable.names))
        names_predictors(paste(model$model$forest$independent.variable.names, collapse = ", "))
        
        if (isTRUE(grepl("classif", model$id))) {
          num_classes(length(model$model$forest$levels))
        } 
        
      }
    } else {
      showNotification("Unsupported model type uploaded. Supported: caret, tidymodels, mlr3.", type = "error")
    }
    
  })
  
  # Render UI for extracted model information
  render_n_samples = function(element_id, element_placeholder) {
      if (!is.null(num_training_samples())) {
        tagList(
          numericInput("d_response_3", "Number of Training Samples", value = num_training_samples()),
        )
      } else {
        numericInput("d_response_3", "Number of Training Samples", value = NULL)
      }
  }
  
  render_n_predictors = function(element_id, element_placeholder) {
    if (!is.null(num_predictors())) {
      tagList(
        numericInput("d_predictors_2", "Number of Predictors", value = num_predictors()),
      )
    } else {
      numericInput("d_predictors_2", "Number of Predictors", value = NULL)
    }
  }
  
  render_n_classes = function(element_id, element_placeholder) {
    if (!is.null(num_classes())) {
      tagList(
        numericInput("d_response_4", "Number of Classes", value = num_classes()),
      )
    } else {
      numericInput("d_response_4", "Number of Classes", value = NULL)
    }
  }
  
  render_n_samples_class = function(element_id, element_placeholder) {
    if (!is.null(num_samples_per_class())) {
      tagList(
        textInput("d_response_5", "Number of Samples per Class", value = num_samples_per_class()),
      )
    } else {
      textInput("d_response_5", "Number of Samples per Class", value = NULL)
    }
  }
  
  render_range = function(element_id, element_placeholder) {
    if (!is.null(interpolation_range())) {
      tagList(
        textInput("d_response_6", "Response range", value = interpolation_range()),
      )
    } else {
      textInput("d_response_6", "Response range", value = NULL)
    }
  }
  
  render_names_predictors = function(element_id, element_placeholder) {
    if (!is.null(names_predictors())) {
      tagList(
        textInput("d_predictors_3", "Names of Predictors", value = names_predictors()),
      )
    } else {
      textInput("d_predictors_3", "Names of Predictors", value = NULL)
    }
  } 
  
  render_hyperparameters = function(element_id, element_placeholder) {
    if (!is.null(model_hyperparams())) {
      tagList(
        textInput("m_validation_3", "Hyperparameter values", value = model_hyperparams()),
      )
    } else {
      textInput("m_validation_3", "Hyperparameter values", value = NULL)
    }
  } 
  
  render_model_type = function(element_id, element_placeholder) {
    selectInput("m_algorithms_1", "Model Type",
                choices = c("", "Classification", "Regression"),
                selected = model_type())
    
  }
  
  render_model_algorithm = function(element_id, element_placeholder) {
    # Default list of known algorithms
    default_algos <- c("rf", "gbm", "glm", "svmRadial", "nnet", "rpart")
    selected_algo <- model_algorithm()
    
    # Ensure the selected algorithm is in the choices
    algo_choices <- if (!is.null(selected_algo) && !(selected_algo %in% default_algos)) {
      c(default_algos, selected_algo)
    } else {
      default_algos
    }
    
    selectInput(
        "m_algorithms_2", "Algorithm",
        choices = c("", unique(algo_choices)),
        selected = selected_algo
      )
    
  }
  
  observeEvent(model_type(), {
    if (!is.null(model_type()) && !is.null(model_algorithm()) && !is.null(num_training_samples())) {
      showNotification("Model details filled from uploaded .RDS", type = "message")
    }
  })
  
  
  ## Extract information from uploaded spatial objects ----------
  samples_crs <- reactiveVal(NULL)
  auto_selected <- reactiveVal(NULL)
  samples_valid <- reactiveVal(TRUE)
  trainArea_valid <- reactiveVal(TRUE)
  prediction_valid <- reactiveVal(TRUE)
  
  # check if training area is valid
  observeEvent(input$trainArea_upload, {
    req(input$trainArea_upload)
    
    trainArea_valid(TRUE)  # reset
    auto_selected(NULL)
    
    trainArea <- tryCatch({
      st_read(input$trainArea_upload$datapath, quiet = TRUE)
    }, error = function(e) {
      showNotification("Could not read prediction area file.", type = "error")
      return(NULL)
    })
    
    if (!is.null(trainArea)) {
      geom_type <- unique(st_geometry_type(trainArea))
      if (!all(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
        showNotification("Prediction area must contain only POLYGON geometries.", type = "error")
        trainArea_valid(FALSE)
      }
    }
  })
  
  # check if prediction area is valid
  observeEvent(input$prediction_upload, {
    req(input$prediction_upload)
    
    prediction_valid(TRUE)  # reset
    auto_selected(NULL)
    
    prediction_area <- tryCatch({
      st_read(input$prediction_upload$datapath, quiet = TRUE)
    }, error = function(e) {
      showNotification("Could not read prediction area file.", type = "error")
      return(NULL)
    })
    
    if (!is.null(prediction_area)) {
      geom_type <- unique(st_geometry_type(prediction_area))
      if (!all(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
        showNotification("Prediction area must contain only POLYGON geometries.", type = "error")
        prediction_valid(FALSE)
      }
    }
  })
  
  # check if samples are valid
  observeEvent(input$samples_upload, {
    req(input$samples_upload)
    
    samples_valid(TRUE)  # reset
    auto_selected(NULL)
    
    samples <- tryCatch({
      st_read(input$samples_upload$datapath, quiet = TRUE)
    }, error = function(e) {
      showNotification("Could not read samples file.", type = "error")
      return(NULL)
    })
    
    if (!is.null(samples)) {
      geom_type <- unique(st_geometry_type(samples))
      if (!all(geom_type %in% c("POINT", "MULTIPOINT"))) {
        showNotification("Samples file must contain only POINT geometries.", type = "error")
        samples_valid(FALSE)
      }
    }
  })
  
  # When samples are uploaded, extract epsg string
  observeEvent(input$samples_upload, {
    
    req(samples_valid())
    req(input$samples_upload)
    
    # Reset reactive values if new model object is uploaded
    samples_crs(NULL)
    
    # read data --------
    samples <- tryCatch({
      st_read(input$samples_upload$datapath)
    }, error = function(e) {
      showNotification("Invalid .gpkg file or failed to load model.", type = "error")
      return(NULL)
    })
    
    # Check if samples are a POINT geometry
    samples_crs(st_crs(samples)$epsg)
      
  })
  
  
  render_crs = function(element_id, element_placeholder) {
    textInput(
      inputId = element_id,
      label = "Coordinate Reference System (epsg)",
      value = samples_crs()  # NULL if unset
    )
  } 
  
  render_design = function(element_id, element_placeholder){
    
    req(samples_valid(), prediction_valid())
    
    auto_val <- NULL
    
    if (!is.null(input$samples_upload) && !is.null(input$prediction_upload)) {
      samples <- tryCatch({
        st_read(input$samples_upload$datapath, quiet = TRUE)
      }, error = function(e) NULL)
      
      prediction_area <- tryCatch({
        st_read(input$prediction_upload$datapath, quiet = TRUE)
      }, error = function(e) NULL)
      
      if (!is.null(samples) && !is.null(prediction_area)) {
        
        samples <- st_transform(samples, st_crs(prediction_area))
        
        # sampling design
        geod <- CAST::geodist(samples, modeldomain = prediction_area)

        Gj <- geod[geod$what == "sample-to-sample",]$dist
        Gij <- geod[geod$what == "prediction-to-sample",]$dist
        
        testks <- suppressWarnings(stats::ks.test(Gj, Gij, alternative = "great"))
        
        if(testks$p.value >= 0.05) {
          auto_val <- "random"
          auto_selected("random")
        } else {
          auto_val <- "clustered"
          auto_selected("clustered")
        }
        
      }
    }
    
    observe({
      if (!is.null(auto_selected())) {
        showNotification(paste("Sampling design was automatically set to:", auto_selected()),
                         type = "message", duration = 5)
      }
    })
    
    # Create dropdown menu
    if (is.null(auto_val)) {
      # No preselection
      selectInput("sampling_design", "Sampling Design",
                  choices = c("", "clustered", "random", "stratified"),
                  selected = "")
    } else {
      # Preselect inferred value
      selectInput("sampling_design", "Sampling Design",
                  choices = c("clustered", "random", "stratified"),
                  selected = auto_val)
    }
  }

  
  ## Render other stuff -----------
  render_suggestion = function(element_id, element_placeholder, suggestions){
    suggestions = sort(trimws(unlist(strsplit(suggestions, ","))))
    selectizeInput(inputId = element_id, label = element_placeholder, choices = suggestions, multiple = TRUE, options = list(create = T,  placeholder = "Choose from list or insert new values"))
  }

  

  render_suggestion_single = function(element_id, element_placeholder, suggestions){
    suggestions = sort(trimws(unlist(strsplit(suggestions, ","))))
    suggestions = suggestions[suggestions != ""]  # Remove blanks
    
    selectizeInput(
      inputId = element_id,
      label = element_placeholder,
      choices = suggestions,
      selected = character(0),  # Ensures nothing is selected -- doesnt work!!!
      multiple = FALSE,
      options = list(
        create = TRUE,
        placeholder = "Choose from list or insert new values"
      )
    )
  }
  
  render_model_settings = function(){
    div(
      em(p("Edit fields by double clicking in the table. Add new settings with the plus sign.", style = "font-weight: 300;")),
      tabsetPanel(id = "settings_tabset"),
      actionButton("add_setting", label = NULL, icon = icon("plus")),
      br(),br()
    )
  }
  
  render_section = function(section, subsection, odmap_dict){
    
    if(!is.null(subsection)) {
      section_dict = filter(odmap_dict, section == !!section & .data$subsection %in% !!subsection) 
    } else {
      section_dict = filter(odmap_dict, section == !!section) 
    }
    
    section_rendered = renderUI({
      section_UI_list = vector("list", nrow(section_dict)) # holds UI elements for all ODMAP elements belonging to 'section'
      subsection = ""
      for(i in 1:nrow(section_dict)){
        element_UI_list = vector("list", 3) # holds UI elements for current element 
        
        # First element: Header 
        if(subsection != section_dict$subsection_id[i]){
          subsection = section_dict$subsection_id[i]
          subsection_label = section_dict$subsection[i]
          element_UI_list[[1]] = div(id = section_dict$subsection_id[i], h5(subsection_label, style = "font-weight: bold"))
        }
        
        # Second element: Input field(s) 
        element_UI_list[[2]] = switch(section_dict$element_type[i],
                                      text = render_text(section_dict$element_id[i], section_dict$element_placeholder[i]),
                                      author = render_authors(),
                                      objective = render_objective(section_dict$element_id[i], section_dict$element_placeholder[i]),
                                      
                                      sample_size = render_n_samples(section_dict$element_id[i], section_dict$element_placeholder[i]),
                                      n_predictors = render_n_predictors(section_dict$element_id[i], section_dict$element_placeholder[i]),
                                      names_predictors = render_names_predictors(section_dict$element_id[i], section_dict$element_placeholder[i]),
                                      
                                      n_classes = render_n_classes(section_dict$element_id[i], section_dict$element_placeholder[i]),
                                      n_samples_per_class = render_n_samples_class(section_dict$element_id[i], section_dict$element_placeholder[i]),
                                      
                                      interpolation_range = {
                                        if (!is.null(model_type()) && model_type() == "Classification") {
                                          NULL  # Hide interpolation_range for classification models
                                        } else {
                                          render_range(section_dict$element_id[i], section_dict$element_placeholder[i])
                                        }},
                                        
                                      hyperparams = render_hyperparameters(section_dict$element_id[i], section_dict$element_placeholder[i]),

                                      model_type = render_model_type(section_dict$element_id[i], section_dict$element_placeholder[i]),
                                      model_algorithm = render_model_algorithm(section_dict$element_id[i], section_dict$element_placeholder[i]),
                                      
                                      sampling_design = render_design(section_dict$element_id[i], section_dict$element_placeholder[i]),
                                      samples_crs = render_crs(section_dict$element_id[i], section_dict$element_placeholder[i]),
                                      
                                      suggestion_single = render_suggestion_single(section_dict$element_id[i], section_dict$element_placeholder[i], section_dict$suggestions[i]),
                                      suggestion = render_suggestion(section_dict$element_id[i], section_dict$element_placeholder[i], section_dict$suggestions[i]),
                                      model_setting = render_model_settings())
        
        # Third element: Next/previous button
        if(i == nrow(section_dict)){
          # TODO add next/previous buttons
        }
        
        # Reduce list to non-empty elements
        element_UI_list = Filter(Negate(is.null), element_UI_list)
        section_UI_list[[i]] = element_UI_list
      }
      return(section_UI_list)
    })
    return(section_rendered)
  }
  
  
  # ------------------------------------------------------------------------------------------#
  #                            Rendering functions for Markdown Output                        # 
  # ------------------------------------------------------------------------------------------#
  # Functions for dynamically knitting text elements
  knit_section = function(section_id){
    section = unique(odmap_dict$section[which(odmap_dict$section_id == section_id)])
    cat("\n\n##", section, "\n")
  }
  
  knit_subsection= function(subsection_id){
    # Get all elements
    element_ids = odmap_dict$element_id[which(odmap_dict$subsection_id == subsection_id)]
    subsection = unique(odmap_dict$subsection[which(odmap_dict$subsection_id == subsection_id)])
    
    # Find out whether subsection needs to be rendered at all
    # Are all elements optional?
    all_optional = all((element_ids %in% elem_hide[[input$o_objective_1]] | element_ids %in% elem_optional))
    
    # If not, render header
    if(!all_optional){
      cat("\n\n####", subsection, "\n")
    } else { # if not, render header only when user provided optional inputs
      all_empty = T
      for(id in element_ids){
        if(input[[id]] != ""){
          all_empty = F
          break
        }
      }
      if(!all_empty){
        cat("\n\n####", subsection, "\n")
      }
    }
  }
  
  knit_text = function(element_id){
    if(input[[element_id]] == ""){
      knit_missing(element_id)
    } else {
      element_name = odmap_dict$element[which(odmap_dict$element_id == element_id)]
      cat("\n", element_name, ": ", input[[element_id]], "\n", sep="")
    }
  }
  
  knit_authors = function(element_id){
    paste(authors$df$first_name, authors$df$last_name, collapse = ", ")
  }
  
  knit_suggestion = function(element_id){
    if(is.null(input[[element_id]])){
      knit_missing(element_id)
    } else {
      element_name = odmap_dict$element[which(odmap_dict$element_id == element_id)]
      cat("\n", element_name, ": ", paste(input[[element_id]], collapse = ", "), "\n", sep="")
    }
  }
  
  knit_model_settings = function(element_id){
    if(is.null(input[["o_algorithms_1"]])){
      knit_missing(element_id)
    } else {
      for(alg in input[["o_algorithms_1"]]){
        settings_tab = model_settings[[alg]] %>% filter(value != "")
        if(nrow(settings_tab) == 0) {
          cat("\n\n <span style='color:#DC3522'>\\<", alg, "\\> </span>\n", sep = "")
        } else {
          settings_char = paste0(settings_tab$setting, " (", settings_tab$value, ")", collapse = ", ")
          cat("\n", alg, ": ", settings_char, "\n", sep="")
        }
      }
    }
  }
  
  knit_missing = function(element_id){
    if(!(element_id %in% elem_hide[[input$o_objective_1]] | element_id %in% elem_optional)){
      placeholder = odmap_dict$element[which(odmap_dict$element_id == element_id)]
      cat("\n\n <span style='color:#DC3522'>\\<", placeholder, "\\> </span>\n", sep = "")
    }
  }
  
  # ------------------------------------------------------------------------------------------#
  #                                   Import functions                                        # 
  # ------------------------------------------------------------------------------------------#
  # ODMAP import functions
  import_odmap_to_text = function(element_id, values){
    if(input[[element_id]] == "" | input[["replace_values"]] == "Yes"){
      updateTextAreaInput(session = session, inputId = element_id, value = values)  
    }
  }
  
  import_odmap_to_authors = function(element_id, values){
    if(nrow(authors$df) == 0 | input[["replace_values"]] == "Yes"){
      names_split = unlist(strsplit(values, split = "; "))
      names_split = regmatches(names_split, regexpr(" ", names_split), invert = TRUE)
      authors$df = authors$df[0,] # Delete previous entries
      for(i in 1:length(names_split)){
        author_tmp = names_split[[i]]
        authors$df = rbind(authors$df, data.frame("first_name" = author_tmp[1],  "last_name" = author_tmp[2]))
      }
    }
  }
  
  import_odmap_to_model_objective = function(element_id, values){
    if(input[[element_id]] == "" | input[["replace_values"]] == "Yes"){
      updateSelectizeInput(session = session, inputId = "o_objective_1", selected = values)
    }
  }
  
  import_odmap_to_model_algorithm = function(element_id, values){
    if(length(input[[element_id]]) == 0 | input[["replace_values"]] == "Yes"){
      values = unlist(strsplit(values, split = "; "))
      suggestions_new =  sort(trimws(c(model_settings$suggestions, as.character(values))))
      updateSelectizeInput(session = session, inputId = element_id, choices = suggestions_new, selected = values)  
    }
  }
  
  # Generic import functions
  import_model_settings = function(element_id, values){
    settings_all = unlist(strsplit(values, split = "; ",))
    algorithms = c()
    for(settings_tmp in settings_all){
      if(grepl("no settings provided", settings_tmp)){next}
      settings_split = unlist(regmatches(settings_tmp, regexpr(": ", settings_tmp), invert = TRUE)) # split at first instance of ":"
      alg = settings_split[1]
      algorithms = c(algorithms, alg)
      values_indices = gregexpr("\\((?>[^()]|(?R))*\\)", settings_split[2], perl = T) # indices of model settings in parentheses and string length per setting
      values_start = unlist(values_indices) + 1
      values_end = values_start + attr(values_indices[[1]], "match.length") - 3
      settings_start = c(1, values_end[-length(values_end)] + 4)
      settings_end = c(values_start - 3)
      values_extr = substring(settings_split[2], values_start, values_end)
      settings_extr = substring(settings_split[2], settings_start, settings_end)
      settings_df = data.frame(setting = settings_extr, value = values_extr, stringsAsFactors = F)
      model_settings_import[["algorithms"]] = c(model_settings_import[["algorithms"]], alg)
      model_settings_import[[alg]] = settings_df
    }
    updateSelectizeInput(session = session, inputId = "o_algorithms_1", selected = algorithms)
  }
  
  import_suggestion = function(element_id, values){
    if(length(input[[element_id]]) == 0 | input[["replace_values"]] == "Yes"){
      values = trimws(unlist(strsplit(values, split = ";")))
      suggestions = unlist(strsplit(odmap_dict$suggestions[odmap_dict$element_id == element_id], ","))
      suggestions_new =  sort(trimws(c(suggestions, as.character(values))))
      updateSelectizeInput(session = session, inputId = paste0(element_id), choices = suggestions_new, selected = as.character(values))
    }
  }
  
  # ------------------------------------------------------------------------------------------#
  #                                     Export functions                                      # 
  # ------------------------------------------------------------------------------------------#
  export_standard = function(element_id){
    val = input[[element_id]]
    return(ifelse(!is.null(val), val, NA))
  }
  
  export_authors = function(element_id){
    return(ifelse(nrow(authors$df) > 0, paste(authors$df$first_name, authors$df$last_name, collapse = "; "), NA))
  }
  
  export_suggestion = function(element_id){
    val = input[[element_id]]
    return(ifelse(!is.null(val), paste(input[[element_id]], collapse = "; "), NA))
  }
  
  export_model_setting = function(element_id){
    if(is.null(input[["o_algorithms_1"]])){
      return(NA)
    } else {
      settings = c()
      for(alg in input[["o_algorithms_1"]]){
        settings_tab = model_settings[[alg]] %>% filter(value != "")
        if(nrow(settings_tab) == 0) {
          settings[alg] = paste0(alg, ": no settings provided")
        } else {
          settings_char = paste0(settings_tab$setting, " (", settings_tab$value, ")", collapse = ", ")
          settings[alg] = paste0(alg, ": ", settings_char)
        }
      }
      return(paste0(settings, collapse = "; "))
    }
  }
  
  # ------------------------------------------------------------------------------------------#
  #                                   UI Elements                                             # 
  # ------------------------------------------------------------------------------------------#
  # "Create a protocol" - mainPanel elements
  output$Overview_UI = render_section("Overview", NULL, odmap_dict)
  output$Model_UI_response <- render_section("Model", "Response", odmap_dict)
  output$Model_UI_predictor <- render_section("Model", "Predictors", odmap_dict)
  output$Model_UI_algorithms <- render_section("Model", "Learning method", odmap_dict)
  output$Model_UI_validation <- render_section("Model", c("Parameter uncertainty and biases", "Model validation and selection"), odmap_dict)
  output$Model_UI_interpretation <- render_section("Model", "Model explainability", odmap_dict)
  output$Model_UI_Software <- render_section("Model", "Software", odmap_dict)
  
  output$Prediction_UI_area = render_section("Prediction", "Prediction domain", odmap_dict)
  output$Prediction_UI_eval = render_section("Prediction", "Evaluation and Uncertainty", odmap_dict)
  output$Prediction_UI_post = render_section("Prediction", "Post-Processing", odmap_dict)
  
  for(tab in c("Overview_UI", "Model_UI_response", "Model_UI_predictor", "Prediction_UI_area", "Prediction_UI_eval", "Prediction_UI_post")){
    outputOptions(output, tab, suspendWhenHidden = FALSE) # Add tab contents to output object before rendering
  } 
  
  # -------------------------------------------
  # "Create a protocol" - sidebarPanel elements
  get_progress = reactive({
    progress = c()
    for(sect in unique(odmap_dict$section)){
      all_elements = odmap_dict %>% 
        filter(section == sect & !element_id %in% unlist(elem_hidden) & !element_type %in% c("model_setting", "author")) %>% 
        filter(if(input$hide_optional) !element_id %in% elem_optional else T) %>% 
        pull(element_id)
      if(length(all_elements) == 0){
        next 
      } else {
        completed_elements = sum(sapply(all_elements, function(x){
          !(identical(input[[x]], "") | identical(input[[x]], 0) | identical(input[[x]], NULL))
        }, USE.NAMES = T, simplify = T))
        progress[sect] = (sum(completed_elements) / length(all_elements)) * 100
      }
    }
    return(progress)
  }) 
  
  output$progress_bars = renderUI({
    progress = get_progress()
    progress_UI_list = lapply(names(progress), function(sect){
      progressBar(paste("progress", sect, sep = "_"), value = progress[sect], title = sect)
    })
    return(progress_UI_list)
  })
  
  output$protocol_download = downloadHandler(
    filename = function(){
      author_list = authors$df$last_name
      if(length(author_list) > 2){
        name_string = paste0(author_list[1],"_EtAl")
      } else if(length(author_list) == 2){
        name_string = paste0(author_list[1], author_list[2])
      } else if(length(author_list) == 1){
        name_string = author_list[1]
      } else {
        name_string = "Anonymous"
      }
      paste0("ODMAP_", name_string, "_", Sys.Date(), ".", input$document_format)
    },
    content = function(file){
      odmap_download = odmap_dict %>% 
        filter(!element_id %in% elem_hide[[input$o_objective_1]]) %>% # use only relevent rows
        dplyr::select(section, subsection, element, element_id, element_type) %>% 
        mutate(Value = NA)
      
      # Create .csv-files
      if(input$document_format == "csv"){
        # Create table
        for(i in 1:nrow(odmap_download)){
          odmap_download$Value[i] = switch(odmap_download$element_type[i],
                                           text = export_standard(odmap_download$element_id[i]),
                                           author = export_authors(odmap_download$element_id[i]),
                                           objective = export_standard(odmap_download$element_id[i]),
                                           suggestion = export_suggestion(odmap_download$element_id[i]),
                                           model_algorithm = export_suggestion(odmap_download$element_id[i]),
                                           model_setting = export_model_setting(odmap_download$element_id[i]),
                                           "") 
        }
        
        odmap_download$element_id = NULL
        odmap_download$element_type = NULL
        
        # Write output
        file_conn = file(file, open = "w")
        write.csv(odmap_download, file = file_conn, na = "", row.names = F)
        close(file_conn)
        
        # CREATE WORD FILES
      } else {
        src <- normalizePath("protocol_output.Rmd")
        
        # temporarily switch to the temp dir, in case of missing write permission in the current working directory
        wd_orig <- setwd(tempdir())
        on.exit(setwd(wd_orig))
        file.copy(src, "protocol_output.Rmd", overwrite = TRUE)
        odmap_download = rmarkdown::render("protocol_output.Rmd", rmarkdown::word_document(),
                                           params = list(study_title = paste(input$o_authorship_1), authors = paste(authors$df$first_name, authors$df$last_name, collapse = ", ")))
        file.rename(odmap_download, file)
      }
    }
  )
  
  # -------------------------------------------
  # "Protocol Viewer"
  output$markdown = renderUI({includeMarkdown(knitr::knit("protocol_preview.Rmd", quiet = T))})
  
  # -------------------------------------------
  # "Upload / Import"
  output$Upload_UI = renderUI({
    UI_list = list()
    if(!is.null(input$upload)){
      # Obtain file extension
      file_type = gsub( "(^.*)(\\.[A-z]*$)", replacement = "\\2", input$upload$datapath)
      if(!file_type %in% c(".csv", ".RDS")){
        showNotification("Please select and provide a .csv (ODMAP, RMMS) or .RDS file (RMMS).", duration = 3, type = "error")
        reset("upload")
        return()
      }
      
      # Read in file
      if(file_type == ".RDS"){
        tryCatch({
          protocol_upload = rmmToCSV(protocol_upload, input$upload$datapath)
        }, error = function(e){
          showNotification("Could not read file.", duration = 3, type = "error")
          reset("upload")
          return()
        })
      } else {
        protocol_upload = read.csv(file = input$upload$datapath, header = T, sep = ",", stringsAsFactors = F, na.strings = c("NA", "", "NULL"))
      }
      
      # Identify protocol type
      if(all(c("section", "subsection", "element", "Value") %in% colnames(protocol_upload))){
        protocol_type = "ODMAP"
      } else if(all(c("Field.1", "Field.2", "Field.3", "Entity", "Value") %in% colnames(protocol_upload))){
        protocol_type = "RMMS"
      } else {
        showNotification("Please select a valid ODMAP or RMMS file", duration = 3, type = "error")
        reset("upload")
        return()
      }
      
      if(sum(!is.na(protocol_upload$Value))>0){
        UI_list[[1]] = p(paste0("File: ", input$upload$name, " (", protocol_type, " protocol, ", sum(!is.na(protocol_upload$Value)), " non-empty fields)"))
        UI_list[[2]] = radioButtons("replace_values", "Overwrite non-empty fields with uploaded values?", choices = c("Yes", "No"), selected = "No")
        UI_list[[3]] = actionButton(paste0(protocol_type, "_to_input"), "Copy to input form")
      } else{
        showNotification("Please select a ODMAP or RMMS file with at least one non-empty field", duration = 5, type = "error")
      }
      
    }
    return(UI_list)
  })
  
  # Monitor current progress
  elem_hide = list("Inference and explanation" = c(pull(odmap_dict %>% filter(inference == 0), element_id), # unused elements
                                                   unique(pull(odmap_dict %>% group_by(subsection_id) %>% filter(all(inference  == 0)), subsection_id)), # unused subsections
                                                   "p"),
                   "Prediction and mapping" =  c(pull(odmap_dict %>% filter(prediction == 0), element_id), 
                                                 unique(pull(odmap_dict %>% group_by(subsection_id) %>% filter(all(prediction == 0)), subsection_id))))
  
  elem_optional = c(pull(odmap_dict %>% filter(optional == 1), element_id), # optional elements
                    unique(pull(odmap_dict %>% group_by(subsection_id) %>% filter(all(optional == 1)), subsection_id))) # optional subsections)
  
  elem_hidden = "" # keep track of hidden elements
  
  # ------------------------------------------------------------------------------------------#
  #                                      Event handlers                                       # 
  # ------------------------------------------------------------------------------------------#
  # Study objective
  observeEvent(input$o_objective_1, {
    # Dynamically show/hide corresponding input fields
    shinyjs::show(selector = paste0("#", setdiff(elem_hidden, elem_hide[[input$o_objective_1]])))
    shinyjs::hide(selector = paste0("#", elem_hide[[input$o_objective_1]]))
    elem_hidden <<- elem_hide[[input$o_objective_1]]
    
    # Show/hide Prediction tab when study objective is inference
    if(input$o_objective_1 == "Inference and explanation"){
      hideTab("tabset", "Prediction")
    } else {
      showTab("tabset", "Prediction")
    }
  })
  
  
  # -------------------------------------------
  # Warning message for inappropriate CV strategy
  observe({
    req(input$sampling_design, input$m_validation_1)
    
    if (input$sampling_design == "clustered" && input$m_validation_1 == "Random Cross-Validation") {
      showNotification(
        "⚠️ Warning: Random CV with clustered samples likely results in unreliable error estimates. Use a spatial/target-oriented CV instead.",
        type = "warning", duration = 8
      )
    }
  })
  
  observe({
    req(input$sampling_design, input$m_validation_1)
    
    if (input$sampling_design == "random" && input$m_validation_1 == "Spatial Cross-Validation") {
      showNotification(
        "⚠️ Warning: Spatial CV with random samples likely results in unreliable error estimates. Use a random/target-oriented CV instead.",
        type = "warning", duration = 8
      )
    }
  })
  
  
  observe({
    req(input$sampling_design, input$p_eval_3)
    if (input$sampling_design == "clustered" && input$p_eval_3 == "None") {
      showNotification("⚠️ Warning: Clustered samples often lead to extrapolation when the model is applied to feature combinations not present in the training data.
                         Identifying areas of extrapolation/uncertainty and communicating them to the user of the prediction is recommended.", type = "warning")
    }
    
  })
  
  
  observe({
    req(input$sampling_design, input$d_predictors_1)
    if (input$sampling_design == "clustered" && "Spatial Proxies" %in% input$d_predictors_1) {
        showNotification("⚠️ Warning: Using spatial proxies with clustered samples likely leads to extrapolation situations.\nYou might
                         consider using physically relevant predictors instead.", type = "warning")
      }
  })
  
  
  
  
  # -------------------------------------------
  # Optional fields
  observeEvent(input$hide_optional,{
    if(is.null(input$o_objective_1)){
      return(NULL)
    } else if(input$hide_optional == T & input$o_objective_1 == ""){
      showNotification("Please select a model objective under '1. Overview'", duration = 3, type = "message")
      Sys.sleep(0.3)
      updateMaterialSwitch(session, "hide_optional", value = F)
      updateTabsetPanel(session, "tabset", "Overview")
      # TODO jump to Input field
    } else {
      shinyjs::toggle(selector = paste0("#", setdiff(elem_optional, elem_hide[[input$o_objective_1]])), condition = !input$hide_optional) 
    }
  })
  
  # -------------------------------------------
  # Model algorithms and settings
  model_settings = reactiveValues(suggestions = rmm_dict %>% filter(field1 == "model" & field2 == "algorithm") %>% pull(field3) %>% unique() %>% trimws(),
                                  settings_tabset = NULL)
  model_settings_import = reactiveValues(algorithms = character(0))
  
  observeEvent(input$o_algorithms_1, {
    if(length(input$o_algorithms_1) > length(model_settings$settings_tabset)) { # New algorithm selected
      new_algs = setdiff(input$o_algorithms_1, model_settings$settings_tabset)
      for(new_alg in new_algs){
        # Create dataframe for new algorithm
        if(new_alg %in% model_settings_import[["algorithms"]]){
          model_settings[[new_alg]] = model_settings_import[[new_alg]]
        } else if(new_alg %in% filter(rmm_dict, field2 == "algorithm")$field3){
          model_settings[[new_alg]] = rmm_dict %>%
            filter(field1 == "model" & field2 == "algorithm" & field3 == new_alg) %>%
            mutate(setting = entity, value = as.character(NA)) %>%
            dplyr::select(setting, value)
        } else {
          model_settings[[new_alg]] = data.frame(setting = character(0), value = character(0))
        }
        
        # Add new dataframe to output and settings_tabset
        local({ # Needs local evaluation because of asynchronous execution of renderDataTable
          .new_alg = new_alg
          output[[.new_alg]] = renderDataTable(model_settings[[.new_alg]], editable = T, rownames = F,
                                        options = list(dom = "t", pageLength = 50, autoWidth = T, columnDefs = list(list(width = '50%', targets = "_all"))))
          observeEvent(input[[paste0(.new_alg, '_cell_edit')]], {
            model_settings[[.new_alg]][input[[paste0(.new_alg, '_cell_edit')]]$row, input[[paste0(.new_alg, '_cell_edit')]]$col + 1] = input[[paste0(.new_alg, '_cell_edit')]]$value
          })
        })
        appendTab(inputId = "settings_tabset", select = T, tab = tabPanel(title = new_alg, value = new_alg, dataTableOutput(outputId = new_alg)))
      }
      model_settings$settings_tabset = input$o_algorithms_1 # update name list of displayed tabs
    } else {
      hide_alg = setdiff(model_settings$settings_tabset, input$o_algorithms_1)
      removeTab(inputId = "settings_tabset", target = hide_alg)
      model_settings$settings_tabset = input$o_algorithms_1
    }
    
    if(length(model_settings$settings_tabset) > 0){
      updateTabsetPanel(session, "settings_tabset", selected = model_settings$settings_tabset[1])
    }
  }, ignoreNULL = F, ignoreInit = F, priority = 1)
  
  observeEvent(input$add_setting, {
    if(!is.null(input$settings_tabset)){
      empty_row = data.frame(setting = NA, value = NA)
      model_settings[[input$settings_tabset]] = rbind(model_settings[[input$settings_tabset]], empty_row)
    } else {
      showNotification("Please select or add a model algorithm under '1. Overview'", duration = 3, type = "message")
      updateTabsetPanel(session, "tabset", selected = "Overview")
      # TODO jump to Input field
    }
  })
  
  # -------------------------------------------
  # Authors
  authors = reactiveValues(df = data.frame("first_name" = character(0),  "last_name" = character(0))) 
  
  output$authors_table = DT::renderDataTable({
    if(nrow(authors$df) == 0){
      authors_dt = datatable(authors$df, escape = F, rownames = F, colnames = NULL, 
                             options = list(dom = "t", ordering = F, language = list(emptyTable = "Author list is empty"), columnDefs = list(list(className = 'dt-left', targets = "_all"))))
    } else {
      authors_tmp = authors$df %>% 
        rownames_to_column("row_id") %>% 
        mutate(row_id = as.numeric(row_id),
               delete = sapply(1:nrow(.), function(row_id){as.character(actionButton(inputId = paste("remove_author", row_id, sep = "_"), label = NULL, 
                                                                                     icon = icon("trash"),
                                                                                     onclick = 'Shiny.setInputValue(\"remove_author\", this.id, {priority: "event"})'))})) %>% 
        dplyr::select(-row_id)
      
      authors_dt = datatable(authors_tmp, escape = F, rownames = F, editable = T, colnames = c("First name", "Last name", ""), 
                             options = list(dom = "t", autoWidth = F, 
                                            columnDefs = list(list(width = '10%', targets = c(2)), 
                                                              list(width = '45%', targets = c(0:1)),
                                                              list(orderable = F, targets = c(0:2)))))
    }
    return(authors_dt)
  })
  
  observeEvent(input$add_author, {
    showModal(
      modalDialog(title = "Add new author", footer = NULL, easyClose = T,
                  textInput("first_name", "First name"),
                  textInput("last_name", "Last name"),
                  actionButton("save_new_author", "Save")
      )
    )
  })
  
  observeEvent(input$save_new_author, {
    if(input$first_name == "" | input$last_name == ""){
      showNotification("Please provide first and last name", duration = 3, type = "message")
    } else {
      new_author = data.frame("first_name" = input$first_name, "last_name" = input$last_name, stringsAsFactors = F)
      authors$df = rbind(authors$df, new_author)
      removeModal()
    }
  })
  
  observeEvent(input$remove_author, {
    item_remove = as.integer(parse_number(input$remove_author))
    authors$df = authors$df[-item_remove,]
    output$authors_df = renderDataTable(authors$df)
  })
  
  observeEvent(input$authors_table_cell_edit, {
    authors$df[input$authors_table_cell_edit$row, input$authors_table_cell_edit$col + 1] = input$authors_table_cell_edit$value
    output$authors_df = renderDataTable(authors$df)
  })
  
  # -------------------------------------------
  # Import
  observeEvent(input$ODMAP_to_input, {
    protocol_upload = read.csv(input$upload$datapath, sep = ",", stringsAsFactors = F, na.strings = c("NA", "", "NULL")) %>% 
      right_join(odmap_dict, by = c("section", "subsection", "element"))  %>% 
      mutate(Value = trimws(Value)) %>%
      filter(!is.na(Value))
    
    # Update ODMAP input fields with imported values
    for(i in 1:nrow(protocol_upload)){
      switch(protocol_upload$element_type[i],
             text = import_odmap_to_text(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]),
             author = import_odmap_to_authors(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]),
             objective = import_odmap_to_model_objective(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]),
             suggestion = import_suggestion(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]),
             model_algorithm = import_odmap_to_model_algorithm(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]),
             model_setting = import_model_settings(element_id = protocol_upload$element_id[i], values = protocol_upload$Value[i]))
    }
    
    # Switch to "Create a protocol" 
    reset("upload")
    updateNavbarPage(session, "navbar", selected = "create")
    updateTabsetPanel(session, "Tabset", selected = "Overview")
  })
  
  
  ## Upload gpkg --------------------
  ## Plot sampling locations -------------------------
  uploaded_samplingLocations <- reactive({
    req(input$samples_upload)
    
    tryCatch({
      st_read(input$samples_upload$datapath)
    }, error = function(e) {
      showNotification("Failed to read the sampling location .gpkg file.", type = "error")
      NULL
    })
  })

  output$d_response_7 <- renderPlot({
    sampling_locations <- uploaded_samplingLocations()
    req(sampling_locations)
    
    ggplot(sampling_locations) +
      geom_sf() +
      ggtitle("Sampling locations") +
      theme_minimal()
  })
  
  # This controls conditionalPanel visibility
  output$show_samplingLocations <- reactive({
    !is.null(input$samples_upload) && samples_valid()
  })
  outputOptions(output, "show_samplingLocations", suspendWhenHidden = FALSE)
  
  ## Plot sampling area -------------------------
  uploaded_trainingArea <- reactive({
    req(input$trainArea_upload)
    
    tryCatch({
      st_read(input$trainArea_upload$datapath)
    }, error = function(e) {
      showNotification("Failed to read the training area .gpkg file.", type = "error")
      NULL    })
  })
  
  output$m_algorithms_5 <- renderPlot({
    training_area <- uploaded_trainingArea()
    req(training_area)
    
    ggplot(training_area) +
      geom_sf() +
      ggtitle("Training/Prediction area") +
      theme_minimal()
  })
  
  # This controls conditionalPanel visibility
  output$show_trainArea <- reactive({
    !is.null(input$trainArea_upload) && trainArea_valid()
  })
  outputOptions(output, "show_trainArea", suspendWhenHidden = FALSE)
  
  ## Plot prediction area -------------------------
  uploaded_predictionArea <- reactive({
    req(input$prediction_upload)
    
    tryCatch({
      st_read(input$prediction_upload$datapath)
    }, error = function(e) {
      showNotification("Failed to read the prediction area .gpkg file.", type = "error")
      NULL    })
  })
  
  output$p_pred <- renderPlot({
    prediction_area <- uploaded_predictionArea()
    req(prediction_area)
    
    ggplot(prediction_area) +
      geom_sf() +
      ggtitle("Training/Prediction area") +
      theme_minimal()
  })
  
  output$show_predictionArea <- renderText({
    if (isTruthy(input$prediction_upload) && prediction_valid()) {
      "true"
    } else {
      "false"
    }
  })
  outputOptions(output, "show_predictionArea", suspendWhenHidden = FALSE)
  
  
  ## show geodist plot if both prediction area and samples are uploaded -------------
  output$geodist <- renderPlot({
    samples <- uploaded_samplingLocations()
    prediction_area <- uploaded_predictionArea()
    req(samples, prediction_area)
    
    sf::st_crs(samples) <- sf::st_crs(prediction_area)
    plot(CAST::geodist(samples, prediction_area)) + 
      ggplot2::ggtitle("Density distribution of Nearest-Neighbour Distances") +
      ggplot2::theme(aspect.ratio=0.5)
  })
  
  output$showGeodist <- renderText({
    if (isTruthy(input$prediction_upload) && prediction_valid() && isTruthy(input$samples_upload) && samples_valid()) {
      "true"
    } else {
      "false"
    }
  })
  outputOptions(output, "showGeodist", suspendWhenHidden = FALSE)
  
  
}

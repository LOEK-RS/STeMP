# --- Render functions for various input types ---

#' Render a multiple selection input with suggestions and free text creation
#'
#' @param element_id Input element ID
#' @param label Label text for the input
#' @param suggestions Comma-separated string of suggestion choices
#' @param info_text Optional tooltip/help text
#' @return A selectizeInput with multiple selection enabled
#' @noRd
render_suggestion <- function(element_id, label, suggestions, info_text = NULL, selected = NULL) {
	choices <- sort(trimws(unlist(strsplit(suggestions, ","))))
	if (!is.null(selected) && !is.na(selected)) {
		# Only add values that are not already in the choices
		choices <- unique(c(choices, selected))
	}
	selected_val <- selected
	if (!is.null(selected_val) && is.character(selected_val)) {
		# Convert comma-separated string from CSV to vector
		selected_val <- trimws(unlist(strsplit(selected_val, ",")))
	}

	input <- shiny::selectizeInput(
		inputId = element_id,
		label = label,
		choices = choices,
		selected = selected_val,
		multiple = TRUE,
		options = list(create = TRUE, placeholder = "Choose or type")
	)
	with_tooltip(input, info_text)
}

#' Render a single selection input with suggestions and free text creation
#'
#' Similar to \code{render_suggestion} but only allows one choice.
#'
#' @inheritParams render_suggestion
#' @return A selectizeInput with single selection enabled
#' @noRd
render_suggestion_single <- function(element_id, label, suggestions, info_text = NULL, selected = NULL) {
	choices <- sort(trimws(unlist(strsplit(suggestions, ","))))
	if (!is.null(selected) && !is.na(selected)) {
		# Only add values that are not already in the choices
		choices <- unique(c(choices, selected))
	}

	# If a selected value is passed (from CSV), use it
	selected_val <- selected %||% NULL

	input <- shiny::selectizeInput(
		inputId = element_id,
		label = label,
		choices = choices,
		selected = selected_val,
		multiple = FALSE,
		options = list(
			create = TRUE,
			placeholder = "Choose or type",
			onInitialize = I(sprintf(
				"
        function() {
          if (%s) {
            this.setValue('%s');
          } else {
            this.clear(true);
          }
        }",
				ifelse(!is.null(selected_val), "true", "false"),
				selected_val %||% ""
			))
		)
	)
	with_tooltip(input, info_text)
}

#' Render a simple text input
#'
#' @param value Optional initial value
#' @inheritParams render_suggestion
#' @return A textInput element
#' @noRd
render_text_input <- function(element_id, element, info_text = NULL, value = NULL, required = FALSE) {
	input <- shiny::textInput(inputId = element_id, label = element, value = value)
	input <- apply_required(input, required)
	with_tooltip(input, info_text)
}

#' Render a multi-line text area input
#'
#' @inheritParams render_text_input
#' @return A textAreaInput element with fixed 3 rows
#' @noRd
render_text_area <- function(element_id, element, info_text = NULL, value = NULL) {
	input <- shiny::textAreaInput(inputId = element_id, label = element, value = value, rows = 3)
	with_tooltip(input, info_text)
}

#' Render numeric input for number of training samples, with model metadata fallback
#'
#' @param model_metadata Optional object providing \code{num_training_samples()}
#' @inheritParams render_text_input
#' @return A numericInput element with default from metadata or value
#' @noRd
render_n_samples <- function(element_id, element, model_metadata = NULL, info_text = NULL, value = NULL) {
	val <- get_value(value, function() {
		if (!is.null(model_metadata) && !is.null(model_metadata$num_training_samples)) {
			model_metadata$num_training_samples()
		} else {
			NULL
		}
	})
	input <- shiny::numericInput(inputId = element_id, label = element, value = val)
	with_tooltip(input, info_text)
}

#' Render numeric input for number of predictors with optional metadata fallback
#' @noRd
render_n_predictors <- function(element_id, element, model_metadata = NULL, info_text = NULL, value = NULL) {
	val <- get_value(value, function() {
		if (!is.null(model_metadata) && !is.null(model_metadata$num_predictors)) {
			model_metadata$num_predictors()
		} else {
			NULL
		}
	})
	input <- shiny::numericInput(inputId = element_id, label = element, value = val)
	with_tooltip(input, info_text)
}

#' Render numeric input for number of classes with optional metadata fallback
#' @noRd
render_n_classes <- function(element_id, element, model_metadata = NULL, info_text = NULL, value = NULL) {
	val <- get_value(value, function() {
		if (!is.null(model_metadata) && !is.null(model_metadata$num_classes)) {
			model_metadata$num_classes()
		} else {
			NULL
		}
	})
	input <- shiny::numericInput(inputId = element_id, label = element, value = val)
	with_tooltip(input, info_text)
}

#' Render text input for number of samples per class with optional metadata fallback
#' @noRd
render_n_samples_class <- function(element_id, element, model_metadata = NULL, info_text = NULL, value = NULL) {
	val <- get_value(value, function() {
		if (!is.null(model_metadata) && !is.null(model_metadata$num_samples_per_class)) {
			model_metadata$num_samples_per_class()
		} else {
			NULL
		}
	})
	input <- shiny::textInput(inputId = element_id, label = element, value = val)
	with_tooltip(input, info_text)
}

#' Render text input for interpolation range with optional metadata fallback
#' @noRd
render_range <- function(element_id, element, model_metadata = NULL, info_text = NULL, value = NULL) {
	val <- get_value(value, function() {
		if (!is.null(model_metadata) && !is.null(model_metadata$interpolation_range)) {
			model_metadata$interpolation_range()
		} else {
			NULL
		}
	})
	input <- shiny::textInput(inputId = element_id, label = element, value = val)
	with_tooltip(input, info_text)
}

#' Render text input for validation results with optional metadata fallback
#' @noRd
render_validation_results <- function(element_id, element, model_metadata = NULL, info_text = NULL, value = NULL) {
	val <- get_value(value, function() {
		if (!is.null(model_metadata) && !is.null(model_metadata$validation_results)) {
			model_metadata$validation_results()
		} else {
			NULL
		}
	})
	input <- shiny::textInput(inputId = element_id, label = element, value = val)
	with_tooltip(input, info_text)
}

#' Render text input for predictor names with optional metadata fallback
#' @noRd
render_names_predictors <- function(element_id, element, model_metadata = NULL, info_text = NULL, value = NULL) {
	val <- get_value(value, function() {
		if (!is.null(model_metadata) && !is.null(model_metadata$names_predictors)) {
			model_metadata$names_predictors()
		} else {
			NULL
		}
	})
	input <- shiny::textInput(inputId = element_id, label = element, value = val)
	with_tooltip(input, info_text)
}

#' Render text input for model hyperparameters with optional metadata fallback
#' @noRd
render_hyperparameters <- function(element_id, element, model_metadata = NULL, info_text = NULL, value = NULL) {
	val <- get_value(value, function() {
		if (!is.null(model_metadata) && !is.null(model_metadata$model_hyperparams)) {
			model_metadata$model_hyperparams()
		} else {
			NULL
		}
	})
	input <- shiny::textInput(inputId = element_id, label = element, value = val)
	with_tooltip(input, info_text)
}

#' Render select input for model type (classification/regression) with metadata fallback
#' @noRd
render_model_type <- function(element_id, element, model_metadata = NULL, info_text = NULL, value = NULL) {
	val <- get_value(value, function() {
		if (!is.null(model_metadata) && !is.null(model_metadata$model_type)) {
			model_metadata$model_type()
		} else {
			""
		}
	})
	input <- shiny::selectInput(
		inputId = element_id,
		label = element,
		choices = c("", "Classification", "Regression"),
		selected = val
	)
	with_tooltip(input, info_text)
}

#' Render select input for model algorithm with dynamic choices including metadata fallback
#' @noRd
render_model_algorithm <- function(element_id, element, model_metadata = NULL, info_text = NULL, value = NULL) {
	default_algos <- c("rf", "gbm", "glm", "svmRadial", "nnet", "rpart")

	selected_algo <- if (is.null(value) || is.na(value)) {
		get_value(NULL, function() {
			if (!is.null(model_metadata) && !is.null(model_metadata$model_algorithm)) {
				model_metadata$model_algorithm()
			} else {
				""
			}
		})
	} else {
		value
	}

	algo_choices <- if (selected_algo != "" && !(selected_algo %in% default_algos)) {
		c(default_algos, selected_algo)
	} else {
		default_algos
	}

	input <- shiny::selectInput(
		inputId = element_id,
		label = element,
		choices = c("", unique(algo_choices)),
		selected = selected_algo
	)
	with_tooltip(input, info_text)
}

#' Render text input for coordinate reference system
#' @noRd
render_crs <- function(element_id, element, geo_metadata = NULL, info_text = NULL, value = NULL) {
		input <- shiny::textInput(inputId = element_id, label = element, value = value)
	with_tooltip(input, info_text)
}

#' Server logic to update input for coordinate reference system based on geographic metadata
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param element_id ID of the design select input to update
#' @param geo_metadata Reactive or reactiveValues containing geographic metadata
#' @noRd
render_crs_server <- function(input, output, session, element_id, geo_metadata = NULL) {
  shiny::observeEvent(
    geo_metadata,
    {
      val <- get_value(uploaded_value = NULL, function() {
        if (!is.null(geo_metadata) && !is.null(geo_metadata$samples_crs)) {
          geo_metadata$samples_crs()
        } else {
          NULL
        }
      })
      
      shinyjs::delay(100, {
        shiny::updateTextInput(session, inputId = element_id, value = val)
      })
    },
    ignoreInit = FALSE
  )
}

#' Render select input for sampling design choices
#' @noRd
render_design <- function(element_id, element, selected = NULL, info_text = NULL) {
	input <- shiny::selectInput(
		inputId = element_id,
		label = element,
		choices = c("", "clustered", "random", "stratified"),
		selected = selected %||% ""
	)
	with_tooltip(input, info_text)
}

#' Server logic to update design selection input based on reactive geographic distance selection
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param element_id ID of the design select input to update
#' @param geodist_sel Reactive providing selected design value
#' @noRd
render_design_server <- function(input, output, session, element_id, geodist_sel) {
	shiny::observeEvent(
		geodist_sel(),
		{
			selected_val <- geodist_sel()
			if (is.null(selected_val)) {
				return(NULL)
			}

			shinyjs::delay(100, {
				shiny::updateSelectInput(session, inputId = element_id, selected = selected_val)
			})
		},
		ignoreInit = FALSE
	)

	shiny::observe({
		val <- input[[element_id]]
	})
}

#' Generic renderer for plots with wrapper consistent with rest of protocol
#' @noRd
render_plot <- function(element_id, label, info_text = NULL) {
  inputTag <- shiny::div(
    class = c("form-group shiny-input-container"),
    shiny::tags$label(`for` = element_id, class = "control-label", label),
    shiny::div(
      shiny::uiOutput(outputId = paste0(element_id, "_img"), style = "height: auto")
    ),
    shiny::div(
      shiny::uiOutput(outputId = paste0(element_id, "_plot_ui"))
    )
  )
  with_tooltip(inputTag, info_text)
}

#' Server-side render logic for plots
#' 
#' Manages conditional rendering of plots within a Shiny application by toggling between
#' pre-generated PNG images and dynamically rendered plots based on data availability and
#' objective selection. Handles UI visibility, validates plot/objective combinations, and
#' orchestrates both static image display and reactive plot generation.
#'
#' @param file Character string specifying the filename of a pre-generated PNG image
#' @param valid_geo_metadata Reactive expression containing validated geographic metadata;
#'   used to conditionally render fresh plots
#' @param element_id Input element ID
#' @param objective Character vector specifying objective ("Model and prediction", "Model only")
#' @param uploaded_zip Reactive containing uploaded zip file data; when non-NULL,
#'   pre-generated PNG images are displayed
#' @param output_dir Character string specifying the temporary directory path where
#'   pre-generated PNG files are stored
#' @param ns Function for namespacing Shiny module element IDs; defaults to identity
#' @param output Shiny output object for assigning reactive plot outputs
#' @param plot_fn Function for plotting a fresh plot with the geo data; either geo_map()
#'   or geodist_plot()
#' @noRd
render_plot_server <- function(
    file,
    valid_geo_metadata,
    element_id,
    objective,
    uploaded_zip,
    output_dir,
    ns = identity,
    output,
    plot_fn
) {
    valid_objective_plot_combination <- !isTRUE(
      (identical(element_id, "geodistance_plot_training_area") && identical(objective, "Model and prediction")) ||
        (identical(element_id, "geodistance_plot_prediction_area") && identical(objective, "Model only"))
    )

  if (valid_objective_plot_combination && !is.null(uploaded_zip) && file.exists(file.path(output_dir, file))) {

    # Remove plot UI
    output[[element_id]] <- shiny::renderPlot(NULL)
    output[[paste0(element_id, "_plot_ui")]] <- shiny::renderUI(NULL)

    # Render PNG from ZIP upload
    output[[paste0(element_id, "_img")]] <- shiny::renderUI({
      shiny::tags$div(
        style = "display: flex; justify-content: center;",
        shiny::tags$img(
          src = paste0("/temp_stemp/", file),
          style = "height: 500px"
        )
      )
    })

  } else if (valid_objective_plot_combination && !is.null(valid_geo_metadata)) {

     # Remove image UI
    output[[paste0(element_id, "_img")]] <- shiny::renderUI(NULL)

    # Render fresh plot from geo data upload
    output[[paste0(element_id, "_plot_ui")]] <- shiny::renderUI({
      shiny::plotOutput(outputId = ns(element_id), height = "300px")
    })

    output[[element_id]] <- plot_fn()

  } else {
    # Remove everything
    output[[element_id]] <- shiny::renderPlot(NULL)
    output[[paste0(element_id, "_img")]] <- shiny::renderUI(NULL)
    output[[paste0(element_id, "_plot_ui")]] <- shiny::renderUI(NULL)
  }
}

# --- Master input renderer ---

#' Dispatch rendering of input field based on element type
#'
#' Chooses appropriate input render function and applies metadata or row value overrides.
#'
#' @param element_type Type of input element (e.g., "text", "suggestion", "num_training_samples")
#' @param element_id Input element ID
#' @param label Label text for the input
#' @param o_objective_1 Unused in this function but required by caller
#' @param suggestions Optional suggestions for select inputs
#' @param info_text Optional tooltip/help text
#' @param model_metadata Optional model metadata object for default values
#' @param geo_metadata Optional geographic metadata object
#' @param ns Namespace function for UI IDs
#' @param row Data row containing a \code{value} field for overrides
#' @return Shiny input UI element
#' @noRd
render_input_field <- function(
	element_type,
	element_id,
	label,
	o_objective_1,
	suggestions = NULL,
	info_text = NULL,
	model_metadata = NULL,
	ns = identity,
	row
) {
	uploaded_value <- if (!is.null(row$value) && nzchar(row$value)) row$value else NULL

	required <- row$optional == 0
	label_ui <- make_label(label, row$optional)

	input_tag <- switch(
		element_type,
		"text" = render_text_input(element_id, label_ui, info_text, value = uploaded_value, required = required),
		"author" = render_text_input(element_id, label_ui, info_text, value = uploaded_value),
		"hyperparams" = render_text_area(element_id, label_ui, info_text, value = uploaded_value),
		"suggestion" = render_suggestion(element_id, label_ui, suggestions, info_text, selected = uploaded_value),
		"suggestion_single" = render_suggestion_single(
			element_id,
			label_ui,
			suggestions,
			info_text,
			selected = uploaded_value
		),
		"num_training_samples" = render_n_samples(element_id, label_ui, model_metadata, info_text, value = uploaded_value),
		"num_predictors" = render_n_predictors(element_id, label_ui, model_metadata, info_text, value = uploaded_value),
		"num_classes" = render_n_classes(element_id, label_ui, model_metadata, info_text, value = uploaded_value),
		"num_samples_per_class" = render_n_samples_class(
			element_id,
			label_ui,
			model_metadata,
			info_text,
			value = uploaded_value
		),
		"interpolation_range" = render_range(element_id, label_ui, model_metadata, info_text, value = uploaded_value),
		"names_predictors" = render_names_predictors(
			element_id,
			label_ui,
			model_metadata,
			info_text,
			value = uploaded_value
		),
		"model_hyperparams" = render_hyperparameters(
			element_id,
			label_ui,
			model_metadata,
			info_text,
			value = uploaded_value
		),
		"model_type" = render_model_type(element_id, label_ui, model_metadata, info_text, value = uploaded_value),
		"model_algorithm" = render_model_algorithm(element_id, label_ui, model_metadata, info_text, value = uploaded_value),
		"samples_crs" = render_crs(element_id, label_ui, info_text, value = uploaded_value),
		"validation_results" = render_validation_results(
			element_id,
			label_ui,
			model_metadata,
			info_text,
			value = uploaded_value
		),

		# fallback to text input
		render_text_input(element_id, label_ui, info_text, value = uploaded_value)
	)

	return(input_tag)
}

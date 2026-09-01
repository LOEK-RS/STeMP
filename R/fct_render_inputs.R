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

#' Server logic to update input for text input based on model metadata
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param element_id Input element ID
#' @param element_type Type of input element
#' @param model_metadata Reactive or reactiveValues containing model metadata
#' @param uploaded_value Value from uploaded input
#' @noRd
render_text_input_model_server <- function(
	input,
	output,
	session,
	element_id,
	element_type,
	model_metadata = NULL,
	uploaded_value = NULL
) {
	shiny::observeEvent(
		model_metadata,
		{
			val <- get_value(uploaded_value = uploaded_value, function() {
				if (!is.null(model_metadata) && !is.null(model_metadata[[element_type]])) {
					model_metadata[[element_type]]()
				} else {
					""
				}
			})

			shinyjs::delay(100, {
				shiny::updateTextInput(session, inputId = element_id, value = val)
			})
		},
		ignoreInit = FALSE
	)
}

#' Server logic to update input for text input based on geographic metadata
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param element_id Input element ID
#' @param element_type Type of input element
#' @param uploaded_value Value from uploaded input
#' @param geo_metadata Reactive or reactiveValues containing geographic metadata
#' @noRd
render_text_input_geo_server <- function(
	input,
	output,
	session,
	element_id,
	element_type,
	uploaded_value = NULL,
	geo_metadata = reactiveVal(NULL)
) {
	shiny::observeEvent(
		geo_metadata,
		{
			val <- get_value(uploaded_value = uploaded_value, function() {
				if (!is.null(geo_metadata) && !is.null(geo_metadata[[element_type]])) {
					geo_metadata[[element_type]]()
				} else {
					""
				}
			})

			shinyjs::delay(100, {
				shiny::updateTextInput(session, inputId = element_id, value = val)
			})
		},
		ignoreInit = FALSE
	)
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

#' Render a numeric input
#' @noRd
render_numeric_input <- function(element_id, element, info_text = NULL, value = NULL) {
	input <- shiny::numericInput(inputId = element_id, label = element, value = value)
	with_tooltip(input, info_text)
}

#' Server logic to update input for numeric input based on model metadata
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param element_id Input element ID
#' @param element_type Type of input element
#' @param model_metadata Reactive or reactiveValues containing model metadata
#' @param uploaded_value Value from uploaded input
#' @noRd
render_numeric_input_model_server <- function(
	input,
	output,
	session,
	element_id,
	element_type,
	model_metadata = NULL,
	uploaded_value = NULL
) {
	shiny::observeEvent(
		model_metadata,
		{
			val <- get_value(uploaded_value = uploaded_value, function() {
				if (!is.null(model_metadata) && !is.null(model_metadata[[element_type]])) {
					model_metadata[[element_type]]()
				} else {
					NA
				}
			})

			shinyjs::delay(100, {
				shiny::updateNumericInput(session, inputId = element_id, value = val)
			})
		},
		ignoreInit = FALSE
	)
}

#' Render select input
#' @noRd
render_select_input <- function(element_id, element, choices = c(""), selected = NULL, info_text = NULL) {
	input <- shiny::selectInput(
		inputId = element_id,
		label = element,
		choices = choices,
		selected = selected %||% ""
	)
	with_tooltip(input, info_text)
}

#' Server logic to update input for select input based on model metadata
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param element_id Input element ID
#' @param element_type Type of input element
#' @param model_metadata Reactive or reactiveValues containing model metadata
#' @param uploaded_value Value from uploaded input
#' @param default_choices Vector with default choices as strings
#' @noRd
render_select_input_model_server <- function(
	input,
	output,
	session,
	element_id,
	element_type,
	model_metadata = NULL,
	uploaded_value = NULL,
	default_choices = c("")
) {
	shiny::observeEvent(
		model_metadata,
		{
			selected_val <- get_value(uploaded_value = uploaded_value, function() {
				if (!is.null(model_metadata) && !is.null(model_metadata[[element_type]])) {
					model_metadata[[element_type]]()
				} else {
					""
				}
			})

			choices <- if (selected_val != "" && !(selected_val %in% default_choices)) {
				c(default_choices, selected_val)
			} else {
				default_choices
			}

			shinyjs::delay(100, {
				shiny::updateSelectInput(
					session,
					inputId = element_id,
					choices = c("", unique(choices)),
					selected = selected_val
				)
			})
		},
		ignoreInit = FALSE
	)
}

#' Server logic to update design selection input based on reactive geographic distance selection
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param element_id ID of the design select input to update
#' @param geodist_sel Reactive providing selected design value
#' @param uploaded_value Value from uploaded input
#' @noRd
render_select_input_design_server <- function(
	input,
	output,
	session,
	element_id,
	geodist_sel = shiny::reactive(NULL),
	uploaded_value = NULL
) {
	shiny::observeEvent(
		geodist_sel(),
		{
			selected_val <- get_value(uploaded_value = uploaded_value, function() {
				if (!is.null(geodist_sel())) {
					geodist_sel()
				} else {
					""
				}
			})

			shinyjs::delay(100, {
				shiny::updateSelectInput(session, inputId = element_id, selected = selected_val)
			})
		},
		ignoreInit = FALSE,
		ignoreNULL = FALSE
	)

	shiny::observe({
		val <- input[[element_id]]
	})
}

#' Generic renderer for plots with wrapper consistent with rest of protocol
#' @noRd
render_plot <- function(element_id, label, info_text = NULL) {
	inputTag <- shiny::div(
		id = paste0(element_id, "_field"),
		class = c("form-group shiny-input-container", "hide_plot_field"),
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
		(identical(element_id, "geodist_training_area") && identical(objective, "Model and prediction")) ||
			(identical(element_id, "geodist_prediction_area") && identical(objective, "Model only"))
	)

	if (valid_objective_plot_combination && !is.null(uploaded_zip) && file.exists(file.path(output_dir, file))) {
		# Show plot field
		shinyjs::removeClass(selector = paste0("#", ns(element_id), "_field"), class = "hide_plot_field")

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
		# Show plot field
		shinyjs::removeClass(selector = paste0("#", ns(element_id), "_field"), class = "hide_plot_field")

		# Remove image UI
		output[[paste0(element_id, "_img")]] <- shiny::renderUI(NULL)

		# Render fresh plot from geo data upload
		output[[paste0(element_id, "_plot_ui")]] <- shiny::renderUI({
			shiny::plotOutput(outputId = ns(element_id), height = "300px")
		})

		output[[element_id]] <- plot_fn()
	} else {
		# Hide plot field
		shinyjs::addClass(selector = paste0("#", ns(element_id), "_field"), class = "hide_plot_field")

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
#' @param suggestions Optional suggestions for select inputs
#' @param info_text Optional tooltip/help text
#' @param row Data row containing a \code{value} field for overrides
#' @return Shiny input UI element
#' @noRd
render_input_field <- function(
	element_type,
	element_id,
	label,
	suggestions = NULL,
	info_text = NULL,
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
		"num_training_samples" = render_numeric_input(element_id, label_ui, info_text, value = uploaded_value),
		"num_predictors" = render_numeric_input(element_id, label_ui, info_text, value = uploaded_value),
		"num_classes" = render_numeric_input(element_id, label_ui, info_text, value = uploaded_value),
		"num_samples_per_class" = render_text_input(element_id, label_ui, info_text, value = uploaded_value),
		"interpolation_range" = render_text_input(element_id, label_ui, info_text, value = uploaded_value),
		"names_predictors" = render_text_input(element_id, label_ui, info_text, value = uploaded_value),
		"model_hyperparams" = render_text_input(element_id, label_ui, info_text, value = uploaded_value),
		"model_type" = render_select_input(
			element_id,
			label_ui,
			choices = c("", "Classification", "Regression"),
			selected = uploaded_value,
			info_text = info_text
		),
		"model_algorithm" = render_select_input(
			element_id,
			label_ui,
			choices = c("", "rf", "gbm", "glm", "svmRadial", "nnet", "rpart"),
			selected = uploaded_value,
			info_text = info_text
		),
		"sampling_design" = render_select_input(
			element_id,
			label_ui,
			choices = c("", "clustered", "random", "stratified"),
			selected = uploaded_value,
			info_text = info_text
		),
		"samples_crs" = render_text_input(element_id, label_ui, info_text, value = uploaded_value),
		"validation_results" = render_text_input(element_id, label_ui, info_text, value = uploaded_value),

		# fallback to text input
		render_text_input(element_id, label_ui, info_text, value = uploaded_value)
	)

	return(input_tag)
}

#' Dispatch update of input based on model metadata and element_type
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param element_type Type of input element
#' @param element_id Input element ID
#' @param model_metadata Reactive or reactiveValues containing model metadata
#' @param geo_metadata Reactive or reactiveValues containing geo metadata
#' @param uploaded_value Value from uploaded input
#' @noRd
render_input_field_server <- function(
	input,
	output,
	session,
	element_type,
	element_id,
	model_metadata = NULL,
	geo_metadata = reactiveVal(NULL),
	uploaded_value = NULL
) {
	update_input_tag <- switch(
		element_type,
		"num_training_samples" = render_numeric_input_model_server(
			input,
			output,
			session,
			element_id = element_id,
			element_type = element_type,
			model_metadata = model_metadata,
			uploaded_value = uploaded_value
		),
		"num_predictors" = render_numeric_input_model_server(
			input,
			output,
			session,
			element_id = element_id,
			element_type = element_type,
			model_metadata = model_metadata,
			uploaded_value = uploaded_value
		),
		"num_classes" = render_numeric_input_model_server(
			input,
			output,
			session,
			element_id = element_id,
			element_type = element_type,
			model_metadata = model_metadata,
			uploaded_value = uploaded_value
		),
		"num_samples_per_class" = render_text_input_model_server(
			input,
			output,
			session,
			element_id = element_id,
			element_type = element_type,
			model_metadata = model_metadata,
			uploaded_value = uploaded_value
		),
		"interpolation_range" = render_text_input_model_server(
			input,
			output,
			session,
			element_id = element_id,
			element_type = element_type,
			model_metadata = model_metadata,
			uploaded_value = uploaded_value
		),
		"names_predictors" = render_text_input_model_server(
			input,
			output,
			session,
			element_id = element_id,
			element_type = element_type,
			model_metadata = model_metadata,
			uploaded_value = uploaded_value
		),
		"model_hyperparams" = render_text_input_model_server(
			input,
			output,
			session,
			element_id = element_id,
			element_type = element_type,
			model_metadata = model_metadata,
			uploaded_value = uploaded_value
		),
		"model_type" = render_select_input_model_server(
			input,
			output,
			session,
			element_id = element_id,
			element_type = element_type,
			model_metadata = model_metadata,
			uploaded_value = uploaded_value,
			default_choices = c("Classification", "Regression")
		),
		"model_algorithm" = render_select_input_model_server(
			input,
			output,
			session,
			element_id = element_id,
			element_type = element_type,
			model_metadata = model_metadata,
			uploaded_value = uploaded_value,
			default_choices = c("rf", "gbm", "glm", "svmRadial", "nnet", "rpart")
		),
		"samples_crs" = render_text_input_geo_server(
			input,
			output,
			session,
			element_id = element_id,
			element_type = element_type,
			geo_metadata = geo_metadata,
			uploaded_value = uploaded_value
		),
		"validation_results" = render_text_input_model_server(
			input,
			output,
			session,
			element_id = element_id,
			element_type = element_type,
			model_metadata = model_metadata,
			uploaded_value = uploaded_value
		)
	)

	return(update_input_tag)
}

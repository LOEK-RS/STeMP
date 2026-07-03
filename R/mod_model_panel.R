#' Model Panel UI Module
#'
#' Creates a collapsible panel UI to group and render inputs for model-related protocol elements.
#' Supports dynamic rendering based on the protocol data and metadata, including plots and input fields.
#'
#' @param id Module namespace ID
#' @noRd
mod_model_panel_ui <- function(id) {
	ns <- shiny::NS(id)

	shiny::fluidPage(
		shiny::uiOutput(ns("model_collapse_ui")) # container for the full collapsible UI
	)
}

#' Model Panel Server Module
#'
#' Manages the server-side logic for the model panel inputs grouped by subsections.
#' Updates inputs reactively based on protocol data, uploaded values, model metadata, and geographic metadata.
#' Supports rendering of specialized plots and input fields, including sampling design updates.
#'
#' @param id Module namespace ID
#' @param protocol_data Reactive data frame containing protocol information filtered for model section
#' @param model_metadata Reactive or reactiveValues containing model metadata; optional
#' @param geo_metadata Reactive or reactiveValues containing geographic metadata; optional
#' @param o_objective_1_val Reactive returning the selected modeling objective (e.g., "Model only")
#' @param geodist_sel Reactive returning selected geographic distance classification; defaults to reactive(NULL)
#' @param uploaded_values Reactive data frame with uploaded element_id and value pairs to override defaults; optional
#' @param output_dir temporary output directory
#' @param model_deleted Reactive boolean, resets inputs when TRUE
#' @param hide_optional Reactive boolean, hides optional fields when TRUE
#'
#' @return A list of reactive expressions:
#' \itemize{
#'   \item{model_inputs}{Data frame of current input values for model section elements}
#'   \item{validation_method}{Selected validation strategy}
#'   \item{sampling_design}{Selected sampling design}
#'   \item{predictor_types}{Selected predictor types}
#' }
#' @noRd
mod_model_panel_server <- function(
	id,
	protocol_data,
	model_metadata = reactiveVal(NULL),
	geo_metadata = reactiveVal(NULL),
	o_objective_1_val,
	geodist_sel = shiny::reactive(NULL),
	uploaded_values = shiny::reactive(NULL),
	output_dir = NULL,
	model_deleted = shiny::reactive(FALSE),
	hide_optional = shiny::reactive(FALSE),
	uploaded_zip = NULL
) {
	shiny::moduleServer(id, function(input, output, session) {
		ns <- session$ns

		# Update sampling_design input when geodist_sel changes
		shiny::observe({
			selected_val <- geodist_sel()
			if (!is.null(selected_val)) {
				shiny::updateSelectInput(session, "sampling_design", selected = selected_val)
			}
		})

		# Validate metadata subsets
		valid_model_metadata <- validate_model_metadata(model_metadata, "has_model")
		valid_geo_samples_metadata <- validate_geo_metadata(geo_metadata, "has_samples")
		valid_geo_training_area_metadata <- validate_geo_metadata(geo_metadata, "has_training_area")
		valid_geo_all_metadata <- validate_geo_metadata(geo_metadata, c("has_samples", "has_training_area"))

		# Filter protocol data for Model section
		model_data <- shiny::reactive({
			shiny::req(protocol_data())
			df <- protocol_data()
			df[df$section == "Model", ]
		})

		# Track currently rendered model input IDs
		current_model_ids <- shiny::reactiveVal(NULL)
		shiny::observe({
			df <- model_data()
			if (!is.null(df) && nrow(df) > 0) {
				current_model_ids(df$element_id)
			}
		})

		# Subsections
		subsections <- shiny::reactive({
			unique(model_data()$subsection)
		})

		# Render collapsible UI
		output$model_collapse_ui <- shiny::renderUI({
			df <- model_data()
			subs <- subsections()

			if (nrow(df) == 0) {
				return(shiny::tags$p("No model data available"))
			}

			uploaded_df <- uploaded_values()

			panels <- lapply(subs, function(subsec) {
				sub_df <- df[df$subsection == subsec, ]

				inputs <- lapply(seq_len(nrow(sub_df)), function(i) {
					row <- sub_df[i, ]

					# Override default value if uploaded
					if (!is.null(uploaded_df)) {
						uploaded_val <- uploaded_df$value[uploaded_df$element_id == row$element_id]
						if (length(uploaded_val) == 1 && !is.null(uploaded_val) && nzchar(uploaded_val)) {
							row$value <- uploaded_val
						}
					}

					# Decide if this row should be hidden for classification/regression
					div_class_classification <- if (row$hide_for_classification == 1) "regression_field" else NULL
					div_class_regression <- if (row$hide_for_regression == 1) "classification_field" else NULL

					# Decide if this row is optional
					div_class_optional <- if (row$optional == 1) "optional_field" else NULL

					# Render field
					content <- if (row$element_type %in% c("training_plot", "training_area_plot", "geodist_plot_training")) {
					  render_plot(
					    element_id = ns(row$element_id),
					    label = row$element,
					    info_text = row$info_text
					  )
					} else if (row$element_type == "sampling_design") {
						# Look for uploaded CSV value
						uploaded_val <- NULL
						if (!is.null(uploaded_df)) {
							val <- uploaded_df$value[uploaded_df$element_id == row$element_id]
							if (length(val) == 1 && nzchar(val)) {
								uploaded_val <- val
							}
						}
						selected_val <- uploaded_val

						render_design(
							element_id = ns(row$element_id),
							element = make_label(row$element, row$optional),
							selected = selected_val,
							info_text = row$info_text
						)
					} else {
						render_input_field(
							element_type = row$element_type,
							element_id = ns(row$element_id),
							label = row$element,
							# o_objective_1 = o_objective_1_val(),
							suggestions = row$suggestions,
							info_text = row$info_text,
							ns = ns,
							row = row
						)
					}

					# Wrap with optional/classification/regression class if flagged
					shiny::tags$div(class = c(div_class_optional, div_class_classification, div_class_regression), content)
				})

				shinyBS::bsCollapsePanel(title = subsec, do.call(shiny::tagList, inputs), style = "primary")
			})

			# Signal to trigger observers after dynamic UI render (timestamp ensures a change).
			shinyjs::runjs(sprintf(
			  "Shiny.onInputChange('%s', new Date().getTime());",
			  ns("ui_rendered")
			))
			
			do.call(shinyBS::bsCollapse, panels)
		})

		# Observers for plots
		shiny::observe({
		  render_plot_server(
		    file = "training_locations.png",
		    valid_geo_metadata = valid_geo_samples_metadata(),
		    element_id = "training_locations",
		    objective = o_objective_1_val(),
		    uploaded_zip = uploaded_zip(),
		    output_dir = output_dir,
		    ns = ns,
		    output = output,
		    plot_fn = function() {
		      geo_map(
		        output = output,
		        element_id = "training_locations",
		        geo_metadata = valid_geo_samples_metadata() %||% list(),
		        what = "samples_sf",
		        output_dir = output_dir
		      )
		    }
		  )
		})

		shiny::observe({
		  render_plot_server(
		    file = "training_area.png",
		    valid_geo_metadata = valid_geo_training_area_metadata(),
		    element_id = "training_area_map",
		    objective = o_objective_1_val(),
		    uploaded_zip = uploaded_zip(),
		    output_dir = output_dir,
		    ns = ns,
		    output = output,
		    plot_fn = function() {
		      geo_map(
		        output = output,
		        element_id = "training_area_map",
		        geo_metadata = valid_geo_training_area_metadata() %||% list(),
		        what = "training_area_sf",
		        output_dir = output_dir
		      )
		    }
		  )
		})
		
		shiny::observe({
		  render_plot_server(
		    file = "geodist_training_area.png",
		    valid_geo_metadata = valid_geo_all_metadata(),
		    element_id = "geodistance_plot_training_area",
		    objective = o_objective_1_val(),
		    uploaded_zip = uploaded_zip(),
		    output_dir = output_dir,
		    ns = ns,
		    output = output,
		    plot_fn = function() {
		      geodist_plot(
		        				output = output,
		        				element_id = "geodistance_plot_training_area",
		        				geo_metadata = valid_geo_all_metadata() %||% list(),
		        				objective = "Model only",
		        				output_dir = output_dir
		        			)
		    }
		  )
		})

		# CRS server
		shiny::observe({
		  shiny::req(model_data())
		  df <- model_data()
		  meta_geo_samples_list <- valid_geo_samples_metadata() %||% list()
		  design_id <- df$element_id[df$element_type == "samples_crs"]
		  
		  if (length(design_id) == 1) {
		    render_crs_server(
		      input = input,
		      output = output,
		      session = session,
		      element_id = design_id,
		      geo_metadata = meta_geo_samples_list
		    )
		  }
		})
		
		# Sampling design server
		shiny::observe({
			shiny::req(model_data())
			df <- model_data()
			design_id <- df$element_id[df$element_type == "sampling_design"]

			if (length(design_id) == 1) {
				render_design_server(
					input = input,
					output = output,
					session = session,
					element_id = design_id,
					geodist_sel = geodist_sel
				)
			}
		})
		
		# Update of inputs based on uploaded model_metadata
		shiny::observe({
		  input[["ui_rendered"]]
		  shiny::req(model_data(), valid_model_metadata())
		  df <- model_data()
		  meta_model_list <- valid_model_metadata() %||% list()

		  element_types <- c(
		    "num_training_samples",
		    "num_predictors",
		    "num_classes",
		    "num_samples_per_class",
		    "interpolation_range",
		    "names_predictors",
		    "model_hyperparams",
		    "model_type",
		    "model_algorithm",
		    "validation_results"
		  )

		  lapply(element_types, function(element_type) {
		    render_input_field_server(
		      input = input,
		      output = output,
		      session = session,
		      element_type = element_type,
		      element_id = df$element_id[df$element_type == element_type],
		      model_metadata = meta_model_list,
		      model_deleted = model_deleted
		    )
		  })
		})
		
		# Collect input values
		inputs_reactive <- shiny::reactive({
			df <- model_data()
			vals <- lapply(df$element_id, function(id) {
				val <- input[[id]]
				if (is.null(val) || (is.character(val) && all(val == ""))) {
					NA
				} else if (length(val) > 1) {
					paste(val, collapse = ", ")
				} else {
					val
				}
			})

			data.frame(
				section = df$section,
				subsection = df$subsection,
				element = df$element,
				element_id = df$element_id,
				value = unlist(vals, use.names = FALSE),
				stringsAsFactors = FALSE
			)
		})

		# Reactive getters
		validation_method <- shiny::reactive({
			input[["validation_strategy"]]
		})
		sampling_design <- shiny::reactive({
			input[["sampling_design"]]
		})
		predictor_types <- shiny::reactive({
			input[["predictor_types"]]
		})

		# Hide range/classes fields dynamically in dependence of model type
		shiny::observe({
			model_type <- input[["model_type"]]
			if (is.null(model_type) || model_type == "") {
				shinyjs::removeClass(selector = "body", class = "hide_for_regression")
				shinyjs::removeClass(selector = "body", class = "hide_for_classification")
			} else if (model_type == "Classification") {
				shinyjs::addClass(selector = "body", class = "hide_for_classification")
				shinyjs::removeClass(selector = "body", class = "hide_for_regression")
			} else if (model_type == "Regression") {
				shinyjs::addClass(selector = "body", class = "hide_for_regression")
				shinyjs::removeClass(selector = "body", class = "hide_for_classification")
			}
		})

		# Hide/show optional fields dynamically
		shiny::observe({
			if (isTRUE(hide_optional())) {
				shinyjs::addClass(selector = "body", class = "hide_optional")
			} else {
				shinyjs::removeClass(selector = "body", class = "hide_optional")
			}
		})

		# Return values
		return(list(
			"model_inputs" = inputs_reactive,
			"validation_method" = validation_method,
			"sampling_design" = sampling_design,
			"predictor_types" = predictor_types
		))
	})
}

#' Prediction Panel UI Module
#'
#' Creates UI for the Prediction section of the protocol, rendering inputs grouped
#' by subsections inside collapsible panels. Only shown if the objective is
#' "Model and prediction".
#'
#' @param id Module namespace ID
#' @return UI output container for prediction inputs inside collapsible panels
#' @noRd
mod_prediction_panel_ui <- function(id) {
	ns <- shiny::NS(id)

	shiny::fluidPage(
		shiny::uiOutput(ns("prediction_collapse_ui"))
	)
}

#' Prediction Panel Server Module
#'
#' Manages server-side logic for the Prediction panel inputs.
#' Filters protocol data for Prediction section, supports optional uploaded values
#' to override defaults, and renders geo-spatial plots server-side where appropriate.
#' Only active if the selected objective is "Model and prediction".
#'
#' @param id Module namespace ID
#' @param o_objective_1_val Reactive returning selected objective ("Model and prediction" or "Model only")
#' @param protocol_data Reactive data frame containing protocol information
#' @param geo_metadata Spatial metadata reactive list (optional)
#' @param uploaded_values Reactive data frame (optional) with uploaded element_id/value pairs to override defaults
#' @param output_dir temporary output directory
#' @param hide_optional Reactive boolean to control whether optional inputs should be hidden
#'
#' @return A list containing:
#' \itemize{
#'   \item{prediction_inputs}{Reactive data.frame with current input values for prediction section elements}
#'   \item{uncertainty_quantification}{Selected uncertainty quantification approach}
#'   \item{evaluation_method}{Selected evaluation strategy}
#' }
#' @noRd
mod_prediction_panel_server <- function(
	id,
	o_objective_1_val,
	protocol_data,
	geo_metadata = NULL,
	uploaded_values = shiny::reactive(NULL),
	output_dir = NULL,
	hide_optional = shiny::reactive(FALSE),
	uploaded_zip = NULL
) {
	shiny::moduleServer(id, function(input, output, session) {
		ns <- session$ns

		# Validate spatial metadata for samples and prediction area
		valid_geo_samples_metadata <- validate_geo_metadata(geo_metadata, "has_samples")
		valid_geo_prediction_area_metadata <- validate_geo_metadata(geo_metadata, "has_prediction_area")
		valid_geo_all_metadata <- validate_geo_metadata(geo_metadata, c("has_samples", "has_prediction_area"))

		# Reactive filtered protocol data for Prediction section
		prediction_data <- shiny::reactive({
			shiny::req(protocol_data())
			df <- protocol_data()
			df[df$section == "Prediction", ]
		})

		# Unique subsections within Prediction data
		subsections <- shiny::reactive({
			unique(prediction_data()$subsection)
		})

		# Render UI collapsible panels for each subsection
		output$prediction_collapse_ui <- shiny::renderUI({
			shiny::req(o_objective_1_val() == "Model and prediction")

			df <- prediction_data()
			subs <- subsections()

			if (nrow(df) == 0) {
				return(shiny::tags$p("No prediction data available"))
			}

			uploaded_df <- uploaded_values()

			panels <- lapply(subs, function(subsec) {
				sub_df <- df[df$subsection == subsec, ]

				inputs <- lapply(seq_len(nrow(sub_df)), function(i) {
					row <- sub_df[i, ]

					# Override default value with uploaded value if present
					if (!is.null(uploaded_df)) {
						uploaded_val <- uploaded_df$value[uploaded_df$element_id == row$element_id]
						if (length(uploaded_val) == 1 && !is.null(uploaded_val) && nzchar(uploaded_val)) {
							row$value <- uploaded_val
						}
					}

					# Wrap optional fields
					div_class <- if (!is.null(row$optional) && as.integer(row$optional) == 1) "optional_field" else NULL

					# Render specific plots or inputs
					content <- if (row$element_type %in% c("prediction_area_plot", "geodist_plot_prediction")) {
						render_plot(
							element_id = ns(row$element_id),
							label = row$element,
							info_text = row$info_text
						)
					} else {
						render_input_field(
							element_type = row$element_type,
							element_id = ns(row$element_id),
							label = row$element,
							suggestions = row$suggestions,
							info_text = row$info_text,
							row = row
						)
					}

					shiny::tags$div(class = div_class, content)
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

		# Toggle CSS visibility for optional fields
		shiny::observe({
			if (isTRUE(hide_optional())) {
				shinyjs::addClass(selector = "body", class = "hide_optional")
			} else {
				shinyjs::removeClass(selector = "body", class = "hide_optional")
			}
		})

		# Observers for plots
		shiny::observe({
			input[["ui_rendered"]]
			shiny::req(o_objective_1_val() == "Model and prediction")
			render_plot_server(
				file = "prediction_area.png",
				valid_geo_metadata = valid_geo_prediction_area_metadata(),
				element_id = "prediction_map",
				objective = o_objective_1_val(),
				uploaded_zip = uploaded_zip(),
				output_dir = output_dir,
				ns = ns,
				output = output,
				plot_fn = function() {
					geo_map(
						output = output,
						element_id = "prediction_map",
						geo_metadata = valid_geo_prediction_area_metadata() %||% list(),
						what = "prediction_area_sf",
						output_dir = output_dir
					)
				}
			)
		})

		shiny::observe({
			input[["ui_rendered"]]
			shiny::req(o_objective_1_val() == "Model and prediction")
			render_plot_server(
				file = "geodist_prediction_area.png",
				valid_geo_metadata = valid_geo_all_metadata(),
				element_id = "geodistance_plot_prediction_area",
				objective = o_objective_1_val(),
				uploaded_zip = uploaded_zip(),
				output_dir = output_dir,
				ns = ns,
				output = output,
				plot_fn = function() {
					geodist_plot(
						output = output,
						element_id = "geodistance_plot_prediction_area",
						geo_metadata = valid_geo_all_metadata() %||% list(),
						objective = "Model and prediction",
						output_dir = output_dir
					)
				}
			)
		})

		# Reactive collection of prediction input values
		inputs_reactive <- shiny::reactive({
			df <- prediction_data()
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

		# Reactive getter for uncertainty quantification
		uncertainty_quantification <- shiny::reactive({
			input[["uncertainty_quantification"]]
		})
		evaluation_method <- shiny::reactive({
			input[["evaluation_strategy"]]
		})

		return(list(
			"prediction_inputs" = shiny::reactive(inputs_reactive()),
			"uncertainty_quantification" = uncertainty_quantification,
			"evaluation_method" = evaluation_method
		))
	})
}

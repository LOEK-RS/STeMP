#' Warnings Module - UI
#'
#' No visible UI elements. This module dynamically shows warning notifications
#' based on reactive inputs.
#'
#' @param id Module namespace ID
#' @return Empty UI placeholder (notifications appear dynamically)
#' @noRd
mod_warnings_ui <- function(id) {
	ns <- shiny::NS(id)
	shiny::tagList()
}

#' Warnings Module - Server
#'
#' Observes reactive inputs and shows context-specific warning notifications
#' to alert users about potential issues with sampling design, validation,
#' uncertainty quantification, and predictor types.
#'
#' @param id Module namespace ID
#' @param sampling_design Reactive returning current sampling design (e.g., "clustered", "random")
#' @param validation_method Reactive returning current model validation method (e.g., "Random Cross-Validation", "Spatial Cross-Validation")
#' @param evaluation_method Reactive returning current map evaluation method
#' @param uncertainty_quantification Reactive returning uncertainty quantification method (e.g., "none")
#' @param predictor_types Reactive returning a vector of predictor types (e.g., contains "Spatial Proxies")
#' @param show_warnings Should warnings be displayed?
#' @param o_objective_1_val Is only the model panel or also the prediction panel displayed?
#' @noRd
mod_warnings_server <- function(
	id,
	sampling_design,
	validation_method,
	evaluation_method,
	uncertainty_quantification,
	predictor_types,
	show_warnings = shiny::reactive(TRUE),
	o_objective_1_val
) {
	shiny::moduleServer(id, function(input, output, session) {
		warning_flags <- shiny::reactiveValues()

		# Utility to check condition and show notification only once
		check_and_warn <- function(condition, message, flag_name) {
			if (isTRUE(show_warnings()) && isTRUE(condition()) && is.null(warning_flags[[flag_name]])) {
				shiny::showNotification(message, type = "warning", duration = 10)
				warning_flags[[flag_name]] <- TRUE
			} else if ((!isTRUE(condition()) || !isTRUE(show_warnings())) && !is.null(warning_flags[[flag_name]])) {
				warning_flags[[flag_name]] <- NULL
			}
		}

		# Warning: CV for map accuracy estimation, when also using CV for model selection
		shiny::observe({
			shiny::req(validation_method(), evaluation_method(), o_objective_1_val() == "Model and prediction")

			is_problematic <- grepl("Cross-Validation", validation_method(), fixed = TRUE) &&
				grepl("Cross-Validation", evaluation_method(), fixed = TRUE)

			check_and_warn(
				condition = shiny::reactive({
					is_problematic
				}),
				message = shiny::HTML(
					"Using Cross-Validation for both, model selection and assessing the final prediction might lead to data leakage.",
					'See <a href="https://doi.org/10.1007/978-0-387-84858-7" target="_blank">Hastie et al., 2009</a>'
				),
				flag_name = "both_cv"
			)
		})

		# Warning: Random resampling with clustered samples can be optimistic during model selection
		shiny::observe({
			shiny::req(sampling_design(), validation_method())

			is_problematic <- sampling_design() == "clustered" &&
				grepl("Random", validation_method(), fixed = TRUE)

			check_and_warn(
				condition = shiny::reactive({
					is_problematic
				}),
				message = shiny::HTML(
					"Random resampling might yield overly optimistic results with clustered samples. ",
					'See <a href="https://doi.org/10.1111/ecog.02881" target="_blank">Roberts et al., 2017</a>,
         <a href="https://doi.org/10.1038/s41467-020-18321-y" target="_blank">Ploton et al., 2020</a>,
         <a href="https://doi.org/10.1111/2041-210X.13851" target="_blank">Mil\u00E0 et al., 2022</a>.'
				),
				flag_name = "clustered_random_cv"
			)
		})

		# Warning: Spatial resampling with random samples can be pessimistic during model selection
		shiny::observe({
			shiny::req(sampling_design(), validation_method())

			is_problematic <- sampling_design() == "random" &&
				grepl("Spatial", validation_method(), fixed = TRUE)

			check_and_warn(
				condition = shiny::reactive({
					is_problematic
				}),
				message = shiny::HTML(
					"Spatial resampling might yield overly pessimistic results with clustered samples. ",
					'See <a href="https://doi.org/10.1016/j.ecolmodel.2021.109692" target="_blank">Wadoux et al., 2021</a>,
         <a href="https://doi.org/10.1016/j.ecoinf.2022.101665" target="_blank">de Bruin et al., 2022</a>,
         <a href="https://doi.org/10.1111/2041-210X.13851" target="_blank">Mil\u00E0 et al., 2022</a>.'
				),
				flag_name = "random_clustered_cv"
			)
		})

		# Warning: Random resampling for map accuracy estimation with clustered samples can be optimistic
		shiny::observe({
			shiny::req(sampling_design(), evaluation_method(), o_objective_1_val() == "Model and prediction")

			is_problematic <- sampling_design() == "clustered" &&
				grepl("Random", evaluation_method(), fixed = TRUE)

			check_and_warn(
				condition = shiny::reactive({
					is_problematic
				}),
				message = shiny::HTML(
					"Random resampling might yield overly optimistic estimates of the map accuracy with clustered samples. ",
					'See <a href="https://doi.org/10.1111/ecog.02881" target="_blank">Roberts et al., 2017</a>,
         <a href="https://doi.org/10.1038/s41467-020-18321-y" target="_blank">Ploton et al., 2020</a>,
         <a href="https://doi.org/10.1111/2041-210X.13851" target="_blank">Mil\u00E0 et al., 2022</a>.'
				),
				flag_name = "clustered_random_ev"
			)
		})

		# Warning: Spatial resampling for map accuracy estimation with random samples can be pessimistic
		shiny::observe({
			shiny::req(sampling_design(), evaluation_method(), o_objective_1_val() == "Model and prediction")

			is_problematic <- sampling_design() == "random" &&
				grepl("Spatial", evaluation_method(), fixed = TRUE)

			check_and_warn(
				condition = shiny::reactive({
					is_problematic
				}),
				message = shiny::HTML(
					"Spatial resampling might yield overly pessimistic estimates of the map accuracy with clustered samples. ",
					'See <a href="https://doi.org/10.1016/j.ecolmodel.2021.109692" target="_blank">Wadoux et al., 2021</a>,
         <a href="https://doi.org/10.1016/j.ecoinf.2022.101665" target="_blank">de Bruin et al., 2022</a>,
         <a href="https://doi.org/10.1111/2041-210X.13851" target="_blank">Mil\u00E0 et al., 2022</a>.'
				),
				flag_name = "random_clustered_ev"
			)
		})

		# Warning: Spatial proxies + clustered samples = extrapolation risk
		shiny::observe({
			shiny::req(sampling_design(), predictor_types())

			is_problematic <- "Spatial Proxies" %in% predictor_types() && sampling_design() == "clustered"

			check_and_warn(
				condition = shiny::reactive({
					is_problematic
				}),
				message = shiny::HTML(
					"Warning: Using spatial proxies with clustered samples likely leads to extrapolation situations.\n
          Consider using physically relevant predictors instead.\n",
					'See <a href="https://doi.org/10.1016/j.ecolmodel.2019.108815" target="_blank">Meyer et al., 2019,
          <a href="https://doi.org/10.5194/gmd-17-6007-2024" target="_blank">Mil\u00E0 et al., 2024'
				),
				flag_name = "clustered_proxies"
			)
		})

		# Warning: Clustered samples + no uncertainty quantification
		shiny::observe({
			shiny::req(sampling_design(), uncertainty_quantification(), o_objective_1_val() == "Model and prediction")

			is_problematic <- sampling_design() == "clustered" &&
				uncertainty_quantification() == "None"

			check_and_warn(
				condition = shiny::reactive({
					is_problematic
				}),
				message = shiny::HTML(
					"Warning: Clustered samples often lead to extrapolation when the model is applied to feature combinations not present in the training data.<br>
          Identifying areas of extrapolation/uncertainty and communicating them to the user of the prediction is recommended.<br>
          See <a href='https://doi.org/10.1111/2041-210X.13650' target='_blank'>Meyer & Pebesma, 2021</a>,
          <a href='https://doi.org/10.1111/j.2041-210X.2010.00036.x' target='_blank'>Elith et al., 2010</a>."
				),
				flag_name = "clustered_noAssessment"
			)
		})
	})
}

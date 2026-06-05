#' Warnings Module - UI
#'
#' No visible UI elements. This module dynamically shows warning notifications
#' based on reactive inputs.
#'
#' @param id Module namespace ID
#' @return Empty UI placeholder (notifications appear dynamically)
#' @noRd
mod_warnings_ui <- function(id) {
	shiny::tagList()
}

#' Warnings Module - Server
#'
#' Observes reactive inputs and shows context-specific warning notifications
#' to alert users about potential issues with sampling design, validation,
#' uncertainty quantification, and predictor types.
#'
#' @param id Module namespace ID
#' @param sampling_design Reactive returning current sampling design, e.g. `"clustered"` or `"random"`.
#' @param validation_method Reactive returning current model evaluation method, e.g. `"Random Cross-Validation"`.
#' @param evaluation_method Reactive returning current map evaluation method.
#' @param uncertainty_quantification Reactive returning uncertainty quantification method, e.g. `"None"`.
#' @param predictor_types Reactive returning a vector of predictor types, e.g. containing `"Spatial Proxies"`.
#' @param show_warnings Reactive logical indicating whether warnings should be displayed.
#' @param o_objective_1_val Reactive indicating whether only the model panel or also the prediction panel is displayed.
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

		# ---------------------------------------------------------------------
		# Message formatting helpers
		# ---------------------------------------------------------------------

		make_ref <- function(label, url) {
			list(label = label, url = url)
		}

		format_sections <- function(sections) {
			shiny::tags$p(
				style = "margin-bottom: 4px;",
				shiny::tags$strong("Relevant sections: "),
				paste(sections, collapse = "; ")
			)
		}

		format_refs <- function(refs) {
			if (length(refs) == 0) {
				return(NULL)
			}

			ref_tags <- lapply(seq_along(refs), function(i) {
				shiny::tagList(
					shiny::tags$a(
						href = refs[[i]]$url,
						target = "_blank",
						refs[[i]]$label
					),
					if (i < length(refs)) {
						shiny::HTML(", ")
					} else {
						shiny::HTML(".")
					}
				)
			})

			shiny::tags$p(
				style = "margin-bottom: 0;",
				shiny::tags$strong("References: "),
				ref_tags
			)
		}

		make_warning_message <- function(issue, sections, refs = list()) {
			shiny::tags$div(
				style = "line-height: 1.35;",
				shiny::tags$p(
					style = "margin-bottom: 4px;",
					shiny::tags$strong("Potential issue: "),
					issue
				),
				format_sections(sections),
				format_refs(refs)
			)
		}

		check_and_warn <- function(condition, message, flag_name) {
			if (!isTRUE(show_warnings())) {
				warning_flags[[flag_name]] <- NULL
				return(invisible(NULL))
			}

			if (isTRUE(condition) && is.null(warning_flags[[flag_name]])) {
				shiny::showNotification(
					message,
					type = "warning",
					duration = 10,
					closeButton = TRUE
				)
				warning_flags[[flag_name]] <- TRUE
			} else if (!isTRUE(condition) && !is.null(warning_flags[[flag_name]])) {
				warning_flags[[flag_name]] <- NULL
			}

			invisible(NULL)
		}

		register_warning <- function(flag_name, condition, message) {
			shiny::observe({
				check_and_warn(
					condition = condition(),
					message = message,
					flag_name = flag_name
				)
			})
		}

		# ---------------------------------------------------------------------
		# References
		# ---------------------------------------------------------------------

		ref_hastie_2009 <- make_ref(
			"Hastie et al., 2009",
			"https://doi.org/10.1007/978-0-387-84858-7"
		)

		ref_roberts_2017 <- make_ref(
			"Roberts et al., 2017",
			"https://doi.org/10.1111/ecog.02881"
		)

		ref_ploton_2020 <- make_ref(
			"Ploton et al., 2020",
			"https://doi.org/10.1038/s41467-020-18321-y"
		)

		ref_wadoux_2021 <- make_ref(
			"Wadoux et al., 2021",
			"https://doi.org/10.1016/j.ecolmodel.2021.109692"
		)

		ref_de_bruin_2022 <- make_ref(
			"de Bruin et al., 2022",
			"https://doi.org/10.1016/j.ecoinf.2022.101665"
		)

		ref_mila_2022 <- make_ref(
			"Mil\u00E0 et al., 2022",
			"https://doi.org/10.1111/2041-210X.13851"
		)

		ref_meyer_2019 <- make_ref(
			"Meyer et al., 2019",
			"https://doi.org/10.1016/j.ecolmodel.2019.108815"
		)

		ref_mila_2024 <- make_ref(
			"Mil\u00E0 et al., 2024",
			"https://doi.org/10.5194/gmd-17-6007-2024"
		)

		ref_meyer_pebesma_2021 <- make_ref(
			"Meyer & Pebesma, 2021",
			"https://doi.org/10.1111/2041-210X.13650"
		)

		ref_elith_2010 <- make_ref(
			"Elith et al., 2010",
			"https://doi.org/10.1111/j.2041-210X.2010.00036.x"
		)

		# ---------------------------------------------------------------------
		# Warning: CV used both for model selection and final prediction assessment
		# ---------------------------------------------------------------------

		register_warning(
			flag_name = "both_cv",
			condition = function() {
				shiny::req(validation_method(), evaluation_method(), o_objective_1_val())

				o_objective_1_val() == "Model and prediction" &&
					grepl("Cross-Validation", validation_method(), fixed = TRUE) &&
					grepl("Cross-Validation", evaluation_method(), fixed = TRUE)
			},
			message = make_warning_message(
				issue = paste(
					"Using cross-validation both for model selection and for assessing the final",
					"prediction can lead to data leakage."
				),
				sections = c(
					"Model > Model evaluation and selection",
					"Prediction > Map evaluation and uncertainty assessment"
				),
				refs = list(
					ref_hastie_2009
				)
			)
		)

		# ---------------------------------------------------------------------
		# Warning: Random resampling with clustered training points during model selection
		# ---------------------------------------------------------------------

		register_warning(
			flag_name = "clustered_random_cv",
			condition = function() {
				shiny::req(sampling_design(), validation_method())

				sampling_design() == "clustered" &&
					grepl("Random", validation_method(), fixed = TRUE)
			},
			message = make_warning_message(
				issue = paste(
					"Random resampling might yield overly optimistic model performance estimates",
					"when training points are spatially clustered."
				),
				sections = c(
					"Model > Response",
					"Model > Model evaluation and selection"
				),
				refs = list(
					ref_roberts_2017,
					ref_ploton_2020,
					ref_mila_2022
				)
			)
		)

		# ---------------------------------------------------------------------
		# Warning: Spatial resampling with random training points during model selection
		# ---------------------------------------------------------------------

		register_warning(
			flag_name = "random_clustered_cv",
			condition = function() {
				shiny::req(sampling_design(), validation_method())

				sampling_design() == "random" &&
					grepl("Spatial", validation_method(), fixed = TRUE)
			},
			message = make_warning_message(
				issue = paste(
					"Spatial resampling might yield overly pessimistic model performance estimates",
					"when training points are randomly distributed relative to the prediction area."
				),
				sections = c(
					"Model > Response",
					"Model > Model evaluation and selection"
				),
				refs = list(
					ref_wadoux_2021,
					ref_de_bruin_2022,
					ref_mila_2022
				)
			)
		)

		# ---------------------------------------------------------------------
		# Warning: Random resampling with clustered training points for map accuracy
		# ---------------------------------------------------------------------

		register_warning(
			flag_name = "clustered_random_ev",
			condition = function() {
				shiny::req(sampling_design(), evaluation_method(), o_objective_1_val())

				o_objective_1_val() == "Model and prediction" &&
					sampling_design() == "clustered" &&
					grepl("Random", evaluation_method(), fixed = TRUE)
			},
			message = make_warning_message(
				issue = paste(
					"Random resampling might yield overly optimistic estimates of final map",
					"accuracy when training points are spatially clustered."
				),
				sections = c(
					"Model > Response",
					"Prediction > Map evaluation and uncertainty assessment"
				),
				refs = list(
					ref_roberts_2017,
					ref_ploton_2020,
					ref_mila_2022
				)
			)
		)

		# ---------------------------------------------------------------------
		# Warning: Spatial resampling with random training points for map accuracy
		# ---------------------------------------------------------------------

		register_warning(
			flag_name = "random_clustered_ev",
			condition = function() {
				shiny::req(sampling_design(), evaluation_method(), o_objective_1_val())

				o_objective_1_val() == "Model and prediction" &&
					sampling_design() == "random" &&
					grepl("Spatial", evaluation_method(), fixed = TRUE)
			},
			message = make_warning_message(
				issue = paste(
					"Spatial resampling might yield overly pessimistic estimates of final map",
					"accuracy when training points are randomly distributed relative to the prediction area."
				),
				sections = c(
					"Model > Response",
					"Prediction > Map evaluation and uncertainty assessment"
				),
				refs = list(
					ref_wadoux_2021,
					ref_de_bruin_2022,
					ref_mila_2022
				)
			)
		)

		# ---------------------------------------------------------------------
		# Warning: Spatial proxies + clustered training points = extrapolation risk
		# ---------------------------------------------------------------------

		register_warning(
			flag_name = "clustered_proxies",
			condition = function() {
				shiny::req(sampling_design(), predictor_types())

				sampling_design() == "clustered" &&
					"Spatial Proxies" %in% predictor_types()
			},
			message = make_warning_message(
				issue = "Using spatial proxies with clustered training points can increase the risk of extrapolation to combinations of predictor values not represented in the training data.",
				sections = c(
					"Model > Response",
					"Model > Predictors"
				),
				refs = list(
					ref_meyer_2019,
					ref_mila_2024
				)
			)
		)

		# ---------------------------------------------------------------------
		# Warning: Clustered training points + no uncertainty quantification
		# ---------------------------------------------------------------------

		register_warning(
			flag_name = "clustered_noAssessment",
			condition = function() {
				shiny::req(sampling_design(), uncertainty_quantification(), o_objective_1_val())

				o_objective_1_val() == "Model and prediction" &&
					sampling_design() == "clustered" &&
					any(grepl("None", uncertainty_quantification(), fixed = TRUE), na.rm = TRUE)
			},
			message = make_warning_message(
				issue = "Clustered training points can lead to extrapolation when the model is applied to feature combinations not present in the training data.",
				sections = c(
					"Model > Response",
					"Prediction > Map evaluation and uncertainty assessment"
				),
				refs = list(
					ref_meyer_pebesma_2021,
					ref_elith_2010
				)
			)
		)
	})
}

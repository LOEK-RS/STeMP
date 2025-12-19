#' Analyze STeMP output
#'
#' @param protocol A data.frame containing the downloaded STeMP object.
#' @param render Should the resulting table be rendered? By default TRUE.
#' @return A list containing the warnings as plain text, as well as the rendered table if `render` is TRUE.
#' @export
analyze_protocol <- function(protocol, render = TRUE) {
	# check if input is of class data.frame
	if (!is.data.frame(protocol)) {
		stop("input must be a data.frame")
	}

	# get values of relevant fields
	vals <- stats::setNames(protocol$value, protocol$element_id)
	sampling_design <- unname(vals["sampling_design"])
	validation_strategy <- unname(vals["validation_strategy"])
	evaluation_strategy <- unname(vals["evaluation_strategy"])
	predictor_types <- unname(vals["predictor_types"])
	uncertainty_quantification <- unname(vals["uncertainty_quantification"])

	# list of possible problems:
	# CV used for both model selection and final prediction assessment
	CV_both <- grepl("Cross-Validation", validation_strategy, fixed = TRUE) &&
		grepl("Cross-Validation", evaluation_strategy, fixed = TRUE)
	# sampling_design == "random" + validation_strategy == "Spatial Cross-Validation"
	random_design_spatial_CV <- sampling_design == "random" && grepl("Spatial", validation_strategy, fixed = TRUE)
	# sampling_design == "random" + evaluation_strategy == "Spatial Cross-Validation"
	random_design_spatial_Err <- sampling_design == "random" && grepl("Spatial", evaluation_strategy, fixed = TRUE)
	# sampling_design == "clustered" + validation_strategy == "Random Cross-Validation"
	clustered_design_spatial_CV <- sampling_design == "clustered" && grepl("Random", validation_strategy, fixed = TRUE)
	# sampling_design == "clustered" + evaluation_strategy == "Random Cross-Validation"
	clustered_design_spatial_Err <- sampling_design == "clustered" && grepl("Random", evaluation_strategy, fixed = TRUE)
	# sampling_design == "clustered" + predictor_types %in% "Spatial Proxies"
	clustered_design_spatial_proxies <- sampling_design == "clustered" &&
		grepl("Spatial Proxies", predictor_types, fixed = TRUE)
	# sampling_design == "clustered" + uncertainty_quantification == 'None'"
	clustered_design_no_uncert <- sampling_design == "clustered" &&
		(uncertainty_quantification == "None" | is.na(uncertainty_quantification))

	# Collect warnings
	warnings <- list()
	if (isTRUE(CV_both)) {
		warning_message <- shiny::HTML(
			"Cross-Validation was used for both model selection and evaluating the final prediction.<br>
				This can lead to data leakage. See <a href='https://doi.org/10.1007/978-0-387-84858-7' target='_blank'>Hastie et al., 2009</a>."
		)
		warnings <- append(warnings, warning_message)
	}

	if (isTRUE(random_design_spatial_CV)) {
		warning_message <- shiny::HTML(
			paste0(
				protocol[protocol$element_id == "validation_strategy", "value"],
				" was used for model selection, while the training samples were randomly distributed relative to the prediction area. 
					This might yield overly pessimistic results and thus bias model selection, or false mistrust in the models' performance if
					no final map accuracy is estimated. ",
				'See <a href="https://doi.org/10.1016/j.ecolmodel.2021.109692" target="_blank">Wadoux et al., 2021</a>,
   <a href="https://doi.org/10.1016/j.ecoinf.2022.101665" target="_blank">de Bruin et al., 2022</a>,
   <a href="https://doi.org/10.1111/2041-210X.13851" target="_blank">Mil\u00E0 et al., 2022</a>.'
			)
		)
		warnings <- append(warnings, warning_message)
	}

	if (isTRUE(random_design_spatial_Err)) {
		warning_message <- shiny::HTML(
			paste0(
				protocol[protocol$element_id == "validation_strategy", "value"],
				"  was used for final map accuracy estimation, while the training samples were randomly distributed relative to the prediction area. 
					 This might yield overly pessimistic results and give false mistrust in the resulting map. ",
				'See <a href="https://doi.org/10.1016/j.ecolmodel.2021.109692" target="_blank">Wadoux et al., 2021</a>,
   <a href="https://doi.org/10.1016/j.ecoinf.2022.101665" target="_blank">de Bruin et al., 2022</a>,
   <a href="https://doi.org/10.1111/2041-210X.13851" target="_blank">Mil\u00E0 et al., 2022</a>.'
			)
		)
		warnings <- append(warnings, warning_message)
	}

	if (isTRUE(clustered_design_spatial_CV)) {
		warning_message <- shiny::HTML(
			paste0(
				protocol[protocol$element_id == "validation_strategy", "value"],
				" was used for model selection, while the training samples were clustered relative to the prediction area. 
			This might yield overly optimistic results and thus bias model selection, or false confidence in the models' performance if
			no final map accuracy is estimated. ",
				'See <a href="https://doi.org/10.1016/j.ecolmodel.2021.109692" target="_blank">Wadoux et al., 2021</a>,
   <a href="https://doi.org/10.1016/j.ecoinf.2022.101665" target="_blank">de Bruin et al., 2022</a>,
   <a href="https://doi.org/10.1111/2041-210X.13851" target="_blank">Mil\u00E0 et al., 2022</a>.'
			)
		)
		warnings <- append(warnings, warning_message)
	}

	if (isTRUE(clustered_design_spatial_Err)) {
		warning_message <- shiny::HTML(
			paste0(
				protocol[protocol$element_id == "validation_strategy", "value"],
				" was used for final map accuracy estimation, while the training samples were clustered relative to the prediction area. 
			This might yield overly optimistic results and give false confidence in the resulting map. ",
				'See <a href="https://doi.org/10.1016/j.ecolmodel.2021.109692" target="_blank">Wadoux et al., 2021</a>,
   <a href="https://doi.org/10.1016/j.ecoinf.2022.101665" target="_blank">de Bruin et al., 2022</a>,
   <a href="https://doi.org/10.1111/2041-210X.13851" target="_blank">Mil\u00E0 et al., 2022</a>.'
			)
		)
		warnings <- append(warnings, warning_message)
	}

	if (isTRUE(clustered_design_spatial_proxies)) {
		warning_message <- shiny::HTML(
			"Using spatial proxies with clustered samples likely leads to extrapolation situations.",
			'See <a href="https://doi.org/10.1016/j.ecolmodel.2019.108815" target="_blank">Meyer et al., 2019,
          <a href="https://doi.org/10.5194/gmd-17-6007-2024" target="_blank">Mil\u00E0 et al., 2024'
		)
		warnings <- append(warnings, warning_message)
	}

	if (isTRUE(clustered_design_no_uncert)) {
		warning_message <- shiny::HTML(
			"The samples were clustered relative to the prediction area, which often leads to extrapolation when the model is
			 applied to feature combinations not present in the training data.<br>
				No methods to identify and communicate extrapolation areas were reported.<br>
          See <a href='https://doi.org/10.1111/2041-210X.13650' target='_blank'>Meyer & Pebesma, 2021</a>,
          <a href='https://doi.org/10.1111/j.2041-210X.2010.00036.x' target='_blank'>Elith et al., 2010</a>."
		)
		warnings <- append(warnings, warning_message)
	}

	out <- list("warnings_text" = paste(warnings, sep = "."))
	if (isTRUE(render)) {
		## Return data table
		out$rendered_table <- DT::datatable(data.frame(Warnings = unlist(warnings)), escape = FALSE)
	}

	class(out) <- "stemp_analysis"
	out
}

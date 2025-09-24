#' Model Metadata Extraction Module - UI
#'
#' UI for the model metadata module.
#' No UI elements are required unless exposing metadata visually.
#'
#' @param id Module namespace ID
#' @return UI output (empty tagList here)
#' @noRd
mod_model_metadata_ui <- function(id) {
	ns <- shiny::NS(id)
	shiny::tagList() # No UI needed unless exposing values explicitly
}

#' Model Metadata Extraction Module - Server
#'
#' Extracts metadata from a trained model object.
#' Supports caret, tidymodels, and mlr3 model objects.
#'
#' @param id Module namespace ID
#' @param input_model_object Reactive containing a trained model object
#' @return A list of reactive values with extracted metadata:
#'   - has_model: logical indicating if a model is present
#'   - num_training_samples: number of training samples
#'   - num_predictors: number of predictors/features
#'   - names_predictors: concatenated predictor names
#'   - model_algorithm: algorithm/engine name
#'   - model_type: model type ("Classification" or "Regression")
#'   - model_hyperparams: hyperparameters as string
#'   - num_classes: number of classes (for classification)
#'   - num_samples_per_class: class counts as string
#'   - interpolation_range: range of response variable (for regression)
#'   - validation_results: validation metric summary string
#' @noRd
mod_model_metadata_server <- function(id, input_model_object) {
	shiny::moduleServer(id, function(input, output, session) {
		# Initialize reactive values for metadata
		model_object <- shiny::reactiveVal(NULL)
		num_training_samples <- shiny::reactiveVal(NULL)
		num_predictors <- shiny::reactiveVal(NULL)
		names_predictors <- shiny::reactiveVal(NULL)
		model_algorithm <- shiny::reactiveVal(NULL)
		model_type <- shiny::reactiveVal(NULL)
		model_hyperparams <- shiny::reactiveVal(NULL)
		num_classes <- shiny::reactiveVal(NULL)
		num_samples_per_class <- shiny::reactiveVal(NULL)
		interpolation_range <- shiny::reactiveVal(NULL)
		validation_results <- shiny::reactiveVal(NULL)

		# Logical reactive indicating if model is present
		has_model <- shiny::reactive({
			!is.null(model_object())
		})

		shiny::observeEvent(input_model_object(), {
			model <- input_model_object()
			shiny::req(model)

			# Reset all metadata fields before processing new model
			model_object(NULL)
			num_training_samples(NULL)
			num_predictors(NULL)
			names_predictors(NULL)
			model_algorithm(NULL)
			model_type(NULL)
			model_hyperparams(NULL)
			num_classes(NULL)
			num_samples_per_class(NULL)
			interpolation_range(NULL)
			validation_results(NULL)

			model_object(model)

			# ----------- Caret Model -----------
			if ("train" %in% class(model)) {
				data <- model$trainingData
				if (!is.null(data)) {
					response <- data$.outcome
					predictors <- data[, setdiff(names(data), ".outcome"), drop = FALSE]

					num_training_samples(nrow(data))
					num_predictors(ncol(predictors))
					names_predictors(paste(names(predictors), collapse = ", "))
					model_type(model$modelType)
					model_algorithm(model$method %||% "")

					# Hyperparameters from bestTune
					if (!is.null(model$bestTune)) {
						model_hyperparams(paste0(names(model$bestTune), "=", model$bestTune, collapse = ", "))
					}

					if (model_type() == "Classification") {
						tab <- table(response)
						num_classes(length(tab))
						num_samples_per_class(paste0(names(tab), ": ", as.integer(tab), collapse = ", "))

						# Validation metrics: accuracy and kappa
						if (!is.null(model$resample)) {
							accuracy_val <- round(mean(model$resample$Accuracy, na.rm = TRUE), 3)
							kappa_val <- round(mean(model$resample$Kappa, na.rm = TRUE), 3)
							validation_results(sprintf("Accuracy = %.3f, Kappa = %.3f", accuracy_val, kappa_val))
						}
					} else {
						# Regression: response range and RMSE, R^2 metrics
						interpolation_range(paste(round(range(response), 3), collapse = " to "))
						if (!is.null(model$resample)) {
							rmse_val <- round(mean(model$resample$RMSE, na.rm = TRUE), 3)
							r2_val <- round(mean(model$resample$Rsquared, na.rm = TRUE), 3)
							validation_results(sprintf("RMSE = %.3f, R^2 = %.3f", rmse_val, r2_val))
						}
					}
				}

				# ----------- Tidymodels Model -----------
			} else if ("workflow" %in% class(model) || "model_fit" %in% class(model)) {
				# needed for dplyr use:
				.estimate <- .metric <- NULL

				fit <- if ("workflow" %in% class(model)) workflows::extract_fit_parsnip(model) else model

				# Try to extract mold (training data)
				data <- tryCatch(
					{
						if ("workflow" %in% class(model)) workflows::extract_mold(model) else NULL
					},
					error = function(e) NULL
				)

				model_algorithm(fit$spec$engine %||% "")
				model_type(firstup(fit$spec$mode) %||% "")

				# Extract hyperparameters
				if (!is.null(fit$spec$args)) {
					args <- lapply(fit$spec$args, function(x) if (is.symbol(x)) NA else x)
					model_hyperparams(paste0(names(args), "=", args, collapse = ", "))
				}

				if (!is.null(data)) {
					outcomes <- data$outcomes
					predictors <- data$predictors
					response <- outcomes[[1]]

					num_training_samples(nrow(outcomes))
					num_predictors(ncol(predictors))
					names_predictors(paste(names(predictors), collapse = ", "))

					if (is.factor(response)) {
						tab <- table(response)
						num_classes(length(tab))
						num_samples_per_class(paste0(names(tab), ": ", as.integer(tab), collapse = ", "))

						try(
							{
								# Attempt to extract metrics (currently may not work)
								metrics_tbl <- model |> workflows::extract_fit_engine() |> purrr::pluck(".metrics")
								if (!is.null(metrics_tbl)) {
									acc <- metrics_tbl |> dplyr::filter(.metric == "accuracy") |> dplyr::pull(.estimate)
									kap <- metrics_tbl |> dplyr::filter(.metric == "kap") |> dplyr::pull(.estimate)
									if (length(acc) && length(kap)) {
										validation_results(sprintf("Accuracy = %.3f, Kappa = %.3f", acc, kap))
									}
								}
							},
							silent = TRUE
						)
					} else {
						interpolation_range(paste(round(range(response), 3), collapse = " to "))
						try(
							{
								metrics_tbl <- model |> workflows::extract_fit_engine() |> purrr::pluck(".metrics")
								if (!is.null(metrics_tbl)) {
									rmse_val <- metrics_tbl |> dplyr::filter(.metric == "rmse") |> dplyr::pull(.estimate)
									r2_val <- metrics_tbl |> dplyr::filter(.metric == "rsq") |> dplyr::pull(.estimate)
									validation_results(sprintf("RMSE = %.3f, R^2 = %.3f", rmse_val, r2_val))
								}
							},
							silent = TRUE
						)
					}
				}

				# ----------- mlr3 Model -----------
			} else if (any(class(model) %in% c("Learner", "GraphLearner"))) {
				model_algorithm(model$label %||% "")
				model_type(if (grepl("classif", model$id)) "Classification" else "Regression")

				# Safely extract hyperparameters from mlr3 model
				ps <- tryCatch(model$param_set$values, error = function(e) NULL)
				if (!is.null(ps) && length(ps) > 0) {
					model_hyperparams(paste0(names(ps), "=", unlist(ps), collapse = ", "))
				} else {
					model_hyperparams("") # always character
				}

				# Try to extract training data from stored backend
				task <- tryCatch(
					{
						model$state$train_task
					},
					error = function(e) NULL
				)

				response <- NULL
				predictors <- NULL

				if (!is.null(task) && inherits(task, "Task")) {
					try(
						{
							task_data <- data.table::as.data.table(task, target = TRUE)
							response <- task_data[[task$target_names]]
							predictors <- task_data[, setdiff(names(task_data), task$target_names), with = FALSE]
						},
						silent = TRUE
					)
				}

				# Warn if no training data stored
				if (is.null(response) || is.null(predictors)) {
					shiny::showNotification(
						"Training data not stored in the mlr3 model. Set store_backends = TRUE when training the model to enable complete metadata extraction.",
						type = "warning"
					)
				}

				# Proceed only if data was stored
				if (!is.null(response) && !is.null(predictors)) {
					num_training_samples(nrow(predictors))
					num_predictors(ncol(predictors))
					names_predictors(paste(names(predictors), collapse = ", "))

					if (is.factor(response)) {
						tab <- table(response)
						num_classes(length(tab))
						num_samples_per_class(paste0(names(tab), ": ", as.integer(tab), collapse = ", "))

						# Classification performance metrics
						if (!is.null(model$state$model_performance)) {
							acc <- tryCatch(model$state$model_performance$score("classif.acc"), error = function(e) NA)
							kappa <- tryCatch(model$state$model_performance$score("classif.kappa"), error = function(e) NA)
							validation_results(sprintf("Accuracy = %.3f, Kappa = %.3f", acc, kappa))
						}
					} else {
						interpolation_range(paste(round(range(response), 3), collapse = " to "))

						if (!is.null(model$state$model_performance)) {
							rmse_val <- tryCatch(model$state$model_performance$score("rmse"), error = function(e) NA)
							r2_val <- tryCatch(model$state$model_performance$score("rsq"), error = function(e) NA)
							validation_results(sprintf("RMSE = %.3f, $R^2$ = %.3f", rmse_val, r2_val))
						}
					}
				}

				# ----------- Unsupported Model Type -----------
			} else {
				shiny::showNotification("Unsupported model type. Supported: caret, tidymodels, mlr3.", type = "error")
			}
		})

		# Return all reactive metadata for external use
		return(list(
			has_model = has_model,
			num_training_samples = num_training_samples,
			num_predictors = num_predictors,
			names_predictors = names_predictors,
			model_algorithm = model_algorithm,
			model_type = model_type,
			model_hyperparams = model_hyperparams,
			num_classes = num_classes,
			num_samples_per_class = num_samples_per_class,
			interpolation_range = interpolation_range,
			validation_results = validation_results
		))
	})
}

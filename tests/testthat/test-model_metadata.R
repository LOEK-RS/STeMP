test_that("Caret model metadata is correctly extracted", {
	# Load model
	model_caret <- readRDS(test_path("fixtures", "model_caret.RDS"))

	shiny::testServer(
		mod_model_metadata_server,
		args = list(input_model_object = reactive(model_caret)),
		{
			input_model_object() # trigger observer
			session$flushReact() # flush reactives so model_object() is updated

			expect_true(has_model())
			expect_true(is.numeric(num_training_samples()))
			expect_true(is.numeric(num_predictors()))
			expect_true(is.character(names_predictors()))
			expect_true(is.character(model_algorithm()))
			expect_true(is.character(model_type()))
			expect_true(is.character(model_hyperparams()))

			# Classification-specific fields
			if (!is.null(num_classes())) {
				expect_true(num_classes() > 0)
				expect_true(is.character(num_samples_per_class()))
			}

			# Regression-specific fields
			if (!is.null(interpolation_range())) {
				expect_true(is.character(interpolation_range()))
			}

			# Validation metrics
			if (!is.null(validation_results())) {
				expect_true(is.character(validation_results()))
			}
		}
	)
})

test_that("Tidymodels model metadata is correctly extracted", {
	# Load model
	model_tidymodels <- readRDS(test_path("fixtures", "model_tidymodels.RDS"))

	shiny::testServer(
		mod_model_metadata_server,
		args = list(input_model_object = reactive(model_tidymodels)),
		{
			input_model_object()
			session$flushReact()

			expect_true(has_model())
			expect_true(is.numeric(num_training_samples()))
			expect_true(is.numeric(num_predictors()))
			expect_true(is.character(names_predictors()))
			expect_true(is.character(model_algorithm()))
			expect_true(is.character(model_type()))
			expect_true(is.character(model_hyperparams()))

			if (!is.null(num_classes())) {
				expect_true(num_classes() > 0)
				expect_true(is.character(num_samples_per_class()))
			}
			if (!is.null(interpolation_range())) {
				expect_true(is.character(interpolation_range()))
			}
			if (!is.null(validation_results())) {
				expect_true(is.character(validation_results()))
			}
		}
	)
})

test_that("mlr3 model metadata is correctly extracted", {
	# Load model
	model_mlr3 <- readRDS(test_path("fixtures", "model_mlr3.RDS"))

	shiny::testServer(
		mod_model_metadata_server,
		args = list(input_model_object = reactive(model_mlr3)),
		{
			input_model_object()
			session$flushReact() # flush reactives so model_object() is updated

			expect_true(has_model())
			expect_true(is.character(model_algorithm()))
			expect_true(is.character(model_type()))
			expect_true(is.character(model_hyperparams()))

			# mlr3 classification fields
			if (!is.null(num_classes())) {
				expect_true(num_classes() > 0)
				expect_true(is.character(num_samples_per_class()))
			}
			# mlr3 regression fields
			if (!is.null(interpolation_range())) {
				expect_true(is.character(interpolation_range()))
			}
			if (!is.null(validation_results())) {
				expect_true(is.character(validation_results()))
			}
		}
	)
})

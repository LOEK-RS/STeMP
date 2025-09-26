library(shinytest2)

test_that("{shinytest2} recording: STeMP", {
	skip_on_cran()

	app <- AppDriver$new(
		variant = platform_variant(),
		name = "STeMP",
		seed = 10,
		height = 955,
		width = 1619
	)
	app$set_inputs(navbar = "import")
	app$upload_file(`upload-samples-upload` = test_path("fixtures", "clustered_points.gpkg"))
	app$upload_file(`upload-prediction_area-upload` = test_path("fixtures", "polygon.gpkg"))
	app$set_inputs(navbar = "create")
	app$set_inputs(`protocol-overview-scientific_field` = character(0), wait_ = FALSE)
	app$set_inputs(`protocol-overview-model_title` = "", wait_ = FALSE)
	app$set_inputs(`protocol-overview-author_names` = "", wait_ = FALSE)
	app$set_inputs(`protocol-overview-contact` = "", wait_ = FALSE)
	app$set_inputs(`protocol-overview-study_title` = "", wait_ = FALSE)
	app$set_inputs(`protocol-overview-study_link` = "", wait_ = FALSE)
	app$set_inputs(`protocol-overview-target_variable` = "", wait_ = FALSE)

	app$set_inputs(`protocol-tabset` = "Model", wait_ = FALSE)

	app$wait_for_idle()

	app$run_js(
		"
  $('.panel-collapse').collapse('show');
"
	)
	app$wait_for_idle()

	app$set_inputs(`protocol-model-training_domain` = character(0), wait_ = FALSE)
	app$set_inputs(`protocol-model-sample_acquisition` = character(0), wait_ = FALSE)
	app$set_inputs(`protocol-model-sample_geometry` = character(0), wait_ = FALSE)
	app$set_inputs(`protocol-model-sampling_design` = "clustered", wait_ = FALSE)
	app$set_inputs(`protocol-model-predictor_types` = character(0), wait_ = FALSE)
	app$set_inputs(`protocol-model-preprocessing` = character(0), wait_ = FALSE)
	app$set_inputs(`protocol-model-model_type` = "", wait_ = FALSE)
	app$set_inputs(`protocol-model-learning_method` = "", wait_ = FALSE)
	app$set_inputs(`protocol-model-validation_strategy` = "None", wait_ = FALSE)
	app$set_inputs(`protocol-model-performance_metrics` = character(0), wait_ = FALSE)
	app$set_inputs(`protocol-model-predictor_selection` = character(0), wait_ = FALSE)
	app$set_inputs(`protocol-model-explainability` = character(0), wait_ = FALSE)
	app$set_inputs(`protocol-model-sample_size` = character(0), wait_ = FALSE)
	app$set_inputs(`protocol-model-classes` = character(0), wait_ = FALSE)
	app$set_inputs(`protocol-model-number_of_predictors` = character(0), wait_ = FALSE)
	app$set_inputs(`protocol-model-samples_per_class` = "", wait_ = FALSE)
	app$set_inputs(`protocol-model-range` = "", wait_ = FALSE)
	app$set_inputs(`protocol-model-coordinate_reference_system` = "EPSG:25832", wait_ = FALSE)
	app$set_inputs(`protocol-model-data_sources` = "", wait_ = FALSE)
	app$set_inputs(`protocol-model-names_of_predictors` = "", wait_ = FALSE)
	app$set_inputs(`protocol-model-architecture` = "", wait_ = FALSE)
	app$set_inputs(`protocol-model-potential_biases` = "", wait_ = FALSE)
	app$set_inputs(`protocol-model-limitations` = "", wait_ = FALSE)
	app$set_inputs(`protocol-model-validation_results` = "", wait_ = FALSE)
	app$set_inputs(`protocol-model-hyperparameter_tuning` = "", wait_ = FALSE)
	app$set_inputs(`protocol-model-scientific_interpretation` = "", wait_ = FALSE)
	app$set_inputs(`protocol-model-software` = "", wait_ = FALSE)
	app$set_inputs(`protocol-model-code_availability` = "", wait_ = FALSE)
	app$set_inputs(`protocol-model-data_availability` = "", wait_ = FALSE)
	app$wait_for_idle()

	app$expect_screenshot()
	app$set_inputs(`protocol-model-validation_strategy` = "Random Cross-Validation")
	app$wait_for_idle()

	app$expect_screenshot()
	app$set_inputs(`protocol-tabset` = "Prediction")
	app$wait_for_idle()

	app$run_js(
		"
  $('.panel-collapse').collapse('show');
"
	)
	app$wait_for_idle()

	app$expect_screenshot()
	app$stop()
})

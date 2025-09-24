test_that("Sampling design is correctly derived from geo_metadata (both objectives)", {
	protocol_df <- read.csv(app_sys("app/www/stemp_dict.csv"))
	samples <- sf::st_read(test_path("fixtures/random_points.gpkg"), quiet = TRUE)
	training_area <- sf::st_read(test_path("fixtures/polygon_extrapolation.gpkg"), quiet = TRUE)
	prediction_area <- sf::st_read(test_path("fixtures/polygon.gpkg"), quiet = TRUE)

	# Define geo_metadata
	geo_metadata_training <- shiny::reactiveValues(
		samples_sf = shiny::reactive(samples),
		training_area_sf = shiny::reactive(training_area),
		prediction_area_sf = shiny::reactive(training_area) # just placeholder
	)

	geo_metadata_prediction <- shiny::reactiveValues(
		samples_sf = shiny::reactive(samples),
		training_area_sf = shiny::reactive(training_area), # just placeholder
		prediction_area_sf = shiny::reactive(prediction_area)
	)

	# ---- Model only ----
	testServer(
		mod_create_protocol_server,
		args = list(
			id = "protocol",
			protocol_data = shiny::reactive(protocol_df),
			uploaded_csv = shiny::reactive(NULL),
			model_metadata = shiny::reactiveValues(),
			geo_metadata = geo_metadata_training,
			output_dir = tempdir(),
			model_deleted = shiny::reactive(FALSE),
			csv_deleted = shiny::reactive(FALSE),
			show_warnings = shiny::reactive(FALSE),
			hide_optional = shiny::reactive(FALSE)
		),
		{
			session$setInputs(`overview-o_objective_1` = "Model only")
			session$flushReact()

			# simulate what render_design_server does
			session$setInputs(
				`model-sampling_design` = calculate_geodist_classification(
					geo_metadata_training$samples_sf(),
					geo_metadata_training$training_area_sf()
				)
			)
			session$flushReact()

			df <- session$getReturned()$protocol_updated()
			sd_value <- df$value[df$element == "Sampling design*"]
			expect_equal(sd_value, "clustered")
		}
	)

	# ---- Model and prediction ----
	testServer(
		mod_create_protocol_server,
		args = list(
			id = "protocol",
			protocol_data = shiny::reactive(protocol_df),
			uploaded_csv = shiny::reactive(NULL),
			model_metadata = shiny::reactiveValues(),
			geo_metadata = geo_metadata_prediction,
			output_dir = tempdir(),
			model_deleted = shiny::reactive(FALSE),
			csv_deleted = shiny::reactive(FALSE),
			show_warnings = shiny::reactive(FALSE),
			hide_optional = shiny::reactive(FALSE)
		),
		{
			session$setInputs(`overview-o_objective_1` = "Model and prediction")
			session$flushReact()

			session$setInputs(
				`model-sampling_design` = calculate_geodist_classification(
					geo_metadata_prediction$samples_sf(),
					geo_metadata_prediction$prediction_area_sf()
				)
			)
			session$flushReact()

			df <- session$getReturned()$protocol_updated()
			sd_value <- df$value[df$element == "Sampling design*"]
			expect_equal(sd_value, "random")
		}
	)
})

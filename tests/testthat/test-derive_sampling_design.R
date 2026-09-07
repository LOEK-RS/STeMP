test_that("Sampling design is correctly derived from geo_metadata (both objectives)", {
	protocol_df <- read.csv(app_sys("app/www/stemp_dict.csv"))
	samples <- sf::st_read(test_path("fixtures/random_points.gpkg"), quiet = TRUE)
	training_area <- sf::st_read(test_path("fixtures/polygon_extrapolation.gpkg"), quiet = TRUE)
	prediction_area <- sf::st_read(test_path("fixtures/polygon.gpkg"), quiet = TRUE)

	# Define geo_metadata
	geo_metadata_training <- shiny::reactiveValues(
		samples_sf = shiny::reactive(samples),
		training_area_sf = shiny::reactive(training_area),
		prediction_area_sf = shiny::reactive(training_area), # just placeholder
		has_samples = shiny::reactive(TRUE),
		has_training_area = shiny::reactive(TRUE),
		has_prediction_area = shiny::reactive(TRUE)
	)

	geo_metadata_prediction <- shiny::reactiveValues(
		samples_sf = shiny::reactive(samples),
		training_area_sf = shiny::reactive(training_area), # just placeholder
		prediction_area_sf = shiny::reactive(prediction_area),
		has_samples = shiny::reactive(TRUE),
		has_training_area = shiny::reactive(TRUE),
		has_prediction_area = shiny::reactive(TRUE)
	)

	# ---- Model only ----
	testServer(
		mod_create_protocol_server,
		args = list(
			id = "protocol",
			protocol_data = shiny::reactive(protocol_df),
			uploaded_csv = shiny::reactive(NULL),
			uploaded_zip = shiny::reactive(NULL),
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
			sd_value <- df$value[df$element == "Sampling pattern"]
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
			uploaded_zip = shiny::reactive(NULL),
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
			sd_value <- df$value[df$element == "Sampling pattern"]
			expect_equal(sd_value, "random")
		}
	)
})


test_that("Oversized input is refused with a reason instead of a classification", {
	samples <- sf::st_read(test_path("fixtures/random_points.gpkg"), quiet = TRUE)
	area <- sf::st_read(test_path("fixtures/polygon.gpkg"), quiet = TRUE)

	# The refusal is measured after deduplication, so the fixture has to carry
	# more distinct geometries than the mocked limit below.
	n_distinct <- nrow(unique_geometries(samples))
	skip_if(n_distinct <= 10, "fixture has too few distinct locations to trip the cap")

	testthat::local_mocked_bindings(geodist_max_n = function() 10L)

	reason <- geodist_geographic_data(samples, area)

	expect_type(reason, "character")
	expect_length(reason, 1L)
	expect_false(inherits(reason, "data.frame"))
	expect_match(reason, "^Too many sample locations")
	expect_match(reason, format(n_distinct, big.mark = ","), fixed = TRUE)
	expect_match(reason, "limit 10", fixed = TRUE)

	# The refusal must propagate as "no answer", never as a bogus one.
	expect_null(classify_geodist(reason))
	expect_null(calculate_geodist_classification(samples, area))
})


test_that("classify_geodist rejects every non-data-frame input", {
	expect_null(classify_geodist(NULL))
	expect_null(classify_geodist("Too many sample locations (12,000 distinct, limit 10,000)."))
	expect_null(classify_geodist(character(0)))
	expect_null(classify_geodist(NA))

	# A data frame with too few distances is also unanswerable.
	too_few <- data.frame(
		what = c("sample-to-sample", "prediction-to-sample"),
		dist = c(1, 2)
	)
	expect_null(classify_geodist(too_few))
})


test_that("Refused distances leave the sampling design blank without erroring", {
	protocol_df <- read.csv(app_sys("app/www/stemp_dict.csv"))
	samples <- sf::st_read(test_path("fixtures/random_points.gpkg"), quiet = TRUE)
	area <- sf::st_read(test_path("fixtures/polygon.gpkg"), quiet = TRUE)

	skip_if(nrow(unique_geometries(samples)) <= 10, "fixture too small to trip the cap")

	testthat::local_mocked_bindings(geodist_max_n = function() 10L)

	geo_metadata <- shiny::reactiveValues(
		samples_sf = shiny::reactive(samples),
		training_area_sf = shiny::reactive(area),
		prediction_area_sf = shiny::reactive(area),
		has_samples = shiny::reactive(TRUE),
		has_training_area = shiny::reactive(TRUE),
		has_prediction_area = shiny::reactive(TRUE)
	)

	shiny::testServer(
		mod_create_protocol_server,
		args = list(
			id = "protocol",
			protocol_data = shiny::reactive(protocol_df),
			uploaded_csv = shiny::reactive(NULL),
			uploaded_zip = shiny::reactive(NULL),
			model_metadata = shiny::reactiveValues(),
			geo_metadata = geo_metadata,
			output_dir = tempdir(),
			model_deleted = shiny::reactive(FALSE),
			csv_deleted = shiny::reactive(FALSE),
			show_warnings = shiny::reactive(FALSE),
			hide_optional = shiny::reactive(FALSE)
		),
		{
			session$setInputs(`overview-o_objective_1` = "Model and prediction")

			# The notification observer reads the same reactive; an unhandled
			# error there would take down the session rather than the field.
			expect_no_error(session$flushReact())

			df <- session$getReturned()$protocol_updated()
			sd_value <- df$value[df$element == "Sampling pattern"]

			# Blank and editable, never the string "NULL" or a guessed answer.
			expect_true(length(sd_value) == 0 || is.na(sd_value) || !nzchar(sd_value))
			expect_false(isTRUE(sd_value %in% c("random", "clustered", "NULL")))
		}
	)
})

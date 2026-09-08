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
			sd_value <- df$value[df$element == "Geographical sampling pattern"]
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
			sd_value <- df$value[df$element == "Geographical sampling pattern"]
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
			sd_value <- df$value[df$element == "Geographical sampling pattern"]

			# Blank and editable, never the string "NULL" or a guessed answer.
			expect_true(length(sd_value) == 0 || is.na(sd_value) || !nzchar(sd_value))
			expect_false(isTRUE(sd_value %in% c("random", "clustered", "NULL")))
		}
	)
})

testthat::test_that("too many timestamps refuses the temporal side only", {
	testthat::local_mocked_bindings(geodist_max_n = function() 100L)

	s <- make_size_sf(n_locations = 20L, n_times = 300L)
	a <- make_size_area()

	expect_equal(nrow(unique_geometries(s)), 20)
	expect_equal(nrow(unique_times(s)), 300)

	expect_match(geodist_temporal_data(s, a), "Too many sample timestamps")
	expect_null(calculate_temporal_geodist_classification(s, a))
	expect_s3_class(geodist_geographic_data(s, a), "data.frame")
	expect_type(calculate_geodist_classification(s, a), "character")
})

testthat::test_that("too many locations refuses the geographic side only", {
	testthat::local_mocked_bindings(geodist_max_n = function() 100L)

	s <- make_size_sf(n_locations = 300L, n_times = 24L)
	a <- make_size_area()

	expect_equal(nrow(unique_geometries(s)), 300)
	expect_equal(nrow(unique_times(s)), 24)

	expect_match(geodist_geographic_data(s, a), "Too many sample locations")
	expect_null(calculate_geodist_classification(s, a))
	expect_s3_class(geodist_temporal_data(s, a), "data.frame")
	expect_type(calculate_temporal_geodist_classification(s, a), "character")
})

testthat::test_that("both dimensions can be refused at once", {
	testthat::local_mocked_bindings(geodist_max_n = function() 100L)

	s <- make_size_sf(n_locations = 300L, n_times = 300L)
	a <- make_size_area()

	geo <- geodist_geographic_data(s, a)
	tim <- geodist_temporal_data(s, a)

	expect_match(geo, "Too many sample locations")
	expect_match(tim, "Too many sample timestamps")
	expect_null(calculate_geodist_classification(s, a))
	expect_null(calculate_temporal_geodist_classification(s, a))

	msg <- geodist_refusal_message(geo, tim)
	expect_match(msg, "Geographic dimension")
	expect_match(msg, "Temporal dimension")
	expect_match(msg, "and the <b>temporal sampling pattern</b>")
})

testthat::test_that("the refusal message names the affected dimension", {
	expect_null(geodist_refusal_message())
	expect_match(geodist_refusal_message(geo_reason = "x"), "Geographic dimension")
	expect_false(grepl("Temporal", geodist_refusal_message(geo_reason = "x")))
	expect_match(geodist_refusal_message(time_reason = "y"), "Temporal dimension")
	expect_match(geodist_refusal_message("x", "y"), "and the <b>temporal sampling pattern</b>")
})

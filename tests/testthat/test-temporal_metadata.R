test_that("date-time samples fill the temporal metadata reactives", {
	fx <- temporal_fixtures()

	shiny::testServer(
		mod_spatialdata_metadata_server,
		args = list(
			samples = fake_layer(fx$posix),
			training_area = fake_layer(NULL),
			prediction_area = fake_layer(fx$area_posix)
		),
		{
			meta <- session$getReturned()

			expect_true(meta$has_samples_time())
			expect_true(meta$has_prediction_time())

			expect_match(meta$temporal_extent(), "^2020-03-01 09:30:00 to ")
			expect_identical(meta$temporal_resolution(), "1 week")
			expect_identical(meta$n_timesteps(), 12L)

			expect_match(meta$prediction_temporal_extent(), "^2020-03-01 09:30:00 to ")
			expect_identical(meta$prediction_temporal_resolution(), "1 week")
		}
	)
})

test_that("numeric time leaves every temporal field empty", {
	fx <- temporal_fixtures()

	shiny::testServer(
		mod_spatialdata_metadata_server,
		args = list(
			samples = fake_layer(fx$numeric),
			training_area = fake_layer(NULL),
			prediction_area = fake_layer(fx$area_numeric)
		),
		{
			meta <- session$getReturned()

			# The data uploaded fine; only the time information is unusable.
			expect_true(meta$has_samples())
			expect_true(meta$has_prediction_area())

			expect_false(meta$has_samples_time())
			expect_false(meta$has_prediction_time())

			# "" rather than NULL: render_text_input_geo_server() writes this
			# straight into updateTextInput(), and a req() abort here would
			# leave a stale value from a previous upload on screen.
			expect_identical(meta$temporal_extent(), "")
			expect_identical(meta$temporal_resolution(), "")
			expect_identical(meta$prediction_temporal_extent(), "")
			expect_identical(meta$prediction_temporal_resolution(), "")

			# n_timesteps feeds updateNumericInput(), so NA not "".
			expect_true(is.na(meta$n_timesteps()))
		}
	)
})

test_that("integer years are treated the same as any other number", {
	fx <- temporal_fixtures()

	shiny::testServer(
		mod_spatialdata_metadata_server,
		args = list(
			samples = fake_layer(fx$integer_year),
			training_area = fake_layer(NULL),
			prediction_area = fake_layer(NULL)
		),
		{
			meta <- session$getReturned()

			expect_false(meta$has_samples_time())
			expect_identical(meta$temporal_extent(), "")
		}
	)
})

test_that("a missing time column is not an upload failure", {
	fx <- temporal_fixtures()

	shiny::testServer(
		mod_spatialdata_metadata_server,
		args = list(
			samples = fake_layer(fx$no_time),
			training_area = fake_layer(NULL),
			prediction_area = fake_layer(fx$area_no_time)
		),
		{
			meta <- session$getReturned()

			expect_true(meta$has_samples())
			expect_false(meta$has_samples_time())
			expect_identical(meta$temporal_extent(), "")
			expect_identical(meta$samples_crs(), "EPSG:4326")
		}
	)
})

test_that("replacing dated data with numeric data clears the fields", {
	fx <- temporal_fixtures()
	current <- shiny::reactiveVal(fx$posix)

	shiny::testServer(
		mod_spatialdata_metadata_server,
		args = list(
			samples = list(data = current, valid = shiny::reactive(TRUE)),
			training_area = fake_layer(NULL),
			prediction_area = fake_layer(NULL)
		),
		{
			meta <- session$getReturned()
			expect_identical(meta$temporal_resolution(), "1 week")

			current(fx$numeric)
			session$flushReact()

			# The regression this guards: with req() in the reactive, the old
			# value survives the swap.
			expect_identical(meta$temporal_extent(), "")
			expect_identical(meta$temporal_resolution(), "")
		}
	)
})

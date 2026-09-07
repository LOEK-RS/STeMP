test_that("coerce_time_column drops unparsed rows and keeps geometry aligned", {
	fx <- temporal_fixtures()

	out <- coerce_time_column(fx$partial_na)
	expect_identical(nrow(out), 6L)
	expect_identical(nrow(out), length(sf::st_geometry(out)))
	expect_s3_class(out[[stemp_time_column()]], "POSIXct")
	expect_false(any(is.na(out[[stemp_time_column()]])))
})

test_that("coerce_time_column enforces a two-row floor", {
	fx <- temporal_fixtures()

	expect_null(coerce_time_column(fx$one_usable))
	expect_identical(nrow(coerce_time_column(fx$two_usable)), 2L)
	expect_null(coerce_time_column(fx$numeric))
	expect_null(coerce_time_column(fx$no_time))
})

test_that("numeric time yields no temporal geodist", {
	fx <- temporal_fixtures()

	# Each of these must return before reaching CAST::geodist().
	expect_null(geodist_temporal_data(fx$numeric, fx$area_numeric))
	expect_null(geodist_temporal_data(fx$posix, fx$area_numeric))
	expect_null(geodist_temporal_data(fx$numeric, fx$area_posix))
	expect_null(geodist_temporal_data(fx$integer_year, fx$area_posix))
})

test_that("unusable or too-short time yields no temporal geodist", {
	fx <- temporal_fixtures()

	expect_null(geodist_temporal_data(fx$no_time, fx$area_posix))
	expect_null(geodist_temporal_data(fx$posix, fx$area_no_time))
	expect_null(geodist_temporal_data(fx$unparseable, fx$area_posix))

	# has_usable_time() is TRUE here but coerce_time_column() is NULL: the gap
	# that used to reach CAST::geodist(NULL, preddata = NULL).
	expect_true(has_usable_time(fx$one_usable))
	expect_null(geodist_temporal_data(fx$one_usable, fx$area_posix))
})

test_that("date-time pairs produce a temporal geodist table", {
	skip_if_not_installed("CAST")
	fx <- temporal_fixtures()

	res <- geodist_temporal_data(fx$posix, fx$area_posix)

	expect_s3_class(res, "data.frame")
	expect_true(all(c("dist", "what") %in% names(res)))
	expect_true(any(res$dist > 0, na.rm = TRUE))
})

test_that("mixed Date and text inputs land on one scale", {
	skip_if_not_installed("CAST")
	fx <- temporal_fixtures()

	# Both sides go through parse_time_column(), so a Date column and an ISO
	# text column are differenced in the same unit.
	res <- geodist_temporal_data(
		fx$date,
		make_time_area(
			format(as.Date("2020-03-01") + seq(0, by = 7, length.out = 12), "%Y-%m-%d")
		)
	)

	expect_s3_class(res, "data.frame")
	expect_true(any(res$dist > 0, na.rm = TRUE))
})

test_that("plotting a temporal geodist table gives a ggplot", {
	skip_if_not_installed("CAST")
	fx <- temporal_fixtures()

	res <- geodist_temporal_data(fx$posix, fx$area_posix)
	p <- plot(res)

	expect_s3_class(p, "ggplot")
	expect_no_error(add_log_scale_if_needed(p, res))
})

test_that("identical timestamps do not error", {
	skip_if_not_installed("CAST")
	fx <- temporal_fixtures()

	# All-zero distances: ks.test() inside the classifier warns about ties, so
	# this pins behaviour rather than asserting a particular class.
	expect_no_error(geodist_temporal_data(
		fx$constant,
		make_time_area(
			rep(as.POSIXct("2020-03-01 09:30:00", tz = "UTC"), 12)
		)
	))
})

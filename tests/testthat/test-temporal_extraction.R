test_that("typed date-time columns are accepted", {
	fx <- temporal_fixtures()

	expect_s3_class(parse_time_column(fx$posix), "POSIXct")
	expect_s3_class(parse_time_column(fx$date), "POSIXct")
	expect_true(has_usable_time(fx$posix))
	expect_true(has_usable_time(fx$date))
})

test_that("Date becomes midnight UTC", {
	parsed <- parse_time_column(temporal_fixtures()$date)

	expect_identical(attr(parsed, "tzone"), "UTC")
	expect_identical(format(parsed[1], tz = "UTC"), "2020-03-01")
})

test_that("parseable text becomes POSIXct", {
	fx <- temporal_fixtures()

	expect_s3_class(parse_time_column(fx$text_iso), "POSIXct")
	expect_s3_class(parse_time_column(fx$text_datetime), "POSIXct")
	expect_s3_class(parse_time_column(fx$text_iso8601), "POSIXct")
	expect_s3_class(parse_time_column(fx$factor_iso), "POSIXct")
})

test_that("ISO 8601 with T and Z keeps the clock time", {
	parsed <- parse_time_column(temporal_fixtures()$text_iso8601)

	expect_identical(format(parsed[1], "%H:%M", tz = "UTC"), "09:30")
})

test_that("plain numbers are rejected", {
	fx <- temporal_fixtures()

	expect_null(parse_time_column(fx$numeric))
	expect_null(parse_time_column(fx$integer_year))
	expect_null(parse_time_column(fx$double_year))

	expect_false(has_usable_time(fx$numeric))
	expect_false(has_usable_time(fx$integer_year))
})

test_that("unparseable text returns NULL instead of erroring", {
	fx <- temporal_fixtures()

	# as.POSIXct.character stop()s unless one format matches every non-NA
	# element, so parse_time_strings() has to swallow the condition.
	expect_no_error(parse_time_column(fx$partial))
	expect_null(parse_time_column(fx$partial))
	expect_null(parse_time_column(fx$unparseable))
})

test_that("mixed formats parse leniently, truncating to midnight", {
	fx <- temporal_fixtures()
	parsed <- parse_time_column(fx$mixed_formats)

	# No format matches every element, so as.POSIXct() falls back to "%Y-%m-%d"
	# and strptime()'s trailing-character tolerance drops the clock time from
	# the ISO half. Lossy, but not an error: pinned so a future parser change
	# that rejects the column outright shows up here.
	expect_s3_class(parsed, "POSIXct")
	expect_true(all(format(parsed, "%H:%M:%S", tz = "UTC") == "00:00:00"))
})

test_that("ISO 8601 without a zone designator keeps its clock time", {
	naive <- make_time_sf(c(
		"2020-03-01T09:30:00",
		"2020-03-01T11:45:00",
		"2020-03-01T14:00:00"
	))
	parsed <- parse_time_column(naive)

	expect_s3_class(parsed, "POSIXct")
	expect_identical(format(parsed[1], "%H:%M", tz = "UTC"), "09:30")
	expect_identical(format_time_resolution(parsed), "2.2 hours")
})

test_that("absent, empty and all-NA columns return NULL", {
	fx <- temporal_fixtures()

	expect_null(parse_time_column(fx$no_time))
	expect_null(parse_time_column(fx$all_na))
	expect_null(parse_time_column(NULL))
	expect_false(has_usable_time(fx$no_time))
})

test_that("NA gaps parse, leaving NAs in place", {
	fx <- temporal_fixtures()

	parsed <- parse_time_column(fx$partial_na)
	expect_s3_class(parsed, "POSIXct")
	expect_identical(sum(!is.na(parsed)), 6L)

	# Whitespace-only entries are normalised to NA before parsing.
	blank <- parse_time_column(fx$blank_text)
	expect_s3_class(blank, "POSIXct")
	expect_identical(sum(!is.na(blank)), 6L)
})

test_that("extent, timesteps and resolution read a weekly series", {
	parsed <- parse_time_column(temporal_fixtures()$date)

	expect_identical(
		format_time_extent(parsed),
		paste(format(range(parsed)), collapse = " to ")
	)
	expect_identical(count_timesteps(parsed), 12L)
	expect_identical(format_time_resolution(parsed), "1 week")
})

test_that("resolution picks a unit from the median gap", {
	fx <- temporal_fixtures()

	expect_identical(format_time_resolution(parse_time_column(fx$minutes)), "5 minutes")
	expect_identical(
		format_time_resolution(as.POSIXct("2020-03-01", tz = "UTC") + c(0, 86400, 172800)),
		"1 day"
	)
})

test_that("resolution is NULL when there is nothing to space out", {
	fx <- temporal_fixtures()

	expect_null(format_time_resolution(parse_time_column(fx$constant)))
	expect_null(format_time_resolution(parse_time_column(fx$single)))
	expect_null(format_time_resolution(NULL))
})

test_that("sub-second spacing does not index breaks with -Inf", {
	times <- as.POSIXct("2020-03-01", tz = "UTC") + c(0, 0.2, 0.4)

	expect_no_error(format_time_resolution(times))
	expect_identical(format_time_resolution(times), "0.2 seconds")
})

test_that("extent and timesteps tolerate NULL", {
	expect_null(format_time_extent(NULL))
	expect_null(count_timesteps(NULL))
})

test_that("protocol_read() reads a valid protocol CSV", {
	protocol_path <- test_path("fixtures", "protocol_example.csv")
	protocol <- protocol_read(protocol_path)

	expect_s3_class(protocol, "data.frame")
	expect_named(protocol, names(protocol))
	expect_equal(protocol$section, protocol$section)
	expect_equal(protocol$element_id, protocol$element_id)
	expect_equal(protocol$value, protocol$value)
})

test_that("protocol_read() errors if protocol_path is not a single string", {
	expect_error(
		protocol_read(1),
		"`protocol_path` must be a single character string.",
		fixed = TRUE
	)

	expect_error(
		protocol_read(c("a.csv", "b.csv")),
		"`protocol_path` must be a single character string.",
		fixed = TRUE
	)
})

test_that("protocol_read() errors if file does not exist", {
	path <- tempfile(fileext = ".csv")

	expect_false(file.exists(path))

	expect_error(
		protocol_read(path),
		"File does not exist:",
		fixed = TRUE
	)
})

test_that("protocol_read() errors for non-csv files", {
	path <- tempfile(fileext = ".txt")
	writeLines("not,a,protocol", path)

	expect_error(
		protocol_read(path),
		"Only CSV protocol files are currently supported.",
		fixed = TRUE
	)
})

test_that("protocol_read() errors if required columns are missing", {
	path <- tempfile(fileext = ".csv")

	bad_protocol <- data.frame(
		section = "Overview",
		element = "Objective",
		value = "Model only"
	)

	utils::write.csv(bad_protocol, path, row.names = FALSE)

	expect_error(
		protocol_read(path),
		"The protocol file is missing required columns:",
		fixed = TRUE
	)

	expect_error(
		protocol_read(path),
		"subsection",
		fixed = TRUE
	)

	expect_error(
		protocol_read(path),
		"element_id",
		fixed = TRUE
	)
})

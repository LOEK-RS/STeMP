# Tests for the analyze_protocol R function
test_that("analyze_protocol works with Model only", {
	protocol_path <- test_path("fixtures", "protocol_example.csv")
	protocol <- readr::read_csv(protocol_path) |>
		dplyr::filter(section != "Prediction")

	warnings <- analyze_protocol(protocol, render = FALSE)$warnings_text

	expect_equal(length(warnings), 2)
	expect_equal(
		grep(
			"The samples were clustered relative to the prediction area, which often leads to extrapolation",
			warnings,
			fixed = TRUE
		),
		2
	)
})


test_that("analyze_protocol works with model and prediction", {
	protocol_path <- test_path("fixtures", "protocol_example.csv")
	protocol <- readr::read_csv(protocol_path)

	warnings <- analyze_protocol(protocol, render = FALSE)$warnings_text

	expect_equal(length(warnings), 2)
	expect_equal(
		grep(
			"The samples were clustered relative to the prediction area, which often leads to extrapolation",
			warnings,
			fixed = TRUE
		),
		2
	)
})

test_that("analyze_protocol renders a datatable when render = TRUE", {
	protocol_path <- test_path("fixtures", "protocol_example.csv")
	protocol <- readr::read_csv(protocol_path)

	warnings <- analyze_protocol(protocol, render = TRUE)

	expect_true("rendered_table" %in% names(warnings))
	expect_s3_class(warnings$rendered_table, "datatables")
})

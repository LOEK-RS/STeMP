fixture_protocol <- function() {
	protocol_read(testthat::test_path("fixtures", "protocol_example.csv"))
}

set_protocol_value <- function(protocol, element_id, value) {
	protocol$value[protocol$element_id == element_id] <- value
	protocol
}

test_that("protocol_analyze() errors if input is not a data frame", {
	expect_error(
		protocol_analyze(list()),
		"input must be a data.frame",
		fixed = TRUE
	)

	expect_error(
		protocol_analyze("not a protocol"),
		"input must be a data.frame",
		fixed = TRUE
	)
})

test_that("protocol_analyze() works with protocol_read() output", {
	protocol <- fixture_protocol()

	expect_s3_class(protocol, "data.frame")

	out <- protocol_analyze(protocol, render = FALSE)

	expect_s3_class(out, "stemp_analysis")
	expect_type(out, "list")
	expect_named(out, "warnings_text")
	expect_false("rendered_table" %in% names(out))
})

test_that("protocol_analyze() detects expected warnings in fixture protocol", {
	protocol <- fixture_protocol()

	out <- protocol_analyze(protocol, render = FALSE)

	expect_s3_class(out, "stemp_analysis")

	expect_length(out$warnings_text, 2)

	expect_true(any(grepl("overly optimistic", out$warnings_text, fixed = TRUE)))
	expect_true(any(grepl("No methods to identify and communicate extrapolation areas", out$warnings_text, fixed = TRUE)))
	expect_true(any(grepl("No methods to identify and communicate extrapolation areas", out$warnings_text, fixed = TRUE)))
})

test_that("protocol_analyze() detects cross-validation used for both model selection and final prediction assessment", {
	protocol <- fixture_protocol()
	protocol <- set_protocol_value(protocol, "evaluation_strategy", "Random Cross-Validation")

	out <- protocol_analyze(protocol, render = FALSE)

	expect_s3_class(out, "stemp_analysis")
	expect_true(any(grepl("Cross-Validation was used for both", out$warnings_text, fixed = TRUE)))
	expect_true(any(grepl("data leakage", out$warnings_text, fixed = TRUE)))
})

test_that("protocol_analyze() detects random sampling design with spatial validation and evaluation", {
	protocol <- fixture_protocol()
	protocol <- set_protocol_value(protocol, "sampling_design", "random")
	protocol <- set_protocol_value(protocol, "validation_strategy", "Spatial Cross-Validation")
	protocol <- set_protocol_value(protocol, "evaluation_strategy", "Spatial Cross-Validation")

	out <- protocol_analyze(protocol, render = FALSE)

	expect_s3_class(out, "stemp_analysis")

	expect_true(any(grepl("Cross-Validation was used for both", out$warnings_text, fixed = TRUE)))
	expect_true(any(grepl("overly pessimistic", out$warnings_text, fixed = TRUE)))
})

test_that("protocol_analyze() detects clustered samples with spatial proxies", {
	protocol <- fixture_protocol()
	protocol <- set_protocol_value(protocol, "predictor_types", "Remote Sensing Images, Spatial Proxies")

	out <- protocol_analyze(protocol, render = FALSE)

	expect_s3_class(out, "stemp_analysis")
	expect_true(any(grepl("Using spatial proxies with clustered samples", out$warnings_text, fixed = TRUE)))
})

test_that("protocol_analyze() returns rendered DT table when render is TRUE", {
	skip_if_not_installed("DT")

	protocol <- fixture_protocol()

	out <- protocol_analyze(protocol, render = TRUE)

	expect_s3_class(out, "stemp_analysis")
	expect_named(out, c("warnings_text", "rendered_table"))
	expect_s3_class(out$rendered_table, "datatables")
	expect_s3_class(out$rendered_table, "htmlwidget")
})

report_dict <- function() {
	data.frame(
		section = c("Prediction", "Prediction"),
		subsection = c("Prediction domain", "Prediction domain"),
		element = c("Prediction map", "Prediction spatial resolution"),
		element_id = c("prediction_area", "prediction_resolution"),
		element_type = c("prediction_area_plot", "text"),
		optional = c(0, 0),
		stringsAsFactors = FALSE
	)
}

report_values <- function() {
	data.frame(
		section = "Prediction",
		element_id = "prediction_resolution",
		value = "1 km",
		stringsAsFactors = FALSE
	)
}

test_that("build_report_structure carries the widget path through", {
	figures <- data.frame(
		element_id = "prediction_area",
		path = "/tmp/prediction_area.png",
		caption = "Prediction map",
		widget = "/tmp/prediction_area.rds",
		stringsAsFactors = FALSE
	)

	items <- build_report_structure(report_values(), report_dict(), figures)

	expect_true("widget" %in% names(items))
	expect_equal(items$widget[items$element_id == "prediction_area"], "/tmp/prediction_area.rds")
	# merge() leaves NA on the field rows; Quarto's YAML serialiser rejects it
	expect_false(anyNA(items$widget))
	expect_equal(items$widget[items$element_id == "prediction_resolution"], "")
})

test_that("build_report_structure tolerates a figures_df without a widget column", {
	figures <- data.frame(
		element_id = "prediction_area",
		path = "/tmp/prediction_area.png",
		caption = "Prediction map",
		stringsAsFactors = FALSE
	)

	items <- build_report_structure(report_values(), report_dict(), figures)

	expect_true("widget" %in% names(items))
	expect_equal(items$widget[items$element_id == "prediction_area"], "")
})

test_that("build_report_structure returns the report columns for an empty protocol", {
	figures <- data.frame(
		element_id = character(0),
		path = character(0),
		caption = character(0),
		widget = character(0),
		stringsAsFactors = FALSE
	)

	empty <- report_values()[0, , drop = FALSE]
	items <- build_report_structure(empty, report_dict(), figures)

	expect_equal(nrow(items), 0L)
	expect_true(all(c("path", "caption", "widget") %in% names(items)))
})

test_that("the template inlines a widget rather than linking a PNG", {
	skip_on_cran()
	skip_if_not_installed("quarto")
	skip_if_not_installed("leaflet")
	skip_if_not_installed("knitr")

	qmd_source <- system.file("app/www/protocol_template.qmd", package = "STeMP")
	skip_if(qmd_source == "", "protocol_template.qmd not found in installed package")

	dir <- withr::local_tempdir()
	file.copy(qmd_source, file.path(dir, "protocol_template.qmd"))
	file.copy(system.file("app/www/stemp_report.scss", package = "STeMP"), file.path(dir, "stemp_report.scss"))

	widget_path <- save_widget(leaflet::leaflet(), "prediction_area", dir)

	items <- data.frame(
		section = "Prediction",
		subsection = "Prediction domain",
		element = "Prediction map",
		element_id = "prediction_area",
		kind = "figure",
		value = "",
		path = "prediction_area.png",
		caption = "Prediction map",
		widget = normalizePath(widget_path, winslash = "/"),
		show_subsection = TRUE,
		stringsAsFactors = FALSE
	)

	out <- tryCatch(
		quarto::quarto_render(
			input = file.path(dir, "protocol_template.qmd"),
			output_file = "protocol.html",
			execute_params = list(layout = "sections", embed = "data-uri", structure = items),
			execute_dir = dir,
			quiet = TRUE
		),
		error = function(e) e
	)

	expect_false(
		inherits(out, "error"),
		info = if (inherits(out, "error")) paste("render failed:", conditionMessage(out))
	)

	html <- paste(readLines(file.path(dir, "protocol.html"), warn = FALSE), collapse = "\n")

	# The widget is inlined, so knitr can deduplicate leaflet's JS and CSS
	expect_true(grepl("leaflet", html, ignore.case = TRUE))
	expect_false(grepl("data:text/html;base64", html, fixed = TRUE))
	expect_false(grepl("<img src='prediction_area.png'", html, fixed = TRUE))
})

test_that("embed = 'none' keeps the PNG and annotates the caption", {
	skip_on_cran()
	skip_if_not_installed("quarto")
	skip_if_not_installed("leaflet")

	qmd_source <- system.file("app/www/protocol_template.qmd", package = "STeMP")
	skip_if(qmd_source == "", "protocol_template.qmd not found in installed package")

	dir <- withr::local_tempdir()
	file.copy(qmd_source, file.path(dir, "protocol_template.qmd"))
	file.copy(system.file("app/www/stemp_report.scss", package = "STeMP"), file.path(dir, "stemp_report.scss"))

	widget_path <- save_widget(leaflet::leaflet(), "prediction_area", dir)

	items <- data.frame(
		section = "Prediction",
		subsection = "Prediction domain",
		element = "Prediction map",
		element_id = "prediction_area",
		kind = "figure",
		value = "",
		path = "prediction_area.png",
		caption = "Prediction map",
		widget = normalizePath(widget_path, winslash = "/"),
		show_subsection = TRUE,
		stringsAsFactors = FALSE
	)

	quarto::quarto_render(
		input = file.path(dir, "protocol_template.qmd"),
		output_file = "protocol.html",
		execute_params = list(layout = "sections", embed = "none", structure = items),
		execute_dir = dir,
		quiet = TRUE
	)

	html <- paste(readLines(file.path(dir, "protocol.html"), warn = FALSE), collapse = "\n")

	# The PDF is rendered from this variant, so it must stay static
	expect_true(grepl("prediction_area.png", html, fixed = TRUE))
	expect_true(grepl("an interactive version is included", html, fixed = TRUE))
})

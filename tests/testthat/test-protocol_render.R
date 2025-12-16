test_that("protocol template renders to HTML", {
	skip_if_not_installed("quarto")
	skip_if_not_installed("stringr")
	skip_if_not_installed("knitr")

	# Minimal example data frame for testing
	test_df <- data.frame(
		section = "Test Section",
		subsection = "Sub",
		element = "Element",
		value = "Some test value",
		stringsAsFactors = FALSE
	)

	# Optional test figures (empty for now)
	test_plots <- character(0)

	# Create a writable temporary directory
	tmpdir <- tempdir()

	# Copy the QMD template from the installed package to tempdir
	qmd_source <- system.file("app/www/protocol_template.qmd", package = "STeMP")
	skip_if(qmd_source == "", "protocol_template.qmd not found in installed package")

	qmd_file <- file.path(tmpdir, "protocol_template.qmd")
	file.copy(qmd_source, qmd_file, overwrite = TRUE)

	# Name of the output HTML file (filename only!)
	html_filename <- "protocol.html"

	# Render HTML
	out <- tryCatch(
		{
			quarto::quarto_render(
				input = qmd_file,
				output_file = html_filename,
				execute_params = list(
					data = test_df,
					plot_files = test_plots
				),
				execute_dir = tmpdir,
				quiet = TRUE
			)
		},
		error = function(e) e
	)

	# Check that render did not produce an error
	expect_false(
		inherits(out, "error"),
		info = if (inherits(out, "error")) paste("HTML render failed:", out$message)
	)

	# Full path to HTML file
	html_path <- file.path(tmpdir, html_filename)

	# Check that the HTML file was created
	expect_true(
		file.exists(html_path),
		info = "HTML file was not created"
	)

	# Optional: check that the HTML contains <html> and <body> tags
	html_content <- readLines(html_path, warn = FALSE)
	expect_true(
		any(grepl("<html", html_content, ignore.case = TRUE)),
		info = "Rendered HTML file does not contain <html> tag"
	)
	expect_true(
		any(grepl("<body", html_content, ignore.case = TRUE)),
		info = "Rendered HTML file does not contain <body> tag"
	)
})

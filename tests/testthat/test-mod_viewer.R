test_that("mod_viewer_server generates HTML preview correctly", {
	skip_if_not_installed("quarto")
	skip_if_not_installed("stringr")
	skip_if_not_installed("knitr")
	skip_if_not_installed("kableExtra")

	# Minimal test data
	test_df <- data.frame(
		section = "Test Section",
		subsection = "Sub",
		element = "Element",
		element_id = "el_1",
		value = "Some test value",
		stringsAsFactors = FALSE
	)

	# Temporary directory for output
	tmpdir <- tempdir()

	# Copy QMD template to tempdir
	qmd_source <- system.file("app/www/protocol_template.qmd", package = "STeMP")
	skip_if(qmd_source == "", "protocol_template.qmd not found in installed package")
	file.copy(qmd_source, file.path(tmpdir, "protocol_template.qmd"), overwrite = TRUE)

	# Dummy generate_html function
	generate_html <- function() {
		html_file <- file.path(tmpdir, "protocol_preview_test.html")
		writeLines("<html><body>Test</body></html>", html_file)
		html_file
	}

	shiny::testServer(
		mod_viewer_server,
		args = list(
			generate_html = generate_html,
			temp_dir = tmpdir
		),
		{
			# Simulate button click to generate HTML
			session$setInputs(preview_html = 1)

			# Access the reactiveVal returned by the module
			html_file <- html_preview_path()
			expect_true(file.exists(html_file), info = "HTML preview file was not created")
		}
	)
})

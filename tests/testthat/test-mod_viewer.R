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

	# Dummy functions for plot selection
	get_allowed_element_ids <- function(x) "dummy"
	get_selected_plot_files <- function(output_dir, allowed_ids, copy_subdir, return_relative) character(0)

	# Run testServer
	shiny::testServer(
		mod_viewer_server,
		args = list(
			protocol_data = reactive(test_df),
			o_objective_1_val = reactive("dummy"),
			output_dir = tmpdir
		),
		{
			# Simulate button click
			session$setInputs(preview_html = 1)

			# The reactive html_preview_path should now be set
			html_file <- html_preview_path()
			html_path <- file.path(tmpdir, html_file)
			expect_true(file.exists(html_path), info = "HTML preview file was not created")

			# Check that iframe would point to the created file
			ui <- output$html_preview_ui

			expect_true(
				length(ui) > 0,
				info = "html_preview_ui produced no output"
			)

			ui_text <- paste(as.character(ui), collapse = " ")

			expect_true(
				grepl(html_file, ui_text),
				info = "Preview HTML filename not found in rendered UI"
			)
		}
	)
})

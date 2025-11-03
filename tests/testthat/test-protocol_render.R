test_that("protocol template renders to PDF", {
	skip_if_not_installed("rmarkdown")
	skip_if_not_installed("tinytex")

	# Path to the Rmd in the installed package
	rmd_path <- system.file("app/www/protocol_template.Rmd", package = "STeMP")
	skip_if(rmd_path == "", "protocol_template.Rmd not found in installed package")

	tmpdir <- tempdir()
	pdf_file <- file.path(tmpdir, "protocol.pdf")

	# Render the PDF in a fresh environment
	out <- tryCatch(
		rmarkdown::render(
			input = rmd_path,
			output_file = pdf_file,
			output_format = rmarkdown::pdf_document(
				toc = FALSE,
				number_sections = FALSE,
				latex_engine = "xelatex"
			),
			output_dir = tmpdir,
			envir = new.env(parent = globalenv()),
			quiet = TRUE,
			clean = TRUE
		),
		error = function(e) e
	)

	# Fail if rendering produced an R error (including LaTeX compilation errors)
	expect_false(
		inherits(out, "error"),
		info = if (inherits(out, "error")) paste("PDF render failed:", out$message)
	)

	# Check that the PDF file was created
	expect_true(
		file.exists(pdf_file),
		info = "PDF file was not created"
	)
})

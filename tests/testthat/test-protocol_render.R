test_that("protocol template renders to PDF", {
	skip_if_not_installed("rmarkdown")
	skip_if_not_installed("tinytex")

	rmd_path <- system.file("app/www/protocol_template.Rmd", package = "STeMP")
	skip_if(rmd_path == "", "protocol_template.Rmd not found in installed package")

	tmpdir <- tempdir()
	out <- tryCatch(
		{
			rmarkdown::render(
				input = rmd_path,
				output_dir = tmpdir,
				quiet = TRUE
			)
		},
		error = function(e) e
	)

	expect_false(
		inherits(out, "error"),
		info = if (inherits(out, "error")) paste("Render error:", out$message) else NULL
	)
})

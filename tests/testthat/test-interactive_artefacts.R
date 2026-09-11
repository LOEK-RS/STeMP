test_that("save_widget stores the widget object and clear_widget removes it", {
	skip_if_not_installed("leaflet")

	dir <- withr::local_tempdir()
	path <- save_widget(leaflet::leaflet(), "test_map", dir)

	expect_equal(path, file.path(dir, "test_map.rds"))
	expect_true(file.exists(path))
	expect_s3_class(readRDS(path), "leaflet")

	clear_widget("test_map", dir)
	expect_false(file.exists(path))
})

test_that("save_widget ignores a NULL widget", {
	dir <- withr::local_tempdir()

	expect_null(save_widget(NULL, "test_map", dir))
	expect_length(list.files(dir), 0L)
})

test_that("export_widget_html writes one self-contained file for the ZIP", {
	skip_if_not_installed("leaflet")
	skip_if_not(rmarkdown::pandoc_available())

	dir <- withr::local_tempdir()
	save_widget(leaflet::leaflet(), "test_map", dir)

	path <- export_widget_html("test_map", dir)

	expect_equal(path, file.path(dir, "test_map.html"))
	expect_true(file.exists(path))
	# Dependencies inlined, not left beside the file
	expect_false(dir.exists(file.path(dir, "lib")))
	expect_true(any(grepl("leaflet", readLines(path, warn = FALSE), ignore.case = TRUE)))
})

test_that("export_widget_html returns NULL when no widget was stored", {
	expect_null(export_widget_html("test_map", withr::local_tempdir()))
})

test_that("is_interactive_mode only accepts the exact mode string", {
	expect_true(is_interactive_mode("Interactive"))
	expect_false(is_interactive_mode("Static"))
	expect_false(is_interactive_mode(NULL))
	expect_false(is_interactive_mode(TRUE))
})

test_that("the PNG is written in both modes, the widget only in interactive", {
	skip_if_not_installed("leaflet")
	local_ne_cache()

	area <- geo_bbox_poly(5, 50, 12, 55)

	for (interactive in c(FALSE, TRUE)) {
		dir <- withr::local_tempdir()

		shiny::testServer(
			function(input, output, session) {
				geo_map(
					output = output,
					element_id = "training_area",
					geo_metadata = list(training_area_sf = shiny::reactive(area)),
					what = "training_area_sf",
					output_dir = dir,
					interactive = interactive
				)
			},
			{
				output[["training_area_plot_ui"]] # force the renderer

				# The static artefact is the contract with PDF, ZIP and re-upload
				expect_true(file.exists(file.path(dir, "training_area.png")))
				expect_equal(file.exists(file.path(dir, "training_area.rds")), interactive)
			}
		)
	}
})

test_that("emit_figure clears both artefacts when there is nothing to draw", {
	dir <- withr::local_tempdir()
	writeLines("stale", file.path(dir, "training_area.png"))
	writeLines("stale", file.path(dir, "training_area.rds"))

	shiny::testServer(
		function(input, output, session) {
			emit_figure(
				output = output,
				element_id = "training_area",
				output_dir = dir,
				ns = identity,
				interactive = TRUE,
				build = function() NULL
			)
		},
		{
			output[["training_area_plot_ui"]]

			expect_false(file.exists(file.path(dir, "training_area.png")))
			expect_false(file.exists(file.path(dir, "training_area.rds")))
		}
	)
})

test_that("an unavailable widget leaves the static figure in place", {
	local_ne_cache()

	dir <- withr::local_tempdir()

	shiny::testServer(
		function(input, output, session) {
			emit_figure(
				output = output,
				element_id = "training_area",
				output_dir = dir,
				ns = identity,
				interactive = TRUE,
				# What a refusal in geo_map_leaflet() looks like to the caller
				build = function() {
					list(static = geo_map_plot(geo_bbox_poly(5, 50, 12, 55)), widget = NULL)
				}
			)
		},
		{
			output[["training_area_plot_ui"]]

			expect_true(file.exists(file.path(dir, "training_area.png")))
			expect_false(file.exists(file.path(dir, "training_area.rds")))
		}
	)
})

test_that("to_leaflet_crs reprojects, and refuses data it cannot place", {
	x <- geo_points(c(7, 9), c(51, 52), crs = 25832)
	expect_equal(sf::st_crs(to_leaflet_crs(x)), sf::st_crs(4326))

	expect_null(to_leaflet_crs(sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 0))))))
	expect_null(to_leaflet_crs(geo_empty_points()))
	expect_null(to_leaflet_crs(data.frame(a = 1)))
})

test_that("popup_table escapes values and reports one popup per feature", {
	x <- geo_points(c(7, 9), c(51, 52), label = c("a < b", "plain"))

	popups <- popup_table(x)

	expect_length(popups, 2L)
	expect_match(popups[1], "a &lt; b", fixed = TRUE)
	expect_false(any(grepl("a < b", popups, fixed = TRUE)))
})

test_that("popup_table returns NULL when there are no attributes", {
	expect_null(popup_table(sf::st_sf(geometry = sf::st_geometry(geo_points(7, 51)))))
})

test_that("geo_map_leaflet builds a map and deduplicates point geometries", {
	skip_if_not_installed("leaflet")
	local_ne_cache()

	x <- geo_points(c(7, 7, 9), c(51, 51, 52))
	m <- geo_map_leaflet(x)

	expect_s3_class(m, "leaflet")
	expect_true("addCircleMarkers" %in% leaflet_methods(m))
})

test_that("interactive builders refuse layers too large for the browser", {
	skip_if_not_installed("leaflet")
	local_ne_cache()
	testthat::local_mocked_bindings(interactive_max_n = function() 2L)

	x <- geo_points(seq(7, 10, length.out = 5), seq(51, 54, length.out = 5))

	expect_null(geo_map_leaflet(x))
	expect_null(sample_locations_leaflet(x))
})

test_that("sample_locations_leaflet reports repeats and their timestamps", {
	skip_if_not_installed("leaflet")
	local_ne_cache()

	m <- sample_locations_leaflet(interactive_samples(), temporal = TRUE)
	strings <- widget_strings(m)

	expect_s3_class(m, "leaflet")
	expect_true(any(grepl("<b>Observations:</b> 2", strings, fixed = TRUE)))
	expect_true(any(grepl("<b>Time steps:</b> 2", strings, fixed = TRUE)))
})

test_that("sample_locations_leaflet omits timestamps in spatial mode", {
	skip_if_not_installed("leaflet")
	local_ne_cache()

	strings <- widget_strings(sample_locations_leaflet(interactive_samples(), temporal = FALSE))

	expect_true(any(grepl("<b>Observations:</b> 2", strings, fixed = TRUE)))
	expect_false(any(grepl("Time steps", strings, fixed = TRUE)))
})

test_that("prediction_domain_leaflet collapses time steps onto one feature", {
	skip_if_not_installed("leaflet")
	local_ne_cache()

	# Same polygon repeated once per time step: the facet grid of the static
	# figure must become a single panel carrying the time as an attribute
	m <- prediction_domain_leaflet(geo_timestep_area(5))
	strings <- widget_strings(m)

	expect_s3_class(m, "leaflet")
	expect_equal(grep("time steps$", strings, value = TRUE), "5 time steps")
	expect_true(any(grepl("<b>Time steps:</b> 5", strings, fixed = TRUE)))
	expect_true(any(grepl("<b>Resolution:</b>", strings, fixed = TRUE)))
})

test_that("prediction_domain_leaflet truncates a long list of stamps", {
	skip_if_not_installed("leaflet")
	local_ne_cache()

	strings <- widget_strings(prediction_domain_leaflet(geo_timestep_area(30), max_listed = 5))

	expect_true(any(grepl("and 25 more", strings, fixed = TRUE)))
})

test_that("prediction_domain_leaflet falls back to attributes without a time column", {
	skip_if_not_installed("leaflet")
	local_ne_cache()

	area <- geo_bbox_poly(5, 50, 12, 55)
	area$note <- "domain"

	strings <- widget_strings(prediction_domain_leaflet(area))

	expect_true(any(grepl("domain", strings, fixed = TRUE)))
	expect_false(any(grepl("Time steps", strings, fixed = TRUE)))
})

test_that("sample_repetitions_widget drops the frequency panel without time", {
	skip_if_not_installed("leaflet")
	local_ne_cache()

	x <- geo_points(c(7, 7, 9), c(51, 51, 52))

	expect_s3_class(sample_repetitions_widget(x, temporal = TRUE), "leaflet")
})

test_that("geodist_widget degrades to a single panel", {
	skip_if_not_installed("plotly")

	p <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) + ggplot2::geom_point()

	expect_s3_class(geodist_widget(p, NULL), "plotly")
	expect_s3_class(geodist_widget(NULL, p), "plotly")
	expect_s3_class(geodist_widget(p, p), "plotly")
})

# --- scale helpers ---------------------------------------------------------

test_that("interior_breaks places every break strictly inside the range", {
	expect_equal(interior_breaks(3, pad = 0.15)(c(0, 10)), c(1.5, 5, 8.5))
	expect_length(interior_breaks(2)(c(-5, 5)), 2)
	# Order of the limits must not matter
	expect_equal(interior_breaks(3)(c(10, 0)), interior_breaks(3)(c(0, 10)))
})

test_that("interior_breaks yields nothing for a degenerate range", {
	expect_length(interior_breaks()(c(5, 5)), 0)
	expect_length(suppressWarnings(interior_breaks()(c(NA_real_, NA_real_))), 0)
	expect_length(interior_breaks()(c(-Inf, Inf)), 0)
})

test_that("map_axis_labels scales its precision to the extent", {
	expect_equal(map_axis_labels(c(-120, 120)), c("-120", "120"))
	expect_equal(map_axis_labels(c(7.6001, 7.6009)), c("7.6001", "7.6009"))
	expect_equal(map_axis_labels(c(5, 5)), c("5", "5"))
})

test_that("map_axis_labels returns one label per break, blank for NA", {
	expect_equal(map_axis_labels(c(1, NA, 2)), c("1", "", "2"))
	expect_equal(map_axis_labels(c(NA_real_, NA_real_)), c("", ""))
})

test_that("integer_breaks returns whole numbers from one upward", {
	brk <- integer_breaks()(c(0, 5))
	expect_true(all(brk == as.integer(brk)))
	expect_true(all(brk >= 1))
})

# --- map_panel_aspect ------------------------------------------------------

test_that("map_panel_aspect returns height over width, clamped", {
	expect_equal(map_panel_aspect(geo_points(c(0, 100), c(0, 100), crs = 3857)), 1)
	expect_equal(map_panel_aspect(geo_points(c(0, 100), c(0, 120), crs = 3857)), 1.2)

	wide <- geo_points(c(0, 1000), c(0, 10), crs = 3857)
	expect_equal(map_panel_aspect(wide), 0.45)
	expect_equal(map_panel_aspect(wide, limits = c(0.1, 2)), 0.1)
	expect_equal(map_panel_aspect(geo_points(c(0, 10), c(0, 1000), crs = 3857)), 1.6)
})

test_that("map_panel_aspect corrects longitude for latitude", {
	# At 60 N coord_sf() halves the effective longitude span, so a 10 x 5
	# degree extent renders square.
	expect_equal(map_panel_aspect(geo_points(c(0, 10), c(57.5, 62.5))), 1, tolerance = 1e-3)
})

test_that("map_panel_aspect returns NULL for a degenerate extent", {
	expect_null(map_panel_aspect(NULL))
	expect_null(map_panel_aspect(geo_points(7.6, 51.9)))
})

# --- map_axis_style --------------------------------------------------------

test_that("map_axis_style returns coord, two scales and theming", {
	s <- map_axis_style(1)

	expect_length(s, 5)
	expect_s3_class(s[[1]], "CoordSf")
	expect_s3_class(s[[2]], "Scale")
	# Narrower panels get fewer breaks
	expect_length(s[[2]]$breaks(c(0, 10)), 3)
	expect_length(map_axis_style(3)[[2]]$breaks(c(0, 10)), 2)
	expect_equal(sf::st_crs(map_axis_style(1, datum = sf::st_crs(3857))[[1]]$datum), sf::st_crs(3857))
})

test_that("map_axis_style pins the panel only when limits are given", {
	free <- map_axis_style(1)[[1]]
	expect_null(free$limits$x)
	expect_true(free$expand)

	lims <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 10, ymax = 20), crs = sf::st_crs(4326))
	pinned <- map_axis_style(1, limits = lims)[[1]]
	expect_equal(pinned$limits$x, c(0, 10))
	expect_equal(pinned$limits$y, c(0, 20))
	# Padding is already in the bbox, so the scales must not add more
	expect_false(pinned$expand)
})

test_that("map_axis_theme angles the x labels and keeps room on the left", {
	th <- map_axis_theme()
	expect_equal(th$axis.text.x$angle, 45)
	expect_equal(as.numeric(th$plot.margin)[4], 12)
})

# --- geo_map_plot ----------------------------------------------------------

test_that("geo_map_plot builds a map for points and for polygons", {
	skip_if_no_naturalearth()

	expect_no_error(ggplot2::ggplot_build(geo_map_plot(geo_regional_points())))
	expect_no_error(ggplot2::ggplot_build(geo_map_plot(geo_global_points())))

	poly <- geo_map_plot(geo_bbox_poly(5, 50, 12, 55))
	expect_no_error(ggplot2::ggplot_build(poly))
	fills <- vapply(poly$layers, function(l) l$aes_params$fill %||% NA_character_, character(1))
	# The translucent overlay, not a solid default grey over the context
	expect_true(ggplot2::alpha("#d35400", 0.15) %in% fills)
})

test_that("geo_map_plot draws the data exactly once, deduplicated", {
	# Regression guard: a leftover plain geom_sf() under sf_overlay() painted
	# the data twice and hid the context behind a solid fill.
	skip_if_no_naturalearth()

	repeated <- geo_points(rep(c(7, 9, 11), each = 4), rep(c(51, 52, 53), each = 4))
	p <- geo_map_plot(repeated)
	last <- p$layers[[length(p$layers)]]

	expect_equal(length(p$layers), length(context_layer(repeated)) + 1L)
	expect_equal(last$aes_params$shape, 21)
	expect_equal(nrow(last$data), 3)
})

test_that("geo_map_plot pins the panel only when a context was drawn", {
	skip_if_no_naturalearth()

	expect_false(is.null(geo_map_plot(geo_regional_points())$coordinates$limits$x))

	# Too small for context, and no CRS at all: the frame stays free, as it
	# was before the backdrop existed.
	for (x in list(geo_local_points(), geo_points(c(1, 2, 3), c(1, 2, 3), crs = NA))) {
		p <- geo_map_plot(x)
		expect_null(p$coordinates$limits$x)
		expect_no_error(ggplot2::ggplot_build(p))
	}
})

# --- location_repetition_plot ---------------------------------------------

test_that("location_repetition_plot picks the scale from the count distribution", {
	skip_if_no_naturalearth()

	cases <- list(
		list(label = "few classes", n = c(1, 1, 2, 3), discrete = TRUE, trans = NULL),
		list(label = "skewed", n = c(1:8, 100, 500), discrete = FALSE, trans = "log-10"),
		list(label = "moderate", n = 1:10, discrete = FALSE, trans = "identity")
	)

	for (case in cases) {
		sc <- suppressWarnings(location_repetition_plot(geo_counts(case$n)))$scales$get_scales("colour")
		expect_equal(sc$is_discrete(), case$discrete, info = case$label)
		expect_equal(sc$name, "Observations per location", info = case$label)
		if (!is.null(case$trans)) {
			# ggplot2 3.5 renamed the field but kept the old one
			expect_equal((sc$trans %||% sc$transform)$name, case$trans, info = case$label)
		}
	}
})

test_that("location_repetition_plot puts its legend below and pins like geo_map_plot", {
	skip_if_no_naturalearth()

	p <- location_repetition_plot(geo_counts(c(1, 2, 3)))
	expect_equal(p$theme$legend.position, "bottom")
	expect_false(is.null(p$coordinates$limits$x))

	single <- location_repetition_plot(geo_points(7.6, 51.9, n = 4))
	expect_null(single$coordinates$limits$x)
	expect_no_error(ggplot2::ggplot_build(single))
})

# --- time_frequency_plot ---------------------------------------------------

test_that("time_frequency_plot derives a visible bar width from the spacing", {
	tc <- function(times, n) data.frame(time = as.POSIXct(times, tz = "UTC"), n_obs = n)
	width <- function(counts) time_frequency_plot(counts)$layers[[1]]$aes_params$width

	# A POSIXct axis has one-second resolution, so geom_col()'s own default
	# would be invisible.
	expect_equal(width(tc(c("2020-01-01", "2020-01-02", "2020-01-03"), c(1, 5, 2))), 86400 * 0.8)
	# One time stamp falls back to a nominal day
	expect_equal(width(tc("2020-01-01", 3)), 86400 * 0.8)
	# Sub-second spacing is floored at one second
	sub <- data.frame(time = as.POSIXct("2020-01-01", tz = "UTC") + c(0, 0.2, 0.4), n_obs = 1:3)
	expect_equal(width(sub), 1)

	p <- time_frequency_plot(tc(c("2020-01-01", "2020-02-01"), c(2, 4)))
	expect_equal(p$labels$y, "Observations")
	expect_null(p$labels$x)
	expect_no_error(ggplot2::ggplot_build(p))
})

# --- geo_map_timesteps_plot ------------------------------------------------

test_that("geo_map_timesteps_plot builds one facet per time step", {
	# Regression guard: context_layer() was once called with no argument, so
	# the plot errored before it was ever built.
	skip_if_no_naturalearth()

	area <- geo_timestep_area(4)
	res <- geo_map_timesteps_plot(area, parse_time_column(area))

	expect_named(res, c("plot", "width", "height"))
	expect_equal(nlevels(res$plot$layers[[length(res$plot$layers)]]$data$time_step), 4)
	expect_null(res$plot$labels$subtitle)
	expect_gt(length(res$plot$layers), 1)
	expect_no_error(ggplot2::ggplot_build(res$plot))
})

test_that("geo_map_timesteps_plot thins long series and says so", {
	skip_if_no_naturalearth()

	area <- geo_timestep_area(40)
	res <- geo_map_timesteps_plot(area, parse_time_column(area), max_facets = 9)

	expect_equal(nlevels(res$plot$layers[[length(res$plot$layers)]]$data$time_step), 9)
	expect_equal(res$plot$labels$subtitle, "Showing 9 of 40 time steps")
})

test_that("geo_map_timesteps_plot keeps the canvas within bounds", {
	skip_if_no_naturalearth()

	for (n in c(1, 4, 9)) {
		area <- geo_timestep_area(n)
		res <- geo_map_timesteps_plot(area, parse_time_column(area))
		expect_equal(res$width, 7)
		expect_gt(res$height, 0)
		expect_lte(res$height, 1.25 * 7)
	}
})

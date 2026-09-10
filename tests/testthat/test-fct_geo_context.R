# --- with_planar_s2 --------------------------------------------------------

test_that("with_planar_s2 runs planar and always restores the session", {
	old <- sf::sf_use_s2()
	withr::defer(suppressMessages(sf::sf_use_s2(old)))

	suppressMessages(sf::sf_use_s2(TRUE))
	expect_false(with_planar_s2(sf::sf_use_s2()))
	expect_true(sf::sf_use_s2())

	expect_error(with_planar_s2(stop("boom")), "boom")
	expect_true(sf::sf_use_s2())

	suppressMessages(sf::sf_use_s2(FALSE))
	expect_false(with_planar_s2(sf::sf_use_s2()))
	expect_false(sf::sf_use_s2())
})

# --- pad_bbox --------------------------------------------------------------

test_that("pad_bbox grows each span by the requested fraction", {
	bb <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 10, ymax = 20), crs = sf::st_crs(4326))
	out <- pad_bbox(bb, 0.1)

	expect_s3_class(out, "bbox")
	expect_equal(as.numeric(out), c(-1, -2, 11, 22))
	expect_equal(sf::st_crs(out), sf::st_crs(4326))
})

test_that("pad_bbox clamps lon/lat but not projected coordinates", {
	ll <- pad_bbox(sf::st_bbox(c(xmin = -179, ymin = -89, xmax = 179, ymax = 89), crs = sf::st_crs(4326)), 0.5)
	expect_equal(as.numeric(ll), c(-180, -89.9, 180, 89.9))

	xy <- pad_bbox(sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 1000, ymax = 1000), crs = sf::st_crs(3857)), 1)
	expect_equal(as.numeric(xy), c(-1000, -1000, 2000, 2000))
})

# --- context_bbox_lonlat ---------------------------------------------------

test_that("context_bbox_lonlat passes lon/lat through and rejects the unusable", {
	pts <- geo_regional_points()
	expect_equal(as.numeric(context_bbox_lonlat(pts)), as.numeric(sf::st_bbox(pts)))

	expect_null(context_bbox_lonlat(NULL))
	expect_null(context_bbox_lonlat(geo_empty_points()))
})

test_that("context_bbox_lonlat densifies, so a curved edge is not understated", {
	# LAEA Europe: the northern edge of a projected rectangle bulges north, so
	# transforming the four corners alone loses several degrees of latitude.
	pts <- geo_points(c(2.6e6, 6.5e6), c(1.6e6, 5.2e6), crs = 3035)

	corners <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(pts)), 4326))
	dense <- context_bbox_lonlat(pts)

	expect_gt(dense[["ymax"]], corners[["ymax"]])
	expect_lte(dense[["xmin"]], corners[["xmin"]] + 1e-9)
	expect_gte(dense[["xmax"]], corners[["xmax"]] - 1e-9)
})

test_that("context_bbox_lonlat stays in range for projected global data", {
	bb <- context_bbox_lonlat(sf::st_transform(geo_global_points(), 3857))

	expect_true(all(is.finite(as.numeric(bb))))
	expect_gte(bb[["ymin"]], -90)
	expect_lte(bb[["ymax"]], 90)
})

# --- context_limits --------------------------------------------------------

test_that("context_limits pads the data by 5 percent and never more", {
	lims <- context_limits(geo_points(c(7, 9), c(51, 53)))
	expect_equal(as.numeric(lims), c(6.9, 50.9, 9.1, 53.1))

	# Regression guard: the crop is padded by 10 percent, the panel by 5, so a
	# context layer can never widen the frame.
	pts <- geo_global_points()
	span <- diff(as.numeric(sf::st_bbox(pts))[c(1, 3)])
	expect_lt(diff(as.numeric(context_limits(pts))[c(1, 3)]), span * 1.12)

	expect_null(context_limits(NULL))
	expect_null(context_limits("not spatial"))
})

# --- ne_context_data -------------------------------------------------------

test_that("ne_context_data returns geometry-only land and derived boundaries", {
	skip_if_no_naturalearth()
	local_ne_cache()

	ne <- ne_context_data("small")

	expect_named(ne, c("land", "lines"), ignore.order = TRUE)
	expect_gt(nrow(ne$land), 100)
	expect_equal(setdiff(names(ne$land), attr(ne$land, "sf_column")), character(0))
	expect_true(all(sf::st_geometry_type(ne$lines) %in% c("MULTILINESTRING", "LINESTRING")))

	# Validity is asserted under the semantics it was produced with. s2 would
	# reject these polygons, but nothing downstream asks it to -- and repairing
	# them spherically is the very step that merges the antimeridian splits.
	valid <- with_planar_s2(sf::st_is_valid(ne$land))
	expect_false(any(!valid | is.na(valid)))
})

test_that("ne_context_data memoises per scale and caches failure", {
	skip_if_no_naturalearth()
	local_ne_cache()

	first <- ne_context_data("small")
	expect_identical(ne_context_data("small"), first)
	expect_null(.ne_state$ne_medium)
	expect_false(identical(ne_context_data("medium"), first))

	clear_ne_cache()
	calls <- 0L
	local_mocked_bindings(
		ne_countries = function(...) {
			calls <<- calls + 1L
			stop("no data package")
		},
		.package = "rnaturalearth"
	)
	expect_null(ne_context_data("small"))
	expect_null(ne_context_data("small"))
	expect_equal(calls, 1L)
})

test_that("Natural Earth is prepared without merging antimeridian splits", {
	skip_if_no_naturalearth()
	local_ne_cache()

	# The original symptom: with s2 enabled, st_make_valid() rebuilds the two
	# halves of Fiji into one spherical polygon that draws as a band across a
	# global map at roughly -17 degrees latitude.
	expect_lt(widest_part(ne_context_data("small")$land), 200)
})

# --- context_layer: refusals ----------------------------------------------

test_that("context_layer declines input it cannot place", {
	expect_null(context_layer(NULL))
	expect_null(context_layer(data.frame(x = 1, y = 2)))
	expect_null(context_layer(geo_empty_points()))
	# No CRS: the backdrop cannot be aligned with the data
	expect_null(context_layer(geo_points(c(1, 2), c(1, 2), crs = NA)))
})

test_that("context_layer declines when there is nothing to draw", {
	skip_if_no_naturalearth()

	# Too small for Natural Earth to contribute anything
	expect_null(context_layer(geo_local_points()))
	expect_null(context_layer(geo_points(7.6, 51.9)))

	# All ocean: the crop is empty, so the map falls back to a plain panel
	expect_null(context_layer(geo_ocean_points()))

	# On land, so only min_span_deg decides
	land <- geo_points(c(7, 8), c(51, 52))
	expect_null(context_layer(land, min_span_deg = 2))
	expect_false(is.null(context_layer(land, min_span_deg = 0.5)))
})

test_that("context_layer degrades gracefully without Natural Earth", {
	skip_if_no_naturalearth()
	local_ne_failure()

	expect_null(context_layer(geo_global_points()))
})

# --- context_layer: output -------------------------------------------------

test_that("context_layer returns a land layer and a boundary layer", {
	skip_if_no_naturalearth()

	ctx <- context_layer(geo_regional_points())
	land <- as_layer(ctx[[1]])
	lines <- as_layer(ctx[[2]])

	expect_length(ctx, 2)
	# A fill border here would trace the crop rectangle
	expect_true(is.na(land$aes_params$colour))
	expect_equal(land$aes_params$fill, "#f4f4f1")
	expect_equal(lines$aes_params$colour, "#c9d1d6")
})

test_that("context_layer picks the coarse scale only for wide extents", {
	skip_if_no_naturalearth()
	local_ne_cache()

	context_layer(geo_global_points())
	expect_false(is.null(.ne_state$ne_small))
	expect_null(.ne_state$ne_medium)

	clear_ne_cache()
	context_layer(geo_regional_points())
	expect_false(is.null(.ne_state$ne_medium))
	expect_null(.ne_state$ne_small)
})

test_that("context_layer crops to the data extent in the data CRS", {
	skip_if_no_naturalearth()

	pts <- geo_regional_points()
	crop <- pad_bbox(sf::st_bbox(pts), 0.10)
	got <- sf::st_bbox(as_layer(context_layer(pts)[[1]])$data)
	expect_true(all(as.numeric(got) >= as.numeric(crop)[c(1, 2, 1, 2)] - 1e-6))
	expect_true(all(as.numeric(got) <= as.numeric(crop)[c(3, 4, 3, 4)] + 1e-6))

	proj <- sf::st_transform(pts, 3035)
	ctx <- context_layer(proj)
	skip_if(is.null(ctx))
	expect_equal(sf::st_crs(as_layer(ctx[[1]])$data), sf::st_crs(proj))
	expect_equal(sf::st_crs(as_layer(ctx[[2]])$data), sf::st_crs(proj))
})

test_that("a global extent yields a clean, finite backdrop in any CRS", {
	skip_if_no_naturalearth()

	pts <- geo_global_points()
	ctx <- context_layer(pts)
	expect_false(is.null(ctx))
	# The reported symptom: a strange horizontal line across the lower third
	expect_lt(widest_part(as_layer(ctx[[1]])$data), 200)

	p <- ggplot2::ggplot() + ctx + sf_overlay(pts) + map_axis_style(1, limits = context_limits(pts))
	expect_no_error(ggplot2::ggplot_build(p))

	# Web Mercator sends the poles to infinity, so the lon/lat crop must run first
	merc <- context_layer(sf::st_transform(pts, 3857))
	skip_if(is.null(merc))
	expect_true(all(is.finite(as.numeric(sf::st_bbox(as_layer(merc[[1]])$data)))))
})

# --- sf_overlay ------------------------------------------------------------

test_that("sf_overlay styles points as haloed markers", {
	l <- as_layer(sf_overlay(geo_regional_points()))

	expect_equal(l$aes_params$shape, 21)
	expect_equal(l$aes_params$colour, "white")
	expect_equal(l$aes_params$fill, "#d35400")
	expect_equal(as_layer(sf_overlay(geo_regional_points(), colour = "#123456"))$aes_params$fill, "#123456")
})

test_that("sf_overlay styles anything areal as a translucent fill", {
	poly <- as_layer(sf_overlay(geo_bbox_poly(0, 0, 1, 1)))
	# The point branch would set a shape; the areal branch must not
	expect_null(poly$aes_params$shape)
	expect_equal(poly$aes_params$colour, "#d35400")
	expect_equal(poly$aes_params$fill, ggplot2::alpha("#d35400", 0.15))

	mixed <- rbind(
		sf::st_sf(geometry = sf::st_geometry(geo_regional_points()[1, ])),
		sf::st_sf(geometry = sf::st_geometry(geo_bbox_poly(0, 0, 1, 1)))
	)
	expect_equal(as_layer(sf_overlay(mixed))$aes_params$fill, ggplot2::alpha("#d35400", 0.15))
})

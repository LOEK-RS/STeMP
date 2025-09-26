test_that("geodist classification correctly classfies random sampling designs", {
	poly_path <- test_path("fixtures", "polygon.gpkg")
	samples_path <- test_path("fixtures", "random_points.gpkg")

	aoi <- sf::st_read(poly_path, quiet = TRUE)
	samples <- sf::st_read(samples_path, quiet = TRUE)

	sampling_design <- calculate_geodist_classification(samples, aoi)

	expect_equal(sampling_design, "random")
})

test_that("geodist classification correctly classfies clustered sampling designs", {
	poly_path <- test_path("fixtures", "polygon.gpkg")
	samples_path <- test_path("fixtures", "clustered_points.gpkg")

	aoi <- sf::st_read(poly_path, quiet = TRUE)
	samples <- sf::st_read(samples_path, quiet = TRUE)

	sampling_design <- calculate_geodist_classification(samples, aoi)

	expect_equal(sampling_design, "clustered")
})

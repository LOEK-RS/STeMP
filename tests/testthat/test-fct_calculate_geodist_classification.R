test_that("geodist classification correctly classfies clustered sampling designs", {
	aoi <- sf::st_as_sfc("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))", crs = "epsg:25832")
	tpoints <- sf::st_as_sfc("MULTIPOINT ((1 1), (1 2), (2 2), (2 3), (1 4), (5 4))", crs = "epsg:25832") |>
		sf::st_cast("POINT")

	sampling_design <- calculate_geodist_classification(tpoints, aoi)

	expect_equal(sampling_design, "clustered")
})

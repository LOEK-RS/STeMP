test_that("count_time_repetitions counts observations per time step", {
	pts <- sf::st_as_sf(
		data.frame(
			x = c(0, 0, 1, 1, 2),
			y = c(0, 0, 1, 1, 2),
			time = as.POSIXct(
				c("2020-01-01", "2020-01-01", "2020-01-01", "2020-01-02", "2020-01-02"),
				tz = "UTC"
			)
		),
		coords = c("x", "y"),
		crs = 4326
	)

	counts <- count_time_repetitions(pts)

	expect_equal(names(counts), c("time", "n_obs"))
	expect_equal(counts$n_obs, c(3L, 2L))
	expect_s3_class(counts$time, "POSIXct")
})

test_that("time_frequency_plot has one bar per time step and no legend", {
	counts <- data.frame(
		time = as.POSIXct(c("2020-01-01", "2020-01-02"), tz = "UTC"),
		n_obs = c(3L, 2L)
	)

	p <- time_frequency_plot(counts)

	expect_s3_class(p, "ggplot")
	expect_equal(nrow(ggplot2::layer_data(p)), 2L)
	expect_null(cowplot::get_legend(p))
})

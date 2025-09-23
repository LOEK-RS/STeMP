# 1) train caret model
data(iris)
TrainData <- iris[, 1:4]
TrainClasses <- iris[, 5]

# caret
model_caret <- caret::train(
	TrainData,
	TrainClasses,
	"ranger",
	tuneLength = 10,
	trControl = caret::trainControl(method = "cv")
)
saveRDS(model_caret, test_path("fixtures", "model_caret.RDS"))


# 2) create spatial objects
poly <- sf::st_as_sfc("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))", crs = "epsg:25832")
sf::st_write(poly, test_path("fixtures", "polygon.gpkg"), delete_dsn = TRUE, quiet = TRUE)

set.seed(10)
random_points <- sf::st_sample(poly, size = 50, type = "random")
sf::st_write(random_points, test_path("fixtures", "random_points.gpkg"), delete_dsn = TRUE, quiet = TRUE)

poly_small <- sf::st_as_sfc("POLYGON ((0 0, 2 0, 2 2, 0 2, 0 0))", crs = "epsg:25832")
clustered_points <- sf::st_sample(poly_small, size = 50, type = "random")
sf::st_write(clustered_points, test_path("fixtures", "clustered_points.gpkg"), delete_dsn = TRUE, quiet = TRUE)

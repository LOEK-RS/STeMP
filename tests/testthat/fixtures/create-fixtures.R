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

# 2) train mlr3 model
task_mlr <- mlr3::as_task_classif(
	iris,
	target = "Species"
)

train_test_split <- mlr3::partition(task_mlr, ratio = 0.7)
mlr3::mlr_learners$get("classif.ranger")
model_mlr3 <- mlr3::lrn("classif.ranger", num.trees = 100, mtry = 4)
measure_rmse <- mlr3::msr("classif.ce")

model_mlr3$train(task_mlr, row_ids = train_test_split$train)
saveRDS(model_mlr3, test_path("fixtures", "model_mlr3.RDS"))

# 3) train tidymodels model
predictor_names <- setdiff(names(iris), "Species")
formula <- as.formula(paste(
	"Species",
	"~",
	paste(predictor_names, collapse = " + ")
))
recipe <- recipes::recipe(formula, data = iris)

rf_model <- parsnip::rand_forest(trees = 100, mode = "classification") |>
	parsnip::set_engine("ranger", importance = "impurity")

# Create the workflow
workflow <- workflows::workflow() |>
	workflows::add_recipe(recipe) |>
	workflows::add_model(rf_model)

# Fit the model
model_tidymodels <- parsnip::fit(workflow, data = iris)

saveRDS(model_tidymodels, test_path("fixtures", "model_tidymodels.RDS"))

# 4) create spatial objects
poly <- sf::st_as_sfc("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))", crs = "epsg:25832")
sf::st_write(poly, test_path("fixtures", "polygon.gpkg"), delete_dsn = TRUE, quiet = TRUE)

poly_extrapolation <- sf::st_as_sfc("POLYGON ((10 0, 20 0, 20 10, 10 10, 10 0))", crs = "epsg:25832")
sf::st_write(poly_extrapolation, test_path("fixtures", "polygon_extrapolation.gpkg"), delete_dsn = TRUE, quiet = TRUE)

set.seed(10)
random_points <- sf::st_sample(poly, size = 50, type = "random")
sf::st_write(random_points, test_path("fixtures", "random_points.gpkg"), delete_dsn = TRUE, quiet = TRUE)

poly_small <- sf::st_as_sfc("POLYGON ((0 0, 2 0, 2 2, 0 2, 0 0))", crs = "epsg:25832")
clustered_points <- sf::st_sample(poly_small, size = 50, type = "random")
sf::st_write(clustered_points, test_path("fixtures", "clustered_points.gpkg"), delete_dsn = TRUE, quiet = TRUE)

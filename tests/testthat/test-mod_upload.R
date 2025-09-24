# Tests for the basic functioning of the upload/delete buttons
# Not testing for the automatic updating of protocol fields based on these uploads

test_that("Protocol csv upload and delete works", {
	protocol_test_path <- test_path("fixtures", "protocol_example.csv")

	testServer(
		mod_upload_server,
		args = list(id = "upload", output_dir = tempdir()),
		{
			# simulate CSV upload
			session$setInputs(csv_upload = list(datapath = protocol_test_path, name = basename(protocol_test_path)))
			session$flushReact()

			# Check that CSV loaded
			expect_s3_class(csv_data(), "data.frame")
			expect_false(csv_deleted())

			# simulate CSV deletion
			session$setInputs(delete_csv = 1)
			session$flushReact()

			# Check deletion
			expect_null(csv_data())
			expect_true(csv_deleted())
		}
	)
})


test_that("caret model upload and delete works", {
	model_path <- test_path("fixtures", "model_caret.RDS")

	testServer(
		mod_upload_server,
		args = list(id = "upload", output_dir = tempdir()),
		{
			# Simulate file upload as fileInput would do
			session$setInputs(
				model_upload = list(
					datapath = model_path,
					name = basename(model_path)
				)
			)

			session$flushReact()
			expect_type(model_object(), "list")

			# Delete
			session$setInputs(delete_model = 1)
			session$flushReact()
			expect_null(model_object())
			expect_true(model_deleted())
		}
	)
})

test_that("mlr3 model upload and delete works", {
	model_path <- test_path("fixtures", "model_mlr3.RDS")

	testServer(
		mod_upload_server,
		args = list(id = "upload", output_dir = tempdir()),
		{
			# Simulate file upload as fileInput would do
			session$setInputs(
				model_upload = list(
					datapath = model_path,
					name = basename(model_path)
				)
			)

			session$flushReact()
			expect_s3_class(model_object(), "Learner")

			# Delete
			session$setInputs(delete_model = 1)
			session$flushReact()
			expect_null(model_object())
			expect_true(model_deleted())
		}
	)
})

test_that("Tidymodels model upload and delete works", {
	model_path <- test_path("fixtures", "model_tidymodels.RDS")

	testServer(
		mod_upload_server,
		args = list(id = "upload", output_dir = tempdir()),
		{
			# Simulate file upload as fileInput would do
			session$setInputs(
				model_upload = list(
					datapath = model_path,
					name = basename(model_path)
				)
			)

			session$flushReact()
			expect_type(model_object(), "list")

			# Delete
			session$setInputs(delete_model = 1)
			session$flushReact()
			expect_null(model_object())
			expect_true(model_deleted())
		}
	)
})

test_that("Samples gpkg upload works", {
	samples_path <- test_path("fixtures", "random_points.gpkg")

	testServer(
		mod_upload_server,
		args = list(id = "upload", output_dir = tempdir()),
		{
			session$setInputs(`samples-upload` = list(datapath = samples_path, name = basename(samples_path)))
			session$flushReact()
			obj <- samples$data()
			expect_s3_class(obj, "sf")
			expect_equal(nrow(obj), 50)
		}
	)
})

test_that("Training area gpkg upload works", {
	poly_path <- test_path("fixtures", "polygon.gpkg")

	testServer(
		mod_upload_server,
		args = list(id = "upload", output_dir = tempdir()),
		{
			session$setInputs(`training_area-upload` = list(datapath = poly_path, name = basename(poly_path)))
			session$flushReact()
			obj <- training_area$data()
			expect_s3_class(obj, "sf")
		}
	)
})

test_that("Prediction area gpkg upload works", {
	poly_path <- test_path("fixtures", "polygon.gpkg")

	testServer(
		mod_upload_server,
		args = list(id = "upload", output_dir = tempdir()),
		{
			session$setInputs(`prediction_area-upload` = list(datapath = poly_path, name = basename(poly_path)))
			session$flushReact()
			obj <- prediction_area$data()
			expect_s3_class(obj, "sf")
		}
	)
})

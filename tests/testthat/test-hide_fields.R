# Tests for sidebar module: hide optional fields toggle

test_that("Hide optional fields toggle filters out optional rows", {
	# Load protocol fixture (has `optional` column)
	csv_path <- app_sys("app/www/stemp_dict.csv")
	protocol_df <- utils::read.csv(csv_path, stringsAsFactors = FALSE)

	# Sanity checks
	expect_true("optional" %in% names(protocol_df))
	expect_true(any(protocol_df$optional == 1), info = "Fixture must contain optional rows")

	# Start the sidebar module under test
	shiny::testServer(
		mod_sidebar_server,
		args = list(
			id = "sidebar",
			protocol_data = reactive(protocol_df),
			o_objective_1_val = reactive("dummy"),
			output_dir = tempdir()
		),
		{
			# Default: hide_optional is FALSE → all rows visible
			session$flushReact()
			df_default <- isolate(filtered_protocol_data())
			expect_true(all(df_default$visible))

			# Toggle ON: hide optional fields
			session$setInputs(hide_optional = TRUE)
			session$flushReact()
			df_hidden <- isolate(filtered_protocol_data())

			# Rows with optional == 1 must have visible == FALSE
			expect_true(all(df_hidden$visible[df_hidden$optional == 1] == FALSE))
			# Rows with optional == 0 must remain visible
			expect_true(all(df_hidden$visible[df_hidden$optional == 0] == TRUE))

			# Toggle OFF again: optional fields come back
			session$setInputs(hide_optional = FALSE)
			session$flushReact()
			df_restored <- isolate(filtered_protocol_data())
			expect_true(all(df_restored$visible))
		}
	)
})

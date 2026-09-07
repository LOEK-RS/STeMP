# Tests for sidebar module: hide optional fields toggle
sidebar_args <- function(protocol_df) {
	list(
		id = "sidebar",
		protocol_data = shiny::reactive(protocol_df),
		protocol_dict = shiny::reactive(protocol_df),
		o_objective_1_val = shiny::reactive("dummy"),
		output_dir = tempdir()
	)
}

test_that("Hide optional fields toggle filters out optional rows", {
	csv_path <- app_sys("app/www/stemp_dict.csv")
	protocol_df <- utils::read.csv(csv_path, stringsAsFactors = FALSE)

	expect_true("optional" %in% names(protocol_df))
	expect_true(any(protocol_df$optional == 1), info = "Fixture must contain optional rows")

	shiny::testServer(
		mod_sidebar_server,
		args = sidebar_args(protocol_df),
		{
			excluded_ids <- non_progress_ids(protocol_df)

			# Default: hide_optional is FALSE -> everything except excluded rows
			session$flushReact()
			df_default <- shiny::isolate(filtered_protocol_data())
			expect_true(all(df_default$visible[!df_default$element_id %in% excluded_ids]))
			expect_true(all(!df_default$visible[df_default$element_id %in% excluded_ids]))

			# Toggle ON: hide optional fields
			session$setInputs(hide_optional = TRUE)
			session$flushReact()
			df_hidden <- shiny::isolate(filtered_protocol_data())

			expect_true(all(!df_hidden$visible[df_hidden$optional == 1]))
			expect_true(all(df_hidden$visible[
				df_hidden$optional == 0 &
					!df_hidden$element_id %in% excluded_ids
			]))

			# Toggle OFF again
			session$setInputs(hide_optional = FALSE)
			session$flushReact()
			df_restored <- shiny::isolate(filtered_protocol_data())
			expect_true(all(df_restored$visible[!df_restored$element_id %in% excluded_ids]))
		}
	)
})

test_that("default-selected switches do not count toward progress", {
	protocol_df <- data.frame(
		section = c("Overview", "Overview", "Model"),
		subsection = c("A", "A", "B"),
		element = c("Domain", "Title", "Algorithm"),
		element_id = c("model_domain", "title", "model_algorithm"),
		element_type = c("radio", "text", "model_algorithm"),
		optional = c(0L, 0L, 0L),
		value = c("Spatial", "", ""),
		stringsAsFactors = FALSE
	)

	shiny::testServer(
		mod_sidebar_server,
		args = sidebar_args(protocol_df),
		{
			session$flushReact()
			df <- shiny::isolate(filtered_protocol_data())

			expect_false(df$visible[df$element_id == "model_domain"])

			filled <- !is.na(df$value) & df$value != "" & df$visible
			expect_equal(sum(filled), 0)
		}
	)
})

test_that("default-selected switches do not count toward progress", {
	# A radio row arrives pre-filled because radioButtons() always selects
	# something. It must not lift the bars off zero on a fresh session.
	protocol_df <- data.frame(
		section = c("Overview", "Overview", "Model"),
		subsection = c("A", "A", "B"),
		element = c("Domain", "Title", "Algorithm"),
		element_id = c("model_domain", "title", "model_algorithm"),
		element_type = c("radio", "text", "model_algorithm"),
		optional = c(0L, 0L, 0L),
		value = c("Spatial", "", ""),
		stringsAsFactors = FALSE
	)

	shiny::testServer(
		mod_sidebar_server,
		args = list(
			id = "sidebar",
			protocol_data = shiny::reactive(protocol_df),
			protocol_dict = shiny::reactive(protocol_df),
			o_objective_1_val = shiny::reactive("dummy"),
			output_dir = tempdir()
		),
		{
			session$flushReact()
			df <- shiny::isolate(filtered_protocol_data())

			expect_false(df$visible[df$element_id == "model_domain"])

			filled <- !is.na(df$value) & df$value != "" & df$visible
			expect_equal(sum(filled), 0)
		}
	)
})

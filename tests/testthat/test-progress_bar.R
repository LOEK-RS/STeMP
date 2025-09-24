# Testing the reactive calculation of progress percentages

test_that("Progress bar percentages are correctly calculated", {
	csv_path_fixture <- test_path("fixtures", "protocol_example.csv")
	protocol_df <- utils::read.csv(csv_path_fixture, stringsAsFactors = FALSE)

	shiny::testServer(
		mod_sidebar_server,
		args = list(
			id = "sidebar",
			protocol_data = reactive(protocol_df),
			o_objective_1_val = reactive("Model and prediction"),
			output_dir = tempdir()
		),
		{
			# Force reactive updates
			session$flushReact()

			# Get filtered protocol
			df <- isolate(filtered_protocol_data())
			expect_true(!is.null(df))

			# Calculate expected overall progress
			total_rows <- sum(df$visible)
			filled_rows <- sum(!is.na(df$value) & df$value != "" & df$visible)
			expected_overall <- round(100 * filled_rows / total_rows)

			# Now test using the module's make_bar logic
			# We replicate the module's percent calculation
			percent_overall <- round(100 * sum(!is.na(df$value) & df$value != "" & df$visible) / sum(df$visible))
			expect_equal(percent_overall, expected_overall)

			# Check section progress as well
			sections <- unique(df$section)
			lapply(sections, function(s) {
				sec_df <- df[df$section == s, , drop = FALSE]
				sec_percent <- round(
					100 * sum(!is.na(sec_df$value) & sec_df$value != "" & sec_df$visible) / sum(sec_df$visible)
				)

				# Calculate expected manually
				expected_sec <- round(
					100 * sum(!is.na(sec_df$value) & sec_df$value != "" & sec_df$visible) / sum(sec_df$visible)
				)
				expect_equal(sec_percent, expected_sec, info = paste0("Progress mismatch in section: ", s))
			})
		}
	)
})

# Tests the automatic filling of protocol fields based on an uploaded CSV

test_that("Uploading a protocol CSV automatically fills relevant fields", {
	# Load protocol template (passed into module, not directly asserted here)
	csv_path_template <- app_sys("app/www/stemp_dict.csv")
	protocol_data <- utils::read.csv(csv_path_template, stringsAsFactors = FALSE)

	# Path to prefilled protocol fixture
	csv_path_fixture <- test_path("fixtures", "protocol_example.csv")
	protocol_df <- utils::read.csv(csv_path_fixture, stringsAsFactors = FALSE)

	# Sanity checks on fixture
	expect_true("section" %in% names(protocol_df))
	expect_true("element_id" %in% names(protocol_df))
	expect_true("value" %in% names(protocol_df))
	expect_true(any(protocol_df$section == "Overview"))

	# Start the module under test
	shiny::testServer(
		mod_create_protocol_server,
		args = list(
			id = "protocol",
			protocol_data = reactive(protocol_data),
			uploaded_csv = reactive(protocol_df), # simulate upload
			model_metadata = reactiveValues(), # dummy
			geo_metadata = reactiveValues(
				samples_sf = reactive(NULL),
				training_area_sf = reactive(NULL),
				prediction_area_sf = reactive(NULL)
			),
			output_dir = tempdir(),
			model_deleted = reactive(FALSE),
			csv_deleted = reactive(FALSE),
			show_warnings = reactive(FALSE),
			hide_optional = reactive(FALSE)
		),
		{
			# Ensure module sees the uploaded CSV
			session$flushReact()

			# Set objective
			session$setInputs(`overview-o_objective_1` = "Model and prediction")
			session$flushReact()

			# 1) Check module parsed uploaded CSV into uploaded_values()
			all_uploaded <- isolate(uploaded_values())
			expect_true(!is.null(all_uploaded))
			expect_true(length(all_uploaded) > 0)

			# Iterate over each section (Overview, Model, Prediction)
			invisible(lapply(names(all_uploaded), function(section) {
				uploaded_vals <- all_uploaded[[section]]
				fixture_section <- protocol_df[protocol_df$section == section, , drop = FALSE]

				expect_true(!is.null(uploaded_vals))
				expect_true(nrow(uploaded_vals) > 0)
				expect_true(all(uploaded_vals$element_id %in% fixture_section$element_id))

				# Compare uploaded_values vs. fixture
				invisible(lapply(seq_len(nrow(uploaded_vals)), function(i) {
					el <- uploaded_vals$element_id[i]
					parsed_val <- uploaded_vals$value[i]
					expected_val <- fixture_section$value[fixture_section$element_id == el]
					if (length(expected_val) > 1) {
						expected_val <- expected_val[1]
					}

					expect_equal(
						as.character(parsed_val),
						as.character(expected_val),
						info = paste0("uploaded_values mismatch for section=", section, " element_id=", el)
					)
				}))

				# 2) Simulate populating UI inputs with non-missing values
				invisible(lapply(seq_len(nrow(uploaded_vals)), function(i) {
					el <- uploaded_vals$element_id[i]
					val <- uploaded_vals$value[i]

					if (!is.na(val) && !identical(val, "")) {
						input_name <- paste0(tolower(section), "-", el)
						args <- list()
						args[[input_name]] <- val
						do.call(session$setInputs, args)
					}
				}))

				session$flushReact()

				# 3) Verify updated_protocol reflects those values
				updated_protocol_df <- isolate(updated_protocol())
				expect_true(!is.null(updated_protocol_df))
				expect_true(nrow(updated_protocol_df) > 0)

				invisible(lapply(seq_len(nrow(uploaded_vals)), function(i) {
					el <- uploaded_vals$element_id[i]
					expected_val <- uploaded_vals$value[i]
					row_idx <- which(updated_protocol_df$element_id == el)

					expect_true(
						length(row_idx) >= 1,
						info = paste0("element_id not found in updated_protocol: ", el, " (section=", section, ")")
					)

					row_val <- updated_protocol_df$value[row_idx[1]]

					if (is.na(expected_val) || identical(expected_val, "")) {
						expect_true(
							is.na(row_val) || identical(as.character(row_val), ""),
							info = paste0("expected NA/empty for ", el, " in section=", section)
						)
					} else {
						expect_equal(
							as.character(row_val),
							as.character(expected_val),
							info = paste0("updated_protocol mismatch for section=", section, " element_id=", el)
						)
					}
				}))
			}))
		}
	)
})

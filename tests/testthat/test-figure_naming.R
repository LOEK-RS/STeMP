# Testing the naming contract of protocol figures
#
# The figure pipeline relies on a single invariant:
#   stemp_dict.csv element_id == <element_id>.png == get_allowed_element_ids()
# If any of the three drifts apart, the plot still shows in the app but silently
# disappears from the HTML preview, the PDF and the figures ZIP.

plot_element_types <- c(
	"training_plot",
	"training_area_plot",
	"geodist_plot_training",
	"prediction_area_plot",
	"geodist_plot_prediction"
)

test_that("Plot element IDs match the figures allowed for download", {
	dict_path <- app_sys("app/www/stemp_dict.csv")
	skip_if_not(file.exists(dict_path), "stemp_dict.csv not found")
	dict_df <- utils::read.csv(dict_path, stringsAsFactors = FALSE)

	plot_ids <- dict_df$element_id[dict_df$element_type %in% plot_element_types]

	# Every dictionary plot ID must be downloadable under at least one objective
	expect_setequal(
		plot_ids,
		union(
			get_allowed_element_ids("Model only"),
			get_allowed_element_ids("Model and prediction")
		)
	)

	# Figure IDs must be unique, otherwise two plots collide on the same PNG.
	# (Duplicates across sections are fine elsewhere in the dictionary, because
	# inputs are namespaced per module.)
	expect_false(any(duplicated(plot_ids)))

	# Within a section, element IDs must be unique: they are Shiny input IDs and
	# are used to look up single rows in the panel modules.
	lapply(unique(dict_df$section), function(section_name) {
		section_ids <- dict_df$element_id[dict_df$section == section_name]
		duplicated_ids <- unique(section_ids[duplicated(section_ids)])

		expect_length(duplicated_ids, 0)
		if (length(duplicated_ids) > 0) {
			expect_true(
				FALSE,
				info = paste0(
					"Duplicated element_id in section ",
					section_name,
					": ",
					paste(duplicated_ids, collapse = ", ")
				)
			)
		}
	})

	# Each objective allows exactly the figures it can produce
	id_of <- function(element_type) {
		id <- dict_df$element_id[dict_df$element_type == element_type]
		expect_length(id, 1)
		id
	}

	expect_setequal(
		get_allowed_element_ids("Model only"),
		c(id_of("training_plot"), id_of("training_area_plot"), id_of("geodist_plot_training"))
	)

	expect_setequal(
		get_allowed_element_ids("Model and prediction"),
		c(
			id_of("training_plot"),
			id_of("training_area_plot"),
			id_of("prediction_area_plot"),
			id_of("geodist_plot_prediction")
		)
	)

	# The two geodistance plots are mutually exclusive
	expect_false(id_of("geodist_plot_training") %in% get_allowed_element_ids("Model and prediction"))
	expect_false(id_of("geodist_plot_prediction") %in% get_allowed_element_ids("Model only"))
})


test_that("Figures are written, found and deleted under their element ID", {
	dict_path <- app_sys("app/www/stemp_dict.csv")
	skip_if_not(file.exists(dict_path), "stemp_dict.csv not found")
	dict_df <- utils::read.csv(dict_path, stringsAsFactors = FALSE)

	plot_ids <- dict_df$element_id[dict_df$element_type %in% plot_element_types]

	output_dir <- file.path(tempdir(), "test_figure_naming")
	dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
	on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

	dummy_plot <- ggplot2::ggplot() + ggplot2::geom_blank()

	# save_figure() must not translate the ID into a different file name
	lapply(plot_ids, function(element_id) {
		plot_path <- save_figure(dummy_plot, element_id, output_dir)
		expect_equal(
			basename(plot_path),
			paste0(element_id, ".png"),
			info = paste0("Unexpected file name for: ", element_id)
		)
	})

	# The download filter must find exactly the allowed figures for each objective
	lapply(c("Model only", "Model and prediction"), function(objective) {
		allowed_ids <- get_allowed_element_ids(objective)
		selected_files <- get_selected_plot_files(
			output_dir,
			allowed_ids,
			copy_subdir = paste0("figures_", make.names(objective)),
			return_relative = FALSE
		)

		expect_setequal(sub("\\.png$", "", basename(selected_files)), allowed_ids)
	})

	# delete_plot_png() must remove what save_figure() wrote
	lapply(plot_ids, function(element_id) {
		delete_plot_png(element_id, output_dir)
		expect_false(
			file.exists(file.path(output_dir, paste0(element_id, ".png"))),
			info = paste0("Figure not deleted: ", element_id)
		)
	})
})


test_that("Hard-coded element IDs in the panel modules exist in the dictionary", {
	dict_path <- app_sys("app/www/stemp_dict.csv")
	skip_if_not(file.exists(dict_path), "stemp_dict.csv not found")
	dict_df <- utils::read.csv(dict_path, stringsAsFactors = FALSE)
	plot_ids <- dict_df$element_id[dict_df$element_type %in% plot_element_types]

	# The panel modules and the objective gate still name figures with string
	# literals. A literal that drifts from the dictionary fails silently, so it
	# is checked against the source. Once the IDs are derived from the
	# dictionary, these greps return nothing and the test skips.
	source_literals <- function(source_file, pattern) {
		source_path <- test_path("..", "..", "R", source_file)
		if (!file.exists(source_path)) {
			return(character(0))
		}
		source_lines <- readLines(source_path, warn = FALSE)
		matches <- regmatches(source_lines, gregexpr(pattern, source_lines, perl = TRUE))
		unique(gsub(pattern, "\\1", unlist(matches), perl = TRUE))
	}

	literal_ids <- unique(c(
		source_literals("mod_model_panel.R", 'element_id\\s*=\\s*"([^"]+)"'),
		source_literals("mod_prediction_panel.R", 'element_id\\s*=\\s*"([^"]+)"'),
		source_literals("fct_render_inputs.R", 'identical\\(element_id,\\s*"([^"]+)"\\)')
	))

	literal_files <- unique(c(
		source_literals("mod_model_panel.R", 'file\\s*=\\s*"([^"]+\\.png)"'),
		source_literals("mod_prediction_panel.R", 'file\\s*=\\s*"([^"]+\\.png)"')
	))

	skip_if(
		length(literal_ids) == 0 && length(literal_files) == 0,
		"No literal figure IDs left in the sources"
	)

	expect_true(
		all(literal_ids %in% plot_ids),
		info = paste0("Unknown element ID(s): ", paste(setdiff(literal_ids, plot_ids), collapse = ", "))
	)

	expect_true(
		all(sub("\\.png$", "", literal_files) %in% plot_ids),
		info = paste0(
			"Unknown PNG name(s): ",
			paste(setdiff(sub("\\.png$", "", literal_files), plot_ids), collapse = ", ")
		)
	)
})

#' Sidebar Module - UI
#'
#' UI for progress display, optional fields toggle, document format selection, and download button.
#'
#' @param id Module namespace ID
#' @return UI elements including progress bars, toggle switch, radio buttons, and download button
#' @noRd
mod_sidebar_ui <- function(id) {
	ns <- shiny::NS(id)
	shiny::tagList(
		shiny::h5("Progress", style = "font-weight: bold"),
		shiny::uiOutput(ns("progress_bars")),

		shiny::h5("Hide optional fields", style = "font-weight: bold"),
		shinyWidgets::materialSwitch(
			ns("hide_optional"),
			label = NULL,
			status = "danger"
		),

		shiny::h5("Display warnings", style = "font-weight: bold"),
		shinyWidgets::materialSwitch(
			ns("show_warnings"),
			label = NULL,
			status = "warning",
			value = TRUE
		),

		shiny::h5("Interactive figures", style = "font-weight: bold"),
		{
			status <- interactive_support_status()
			switch_tag <- shinyWidgets::materialSwitch(
				ns("display_mode"),
				label = NULL,
				status = "info"
			)

			if (status$ok) {
				switch_tag
			} else {
				shiny::div(
					style = "display: inline-block; opacity: 0.45;",
					title = paste0(
						"Interactive figures are unavailable because ",
						status$reason,
						". Static figures will be used instead."
					),
					shiny::div(style = "pointer-events: none;", switch_tag)
				)
			}
		},

		# upload existing protocol
		mod_csv_zip_upload_ui(ns("protocol_csv"), "csv", "Upload protocol (.csv)"),

		# upload existing figures
		mod_csv_zip_upload_ui(ns("figures_zip"), "zip", "Upload figures (.zip)"),

		shiny::tags$hr(),

		# download protocol
		shiny::h5("Download protocol", style = "font-weight: bold"),
		shiny::radioButtons(
			ns("document_format"),
			label = NULL,
			choices = c("csv", "html", "pdf", "figures")
		),
		shiny::downloadButton(ns("protocol_download"))
	)
}

#' Sidebar Module - Server
#'
#' Manages download logic, figure availability, filtering protocol data based on user input,
#' and cleans up figures on session end.
#'
#' @param id Module namespace ID
#' @param protocol_data Reactive returning the protocol data frame
#' @param o_objective_1_val Reactive returning the objective value affecting plot selection
#' @param output_dir temporary output directory
#' @return A list containing:
#' \describe{
#'   \item{filtered_protocol_data}{Reactive filtered protocol data frame based on toggle}
#'   \item{hide_optional}{Reactive logical for hide optional toggle}
#'   \item{show_warnings}{Reactive logical for warnings toggle}
#' }
#' @noRd
mod_sidebar_server <- function(
	id,
	protocol_data,
	protocol_dict,
	o_objective_1_val,
	output_dir,
	generate_html
) {
	shiny::moduleServer(id, function(input, output, session) {
		## Reactive filtered protocol data based on "hide optional" toggle
		filtered_protocol_data <- shiny::reactive({
			shiny::req(protocol_data())
			df <- protocol_data()
			dict <- protocol_dict()

			df$visible <- TRUE

			if (isTRUE(input$hide_optional)) {
				df$visible[df$optional == 1] <- FALSE
			}

			# Remove "spatio-temporal" vs "spatial" toggle and fig captions from the progress bar
			df$visible[df$element_id %in% non_progress_ids(dict)] <- FALSE

			df
		})

		# Interactive figures need leaflet plus pandoc; without them the toggle
		# would silently do nothing, so it is disabled instead.
		shiny::observe({
			if (!interactive_supported()) {
				shinyjs::disable("display_mode")
			}
		})

		display_mode <- shiny::reactive({
			if (!interactive_supported()) {
				return("Static")
			}
			if (isTRUE(input$display_mode)) "Interactive" else "Static"
		})

		## Progress bar (reactive to filtered data)
		output$progress_bars <- shiny::renderUI({
			# Use filtered_protocol_data reactive
			df <- filtered_protocol_data()
			shiny::req(df)

			make_bar <- function(data, label, id, bold = FALSE, status = "info") {
				total <- sum(data$visible) # count only visible rows
				if (total == 0) {
					return(NULL)
				}

				filled <- !is.na(data$value) & data$value != "" & data$visible
				completed <- sum(filled, na.rm = TRUE)
				percent <- round(100 * completed / total)

				shiny::div(
					style = if (bold) {
						"font-weight: bold; margin-bottom: 6px;"
					} else {
						"margin-bottom: 6px;"
					},
					shinyWidgets::progressBar(
						id = session$ns(id),
						value = percent,
						total = 100,
						display_pct = TRUE,
						title = label,
						status = if (percent < 100) status else "success"
					)
				)
			}

			# Overall progress (bold)
			overall <- make_bar(
				df,
				"Overall",
				"progress_overall",
				bold = TRUE,
				status = "primary"
			)

			# Section progress bars
			section_bars <- lapply(unique(df$section), function(s) {
				make_bar(
					df[df$section == s, , drop = FALSE],
					paste("Section:", s),
					paste0("progress_", s),
					bold = FALSE,
					status = "info"
				)
			})

			shiny::tagList(overall, section_bars)
		})

		## CSV (protocol data) upload via nested module
		csv_handlers <- mod_csv_zip_upload_server(
			"protocol_csv",
			filetype = "csv",
			read_fn = function(path, ...) {
				df <- utils::read.csv(path, stringsAsFactors = FALSE)
				if (!"element_id" %in% names(df) || all(!nzchar(trimws(df$element_id)))) {
					df$element_id <- normalize_id(df$element)
				}

				df$element_id <- trimws(df$element_id)
				df$value <- as.character(df$value)
				df$value[is.na(df$value)] <- ""
				df
			},
			delete_fn = function(...) {}
		)

		## ZIP (figures) upload via nested module
		zip_handlers <- mod_csv_zip_upload_server(
			"figures_zip",
			filetype = "zip",
			read_fn = function(path, outdir) {
				if (grepl("\\.zip$", basename(path))) {
					utils::unzip(path, exdir = outdir)
					TRUE
				} else {
					stop("This is not a ZIP file")
				}
			},
			delete_fn = function(outdir) {
				delete_plot_png("training_locations", outdir)
				delete_plot_png("training_area", outdir)
				delete_plot_png("prediction_area", outdir)
				delete_plot_png("geodist_training_area", outdir)
				delete_plot_png("geodist_prediction_area", outdir)
			},
			outdir = output_dir
		)

		## Download options
		# Reactive timer for figure existence check, updates every second
		autoInvalidate <- shiny::reactiveTimer(1000)

		figures_exist <- shiny::reactive({
			autoInvalidate()
			length(list.files(output_dir, pattern = "\\.png$")) > 0
		})

		## Enable/disable download button based on figures availability and selected format
		shiny::observe({
			shiny::req(input$document_format)
			if (input$document_format == "figures" && !figures_exist()) {
				shinyjs::disable("protocol_download")
				shiny::showNotification(
					"No figures generated yet. Download disabled.",
					type = "warning"
				)
			} else {
				shinyjs::enable("protocol_download")
			}
		})

		## Clean up figures in output_dir on session end
		session$onSessionEnded(function() {
			if (dir.exists(output_dir)) {
				files <- list.files(
					output_dir,
					pattern = "\\.(png|html|rds|Rmd|qmd)$",
					full.names = TRUE
				)
				if (length(files) > 0) file.remove(files)
			}
		})

		## Download handler for protocol data (csv/pdf/figures zip)
		output$protocol_download <- shiny::downloadHandler(
			filename = function() {
				ext <- switch(
					input$document_format,
					"csv" = "csv",
					"html" = "html",
					"pdf" = "pdf",
					"figures" = "zip"
				)
				paste0("protocol_", Sys.Date(), ".", ext)
			},
			content = function(file) {
				if (input$document_format == "csv") {
					df <- filtered_protocol_data()
					if ("visible" %in% names(df)) {
						df <- df[, setdiff(names(df), "visible"), drop = FALSE]
					}
					utils::write.csv(df, file, row.names = FALSE)
				} else if (input$document_format == "html") {
					file.copy(generate_html("sections"), file, overwrite = TRUE)
				} else if (input$document_format == "pdf") {
					# Interactive widgets cannot be printed reproducibly: an iframe
					# captures only its viewport, at whatever zoom and with whatever
					# tiles happened to load. The PNG exists in both modes.
					html_file <- generate_html(embed = "none")
					html_file <- normalizePath(html_file)

					pagedown::chrome_print(
						input = html_file,
						output = file
					)
				} else if (input$document_format == "figures") {
					subdir_zip <- "figures_for_zip"
					allowed_ids <- get_allowed_element_ids(
						o_objective_1_val(),
						uploaded_figure_ids = visible_uploaded_figure_ids(
							protocol_dict = protocol_dict(),
							output_dir = output_dir,
							hide_optional = isTRUE(input$hide_optional)
						)
					)

					figures_to_zip <- get_selected_plot_files(
						output_dir,
						allowed_ids,
						copy_subdir = subdir_zip,
						return_relative = FALSE
					)

					# Staged flat, so a re-uploaded ZIP still finds <id>.png at the
					# top level exactly as before.
					stage <- file.path(output_dir, "zip_stage")
					unlink(stage, recursive = TRUE)
					dir.create(stage, recursive = TRUE)
					file.copy(figures_to_zip, stage, overwrite = TRUE)

					# Interactive widgets, if any were produced for a visible figure.
					# The standalone HTML is generated here rather than on every
					# re-render, because each one costs a pandoc call.
					widget_files <- vapply(
						allowed_ids,
						function(id) export_widget_html(id, output_dir) %||% "",
						character(1)
					)
					widget_files <- widget_files[nzchar(widget_files)]

					if (length(widget_files) > 0) {
						dir.create(file.path(stage, "interactive"), showWarnings = FALSE)
						file.copy(widget_files, file.path(stage, "interactive"), overwrite = TRUE)

						# Linked mode keeps the report an order of magnitude smaller
						# than the data-URI single file, and browsable after extraction
						report <- generate_html(embed = "link")
						file.copy(report, file.path(stage, "report.html"), overwrite = TRUE)
					}

					zipfile <- file.path(output_dir, "figures.zip")
					unlink(zipfile)

					withr::with_dir(stage, {
						utils::zip(zipfile = zipfile, files = list.files(".", recursive = TRUE))
					})

					file.copy(zipfile, file)
					unlink(file.path(output_dir, subdir_zip), recursive = TRUE)
					unlink(stage, recursive = TRUE)
					unlink(zipfile)
				}
			}
		)

		# Return reactive values for use in app
		list(
			csv = csv_handlers$data,
			zip = zip_handlers$data,
			filtered_protocol_data = filtered_protocol_data,
			hide_optional = shiny::reactive(input$hide_optional),
			show_warnings = shiny::reactive(input$show_warnings),
			display_mode = display_mode
		)
	})
}

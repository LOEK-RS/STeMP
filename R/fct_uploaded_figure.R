#' Read an uploaded image and re-encode it as PNG
#'
#' Decodes the upload to a pixel array and writes a fresh PNG. This validates
#' the file (a non-image fails to decode) and strips any embedded content,
#' which matters because output_dir is served over HTTP by addResourcePath().
#'
#' @param source_path Path to the uploaded temporary file
#' @param target_path Destination PNG path
#' @return TRUE on success, FALSE otherwise
#' @noRd
reencode_uploaded_image <- function(source_path, target_path) {
	if (!file.exists(source_path)) {
		return(FALSE)
	}

	image_array <- tryCatch(
		{
			signature <- readBin(source_path, "raw", n = 8)

			if (identical(signature[1:8], as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))) {
				png::readPNG(source_path)
			} else if (identical(signature[1:3], as.raw(c(0xff, 0xd8, 0xff)))) {
				jpeg::readJPEG(source_path)
			} else {
				NULL
			}
		},
		error = function(e) NULL
	)

	if (is.null(image_array)) {
		return(FALSE)
	}

	ok <- tryCatch(
		{
			dir.create(dirname(target_path), showWarnings = FALSE, recursive = TRUE)
			png::writePNG(image_array, target_path)
			TRUE
		},
		error = function(e) FALSE
	)

	ok
}

#' Render an upload slot for a user-supplied figure
#'
#' @param element_id Namespaced element ID
#' @param label Label text
#' @param info_text Optional tooltip
#' @noRd
render_uploaded_figure <- function(element_id, label, info_text = NULL) {
	inputTag <- shiny::div(
		id = paste0(element_id, "_field"),
		class = "form-group shiny-input-container",
		shiny::tags$label(`for` = element_id, class = "control-label", label),
		shiny::div(
			style = "display: flex; gap: 6px; align-items: flex-start;",
			shiny::div(
				style = "flex: 1;",
				shiny::fileInput(
					inputId = paste0(element_id, "_upload"),
					label = NULL,
					accept = c("image/png", "image/jpeg", ".png", ".jpg", ".jpeg"),
					placeholder = "PNG or JPEG"
				)
			),
			shiny::actionButton(
				inputId = paste0(element_id, "_remove"),
				label = "Remove",
				class = "btn btn-sm btn-danger"
			)
		),
		shiny::uiOutput(outputId = paste0(element_id, "_img"))
	)
	with_tooltip(inputTag, info_text)
}


#' Server logic for one uploaded-figure slot
#'
#' Writes the re-encoded upload to output_dir/<element_id>.png, which is the
#' same convention the derived figures use, so the preview, PDF and ZIP need
#' no knowledge of where a figure came from.
#'
#' @param input,output,session Shiny module objects
#' @param element_id Un-namespaced element ID (also the PNG stem)
#' @param output_dir Directory holding the protocol figures
#' @param max_bytes Maximum accepted upload size
#' @noRd
uploaded_figure_server <- function(input, output, session, element_id, output_dir, max_bytes = 10 * 1024^2) {
	target_path <- file.path(output_dir, paste0(element_id, ".png"))
	upload_id <- paste0(element_id, "_upload")
	remove_id <- paste0(element_id, "_remove")

	# Bumped on every change so the preview re-renders from disk
	figure_version <- shiny::reactiveVal(0L)

	shiny::observeEvent(input[[upload_id]], {
		file_info <- input[[upload_id]]
		shiny::req(file_info)

		# The fileInput is inside a renderUI, so it is re-created whenever the
		# panel re-renders. The stale datapath is gone by then; ignore it.
		if (!file.exists(file_info$datapath)) {
			return(invisible(NULL))
		}

		if (file_info$size > max_bytes) {
			shiny::showNotification(
				sprintf("Figure is too large (max %.0f MB).", max_bytes / 1024^2),
				type = "error"
			)
			return(invisible(NULL))
		}

		if (!reencode_uploaded_image(file_info$datapath, target_path)) {
			shiny::showNotification(
				"Could not read this file. Please upload a PNG or JPEG image.",
				type = "error"
			)
			return(invisible(NULL))
		}

		figure_version(figure_version() + 1L)
	})

	shiny::observeEvent(input[[remove_id]], {
		if (file.exists(target_path)) {
			file.remove(target_path)
		}
		shinyjs::reset(upload_id)
		figure_version(figure_version() + 1L)
	})

	output[[paste0(element_id, "_img")]] <- shiny::renderUI({
		figure_version()
		if (!file.exists(target_path)) {
			return(NULL)
		}
		shiny::div(
			style = "display: flex; justify-content: center;",
			shiny::tags$img(
				# Cache-buster: the path is stable across re-uploads
				src = paste0("temp_stemp/", basename(target_path), "?v=", as.integer(Sys.time())),
				style = "max-height: 400px; max-width: 100%;"
			)
		)
	})

	shiny::outputOptions(output, paste0(element_id, "_img"), suspendWhenHidden = FALSE)
}

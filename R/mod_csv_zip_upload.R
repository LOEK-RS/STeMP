#' CSV/ZIP Upload Module UI
#'
#' Provides a file input UI for uploading (and deleting) CSV (protocol data) or ZIP (figures) files.
#'
#' @param id Module namespace ID
#' @param filetype A character string, either `csv` or `zip`; determines accepted file extension
#'   and is used to compose input/output ids
#' @param label Label for the file input block (displayed above buttons)
#' @return UI elements for file input (tagList)
#' @noRd

mod_csv_zip_upload_ui <- function(id, filetype, label) {
	ns <- shiny::NS(id)

	shiny::tagList(
		shiny::h5(
			label,
			style = "font-weight: bold; margin-bottom: 5px;"
		),

		# Single row: Browse and Delete buttons
		shiny::div(
			style = "display: flex; gap: 6px; align-items: center; margin-bottom: 6px;",

			# Custom Browse button
			shiny::actionButton(
				ns(paste0("browse_trigger_", filetype)),
				label = "Browse",
				class = "btn btn-sm btn-primary",
				style = "flex: 1;"
			),

			# Delete button
			shiny::actionButton(
				ns(paste0("delete_", filetype)),
				label = "Delete",
				class = "btn btn-sm btn-danger",
				style = "width: 90px;"
			)
		),

		shiny::uiOutput(ns(paste0(filetype, "_status"))),

		# hidden native file input
		shiny::div(
			style = "display: none;",
			shiny::fileInput(
				ns(paste0(filetype, "_upload")),
				label = NULL,
				accept = paste0(".", filetype),
				buttonLabel = "Hidden",
				width = "0px"
			)
		)
	)
}

#' CSV/ZIP Upload Module Server
#'
#' Handles the server logic for uploading and managing a CSV (protocol data) or ZIP (figures) file.
#' Triggers a hidden file input from a custom button, reads the uploaded file with the provided
#' read_fn, enables a delete button on success, and calls delete_fn on deletion. Updates a
#' status UI element named paste0(filetype, "_status") ("csv_status" or "zip_status")
#' with success, error, or deletion messages.
#'
#' @param id Module namespace ID
#' @param filetype Character string, either `csv` or `zip`; used to compose input/output ids
#' @param read_fn Function(datapath, ...) called when a file is uploaded; should return the parsed
#'   object that will be stored in the returned reactiveVal (or throw an error to signal failure)
#' @param delete_fn Function(...) supplied by the caller; executed when the delete button is pressed
#'   to perform cleanup (e.g. file removal)
#'
#' @return A list with reactive elements:
#' \describe{
#'   \item{data}{A reactiveVal containing the object returned by read_fn, or NULL if none}
#' }
#' @noRd
mod_csv_zip_upload_server <- function(id, filetype, read_fn, delete_fn, ...) {
	shiny::moduleServer(id, function(input, output, session) {
		ns <- session$ns

		data_rv <- shiny::reactiveVal(NULL)
		deleted_rv <- shiny::reactiveVal(FALSE)
		status_output <- paste0(filetype, "_status")

		# Disable "Delete" button as long as no file is uploaded
		shiny::observe({
			if (is.null(data_rv())) {
				shinyjs::disable(paste0("delete_", filetype))
			} else {
				shinyjs::enable(paste0("delete_", filetype))
			}
		})

		# Trigger hidden file input when custom upload button is clicked
		shiny::observeEvent(
			input[[paste0("browse_trigger_", filetype)]],
			{
				shinyjs::click(paste0(filetype, "_upload"))
			},
			ignoreNULL = TRUE
		)

		# Handle file upload
		shiny::observeEvent(input[[paste0(filetype, "_upload")]], {
			shiny::req(input[[paste0(filetype, "_upload")]])
			tryCatch(
				{
					res <- read_fn(input[[paste0(filetype, "_upload")]]$datapath, ...)
					data_rv(res)
					shinyjs::enable(paste0("delete_", filetype))
					output[[status_output]] <- shiny::renderUI({
						shiny::tags$p(paste0(toupper(filetype), " file loaded successfully."), style = "color: blue;")
					})
				},
				error = function(e) {
					output[[status_output]] <- shiny::renderUI({
						shiny::tags$p(paste("Error loading", toupper(filetype), "file."), style = "color: red;")
					})
				}
			)
		})

		# Delete handler
		shiny::observeEvent(
			input[[paste0("delete_", filetype)]],
			{
				data_rv(NULL)
				shinyjs::reset(paste0(filetype, "_upload"))
				delete_fn(...)
				shinyjs::disable(paste0("delete_", filetype))
				deleted_rv(TRUE)
				output[[status_output]] <- shiny::renderUI({
					shiny::tags$p(paste0(toupper(filetype), " file upload deleted."), style = "color: orange;")
				})
			},
			ignoreInit = TRUE
		)

		list(data = data_rv)
	})
}

#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
	onStart = NULL,
	options = list(),
	enableBookmarking = NULL,
	uiPattern = "/",
	...
) {
	max_mb <- tryCatch(get_golem_config("max_upload_mb"), error = function(e) NULL)
	if (!is.numeric(max_mb) || length(max_mb) != 1L || is.na(max_mb) || max_mb <= 0) {
		max_mb <- 100
	}

	with_golem_options(
		app = shinyApp(
			ui = app_ui,
			server = app_server,
			onStart = function() {
				old <- base::options(shiny.maxRequestSize = max_mb * 1024^2)
				shiny::onStop(function() base::options(old))
			},
			options = options,
			enableBookmarking = enableBookmarking,
			uiPattern = uiPattern
		),
		golem_opts = list(...)
	)
}

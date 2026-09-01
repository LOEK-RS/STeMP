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
	# Upload limit for user-supplied figures; slightly above the per-file limit
	# enforced in uploaded_figure_server(), so oversized files reach our handler
	# and get a readable message instead of a Shiny error page.
	old_max <- options(shiny.maxRequestSize = get_golem_config("max_upload_mb") * 1024^2)
	on.exit(options(old_max), add = TRUE)

	# Increase max upload size
	#options(shiny.maxRequestSize = 30 * 1024^2)  # 30 MB

	with_golem_options(
		app = shinyApp(
			ui = app_ui,
			server = app_server,
			onStart = onStart,
			options = options,
			enableBookmarking = enableBookmarking,
			uiPattern = uiPattern
		),
		golem_opts = list(...)
	)
}

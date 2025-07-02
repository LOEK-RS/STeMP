#' Launch the STeMP shiny app
#' @export
run_app <- function() {
  appDir <- system.file("app", package = "STeMP")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing the package.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}

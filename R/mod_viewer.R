mod_viewer_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(2),
      column(8, htmlOutput(ns("markdown"))),
      column(2)
    )
  )
}

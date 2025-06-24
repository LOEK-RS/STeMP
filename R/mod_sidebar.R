mod_sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h5("Progress", style = "font-weight: bold"),
    uiOutput(ns("progress_bars")),
    
    h5("Hide optional fields", style = "font-weight: bold"),
    materialSwitch(ns("hide_optional"), label = NULL, status = "danger"),
    
    h5("Download protocol", style = "font-weight: bold"),
    radioButtons(ns("document_format"), label = NULL, choices = c("csv", "docx")),
    downloadButton(ns("protocol_download"))
  )
}

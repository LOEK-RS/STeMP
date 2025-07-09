#' Add Tooltip Info to Shiny Input Tag
#'
#' @param inputTag A Shiny input tag.
#' @param info_text Text to show in tooltip.
#' @return Modified input tag with tooltip added.
#' @noRd
inputWithHoverInfo <- function(inputTag, info_text) {
  children <- inputTag$children
  if (is.null(children) || !is.list(children)) {
    return(inputTag)
  }

  labelIndex <- which(sapply(children, function(x) inherits(x, "shiny.tag") && x$name == "label"))

  if (length(labelIndex) == 1 && !is.null(info_text) && nchar(info_text) > 0) {
    originalLabel <- children[[labelIndex]]

    wrappedLabel <- htmltools::tags$div(class = "input-label-icon",
                                        originalLabel,
                                        htmltools::tags$span(
                                          shiny::icon("info-circle"),
                                          class = "info-hover-icon",
                                          `data-toggle` = "tooltip",
                                          `data-placement` = "right",
                                          `data-html` = "true",
                                          title = htmltools::HTML(info_text)
                                        )
    )

    inputTag$children[[labelIndex]] <- wrappedLabel
  }

  inputTag
}

#' Add Tooltip to Shiny Input if Info Text Provided
#'
#' @param inputTag A Shiny input tag.
#' @param info_text Optional tooltip text.
#' @return Shiny input tag with tooltip if info_text is not NULL/empty.
#' @noRd
with_tooltip <- function(inputTag, info_text = NULL) {
  if (!is.null(info_text) && nchar(info_text) > 0) {
    inputWithHoverInfo(inputTag, info_text)
  } else {
    inputTag
  }
}



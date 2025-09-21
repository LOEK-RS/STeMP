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
    shinyWidgets::materialSwitch(ns("hide_optional"), label = NULL, status = "danger"),

    shiny::h5("Display warnings", style = "font-weight: bold"),
    shinyWidgets::materialSwitch(
      ns("show_warnings"),
      label = NULL,
      status = "warning",
      value = TRUE
    ),

    shiny::h5("Download protocol", style = "font-weight: bold"),
    shiny::radioButtons(ns("document_format"), label = NULL, choices = c("csv", "pdf", "figures")),
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
mod_sidebar_server <- function(id, protocol_data, o_objective_1_val, output_dir) {
  shiny::moduleServer(id, function(input, output, session) {

    ## Reactive filtered protocol data based on "hide optional" toggle
    filtered_protocol_data <- shiny::reactive({
      shiny::req(protocol_data())
      df <- protocol_data()
      # Keep all rows, but optionally mark optional rows
      df$visible <- TRUE
      if (isTRUE(input$hide_optional)) {
        df$visible[df$optional == 1] <- FALSE
      }
      df
    })


    ## Progress bar (reactive to filtered data)
    output$progress_bars <- shiny::renderUI({
    # Use filtered_protocol_data reactive
    df <- filtered_protocol_data()
    shiny::req(df)

    make_bar <- function(data, label, id, bold = FALSE, status = "info") {
      total <- sum(data$visible)   # count only visible rows
      if (total == 0) return(NULL)

      filled <- !is.na(data$value) & data$value != "" & data$visible
      completed <- sum(filled, na.rm = TRUE)
      percent <- round(100 * completed / total)

      shiny::div(
        style = if (bold) "font-weight: bold; margin-bottom: 6px;" else "margin-bottom: 6px;",
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
    overall <- make_bar(df, "Overall", "progress_overall", bold = TRUE, status = "primary")

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


    ## Reactive timer for figure existence check, updates every second
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
        shiny::showNotification("No figures generated yet. Download disabled.", type = "warning")
      } else {
        shinyjs::enable("protocol_download")
      }
    })

    ## Clean up figures in output_dir on session end
    session$onSessionEnded(function() {
      if (dir.exists(output_dir)) {
        files <- list.files(output_dir, pattern = "\\.(png|Rmd)$", full.names = TRUE)
        if (length(files) > 0) file.remove(files)
      }
    })

    ## Download handler for protocol data (csv/pdf/figures zip)
    output$protocol_download <- shiny::downloadHandler(
      filename = function() {
        ext <- switch(input$document_format,
                      "csv" = "csv",
                      "pdf" = "pdf",
                      "figures" = "zip")
        paste0("protocol_", Sys.Date(), ".", ext)
      },
      content = function(file) {
        if (input$document_format == "csv") {
          utils::write.csv(filtered_protocol_data(), file, row.names = FALSE)

        } else if (input$document_format == "pdf") {
          subdir_pdf <- "figures_for_pdf"
          allowed_ids <- get_allowed_element_ids(o_objective_1_val())
          plot_files_rel <- get_selected_plot_files(output_dir, allowed_ids, copy_subdir = subdir_pdf)

          temp_rmd <- file.path(output_dir, "protocol_temp.Rmd")
          template_path <- app_sys("app/www/protocol_template.Rmd")
          file.copy(template_path, temp_rmd, overwrite = TRUE)

          df_sanitized <- filtered_protocol_data() |> 
            dplyr::mutate(dplyr::across(dplyr::everything(), sanitize_latex))

          rmarkdown::render(
            input = temp_rmd,
            output_file = file,
            params = list(
              data = df_sanitized,
              plot_files = plot_files_rel
            ),
            envir = new.env(parent = globalenv()),
            quiet = TRUE,
            clean = TRUE
          )

          unlink(file.path(output_dir, subdir_pdf), recursive = TRUE)

        } else if (input$document_format == "figures") {
          subdir_zip <- "figures_for_zip"
          allowed_ids <- get_allowed_element_ids(o_objective_1_val())
          figures_to_zip <- get_selected_plot_files(output_dir, allowed_ids, copy_subdir = subdir_zip, return_relative = FALSE)

          zipfile <- file.path(output_dir, "figures.zip")
          utils::zip(zipfile = zipfile, files = figures_to_zip, flags = "-j")

          file.copy(zipfile, file)
          unlink(file.path(output_dir, subdir_zip), recursive = TRUE)
          unlink(zipfile)
        }
      }
    )

    # Return reactive values for use in app
    list(
      filtered_protocol_data = filtered_protocol_data,
      hide_optional = shiny::reactive(input$hide_optional),
      show_warnings = shiny::reactive(input$show_warnings)
    )
  })
}

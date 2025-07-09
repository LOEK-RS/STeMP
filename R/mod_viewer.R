#' Protocol PDF Viewer Module - UI
#'
#' UI for displaying a protocol PDF preview and a button to generate/refresh the PDF.
#'
#' @param id Module namespace ID
#' @return UI elements including a header, action button, and PDF preview output
#' @noRd
mod_viewer_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Protocol PDF Preview"),
    shiny::actionButton(ns("preview_pdf"), "Generate / Refresh PDF Preview"),
    shiny::br(), shiny::br(),
    shiny::uiOutput(ns("pdf_preview_ui"))
  )
}

#' Protocol PDF Viewer Module - Server
#'
#' Generates a PDF preview of the protocol using provided data and plots,
#' manages temporary files, and renders a PDF viewer iframe.
#'
#' @param id Module namespace ID
#' @param protocol_data Reactive returning a data frame with protocol data
#' @param o_objective_1_val Reactive returning a string controlling which plots to include
#' @param output_dir temporary output directory
#' @return None; creates reactive outputs and handles side effects
#' @noRd
mod_viewer_server <- function(id, protocol_data, o_objective_1_val, output_dir) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # define subdirectory for preview figures
    subdir_preview <- "figures_preview"

    # Cleanup any protocol preview PDFs in output_dir when session ends
    session$onSessionEnded(function() {
      old_files <- list.files(output_dir, pattern = paste0("^protocol_preview_", session$token, ".*\\.pdf$"), full.names = TRUE)
      if (length(old_files) > 0) file.remove(old_files)
    })

    pdf_preview_path <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$preview_pdf, {
      shiny::req(protocol_data())

      # Select and copy plot files based on objective
      allowed_ids <- get_allowed_element_ids(o_objective_1_val())
      plot_files_rel <- get_selected_plot_files(
        output_dir = output_dir,
        allowed_ids = allowed_ids,
        copy_subdir = subdir_preview,
        return_relative = TRUE
      )

      temp_rmd <- file.path(output_dir, "protocol_temp.Rmd")
      template_path <- app_sys("app/www/protocol_template.Rmd")
      file.copy(template_path, temp_rmd, overwrite = TRUE)

      # Remove old preview PDFs
      old_pdfs <- list.files(output_dir, pattern = paste0("^protocol_preview_", session$token, "_.*\\.pdf$"), full.names = TRUE)
      if (length(old_pdfs) > 0) file.remove(old_pdfs)

      temp_pdf_file <- file.path(output_dir, paste0("protocol_preview_", session$token, "_", as.integer(Sys.time()), ".pdf"))

      # load protocol data
      df <- protocol_data()
      df_sanitized <- df |> dplyr::mutate(dplyr::across(dplyr::everything(), sanitize_latex))

      rmarkdown::render(
        input = temp_rmd,
        output_file = temp_pdf_file,
        params = list(
          data = df_sanitized,
          plot_files = plot_files_rel
        ),
        envir = new.env(parent = globalenv()),
        quiet = TRUE,
        clean = TRUE
      )

      pdf_preview_path(temp_pdf_file)

      # Optional: Clean up temp plot files after render
      temp_figures_dir <- file.path(output_dir, subdir_preview)
      if (dir.exists(temp_figures_dir)) {
        unlink(temp_figures_dir, recursive = TRUE)
      }
    })

    output$pdf_preview_ui <- shiny::renderUI({
      shiny::req(pdf_preview_path())

      preview_filename <- paste0("protocol_preview_", session$token, ".pdf")
      preview_www_path <- file.path(output_dir, preview_filename)

      file.copy(pdf_preview_path(), preview_www_path, overwrite = TRUE)

      shiny::tags$iframe(
        src = file.path("temp_stemp", preview_filename),
        style = "width:100%; height:700px;",
        frameborder = 0
      )
    })
  })
}


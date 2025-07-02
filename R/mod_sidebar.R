# UI
mod_sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h5("Progress", style = "font-weight: bold"),
    uiOutput(ns("progress_bars")),
    
    h5("Hide optional fields", style = "font-weight: bold"),
    materialSwitch(ns("hide_optional"), label = NULL, status = "danger"),
    
    shinyjs::useShinyjs(),
    h5("Download protocol", style = "font-weight: bold"),
    radioButtons(ns("document_format"), label = NULL, choices = c("csv", "pdf", "figures")),
    downloadButton(ns("protocol_download"))
  )
}

# Server
mod_sidebar_server <- function(id, protocol_data, o_objective_1_val) {
  moduleServer(id, function(input, output, session) {
    
    # Add a reactive timer that invalidates every second
    autoInvalidate <- reactiveTimer(1000)  # 1000 ms = 1 sec
    
    # This now updates every second
    figures_exist <- reactive({
      autoInvalidate()
      plot_dir <- file.path("www", "figures")
      length(list.files(plot_dir, pattern = "\\.png$")) > 0
    })
    
    observe({
      req(input$document_format)
      if (input$document_format == "figures" && !figures_exist()) {
        shinyjs::disable("protocol_download")
        showNotification("No figures generated yet. Download disabled.", type = "warning")
      } else {
        shinyjs::enable("protocol_download")
      }
    })
    
    
    # Clean permanent figures on session end
    session$onSessionEnded(function() {
      fig_dir <- file.path("www", "figures")
      if (dir.exists(fig_dir)) {
        files <- list.files(fig_dir, pattern = "*\\.png$", full.names = TRUE)
        if (length(files) > 0) {
          file.remove(files)
        }
      }
    })
    
    # Helper: get allowed element_ids based on objective
    get_allowed_element_ids <- function(objective) {
      switch(
        objective,
        "Model and prediction" = c(
          "sampling_locations",
          "sampling_area",
          "prediction_area",
          "geodist_prediction_area"
        ),
        "Model only" = c(
          "sampling_locations",
          "sampling_area",
          "geodist_sampling_area"
        ),
        character(0)
      )
    }
    
    output$protocol_download <- downloadHandler(
      filename = function() {
        ext <- switch(input$document_format,
                      "csv" = "csv",
                      "pdf" = "pdf",
                      "figures" = "zip")
        paste0("protocol_", Sys.Date(), ".", ext)
      },
      content = function(file) {
        ## csv files ----------
        if (input$document_format == "csv") {
          write.csv(protocol_data(), file, row.names = FALSE)
          
          ## pdf files ----------
        } else if (input$document_format == "pdf") {
          df <- protocol_data()
          
          plot_dir <- file.path("www", "figures")
          all_plot_files <- list.files(plot_dir, pattern = "\\.png$", full.names = TRUE)
          
          # Filter plot files based on allowed element_ids
          allowed_ids <- get_allowed_element_ids(o_objective_1_val())
          file_names <- basename(all_plot_files)
          ids_no_ext <- trimws(sub("\\.png$", "", tolower(file_names)))
          allowed_ids_lc <- trimws(tolower(allowed_ids))
          
          match_idx <- match(allowed_ids_lc, ids_no_ext)
          valid_idx <- which(!is.na(match_idx))
          selected_files <- all_plot_files[match_idx[valid_idx]]
          
          
          # Prepare temp .Rmd and copy plots
          temp_dir <- tempdir()
          temp_rmd <- file.path(temp_dir, "protocol_temp.Rmd")
          template_path <- "www/protocol_template.Rmd"
          file.copy(template_path, temp_rmd, overwrite = TRUE)
          
          temp_fig_dir <- file.path(temp_dir, "figures")
          if (!dir.exists(temp_fig_dir)) dir.create(temp_fig_dir)
          file.copy(selected_files, temp_fig_dir, overwrite = TRUE)
          
          plot_files_rel <- file.path("figures", basename(selected_files))
          
          # remove underscores etc., which cause errors in latex
          sanitize_latex <- function(x) {
            x <- gsub("\\\\", "\\textbackslash{}", x)   # backslash first
            x <- gsub("([_%&$#{}])", "\\\\\\1", x)      # escape special chars
            x <- gsub("\n", " ", x)                      # remove newlines if needed
            x
          }
          
          df_sanitized <- df |> dplyr::mutate(across(everything(), sanitize_latex))
          
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
          
          ## figures folder (zip) ----------
        } else if (input$document_format == "figures") {
          plot_dir <- file.path("www", "figures")
          all_plot_files <- list.files(plot_dir, pattern = "\\.png$", full.names = TRUE)
          
          allowed_ids <- get_allowed_element_ids(o_objective_1_val())
          file_names <- basename(all_plot_files)
          ids_no_ext <- trimws(sub("\\.png$", "", tolower(file_names)))
          allowed_ids_lc <- trimws(tolower(allowed_ids))
          
          match_idx <- match(allowed_ids_lc, ids_no_ext)
          valid_idx <- which(!is.na(match_idx))
          selected_files <- all_plot_files[match_idx[valid_idx]]
          
          temp_dir <- tempdir()
          zip_dir <- file.path(temp_dir, "figures_for_zip")
          dir.create(zip_dir, showWarnings = FALSE)
          
          file.copy(selected_files, zip_dir, overwrite = TRUE)
          
          zipfile <- file.path(temp_dir, "figures.zip")
          old_wd <- getwd()
          setwd(zip_dir)
          zip(zipfile, files = list.files(zip_dir))
          setwd(old_wd)
          
          file.copy(zipfile, file)
          
          unlink(zip_dir, recursive = TRUE)
          unlink(zipfile)
        }
      }
    )
  })
}

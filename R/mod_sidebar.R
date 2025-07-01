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
    
    figures_exist <- reactive({
      plot_dir <- file.path("www", "figures")
      length(list.files(plot_dir, pattern = "\\.png$")) > 0
    })
    
    observe({
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
        files <- list.files(fig_dir, pattern = "^protocol_plot_.*\\.png$", full.names = TRUE)
        if (length(files) > 0) {
          file.remove(files)
          message("Permanent figures cleaned up on session end.")
        }
      }
    })
    
    # Helper: get allowed element_ids based on objective
    get_allowed_element_ids <- reactive({
      if (is.null(o_objective_1_val())) return(character(0))
      
      if (o_objective_1_val() == "Model and prediction") {
        c("sampling_locations",
          "sampling_area",
          "prediction_area",
          "geodist_prediction_area")
      } else if (o_objective_1_val() == "Model only") {
        c("sampling_locations",
          "sampling_area",
          "geodist_sampling_area")
      } else {
        character(0)
      }
    })
    
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
          allowed_ids <- get_allowed_element_ids()
          # We assume plot file names contain the element_id strings, so filter:
          selected_files <- all_plot_files[sapply(all_plot_files, function(f) {
            any(sapply(allowed_ids, function(id) grepl(id, basename(f), fixed = TRUE)))
          })]
          
          # Prepare temp .Rmd and copy plots
          temp_dir <- tempdir()
          temp_rmd <- file.path(temp_dir, "protocol_temp.Rmd")
          template_path <- "www/protocol_template.Rmd"
          file.copy(template_path, temp_rmd, overwrite = TRUE)
          
          temp_fig_dir <- file.path(temp_dir, "figures")
          if (!dir.exists(temp_fig_dir)) dir.create(temp_fig_dir)
          file.copy(selected_files, temp_fig_dir, overwrite = TRUE)
          
          plot_files_rel <- file.path("figures", basename(selected_files))
          
          rmarkdown::render(
            input = temp_rmd,
            output_file = file,
            params = list(
              data = df,
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
          
          allowed_ids <- get_allowed_element_ids()
          selected_files <- all_plot_files[sapply(all_plot_files, function(f) {
            any(sapply(allowed_ids, function(id) grepl(id, basename(f), fixed = TRUE)))
          })]
          
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

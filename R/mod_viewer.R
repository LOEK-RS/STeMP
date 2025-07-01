mod_viewer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Protocol PDF Preview"),
    actionButton(ns("preview_pdf"), "Generate / Refresh PDF Preview"),
    br(), br(),
    uiOutput(ns("pdf_preview_ui"))
  )
}

mod_viewer_server <- function(id, protocol_data, o_objective_1_val) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    session$onSessionEnded(function() {
      www_dir <- normalizePath("www", mustWork = TRUE)
      pdf_files <- list.files(www_dir, pattern = "^protocol_preview_.*\\.pdf$", full.names = TRUE)
      if (length(pdf_files) > 0) {
        file.remove(pdf_files)
      }
    })
    
    pdf_preview_path <- reactiveVal(NULL)
    
    observeEvent(input$preview_pdf, {
      req(protocol_data())
      df <- protocol_data()
      
      plot_dir <- file.path("www", "figures")
      all_plot_files <- list.files(plot_dir, pattern = "\\.png$", full.names = TRUE)
      
      allowed_ids <- switch(
        o_objective_1_val(),
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
      
      if (length(allowed_ids) == 0 || length(all_plot_files) == 0) {
        selected_files <- character(0)
      } else {
        file_names <- basename(all_plot_files)
        ids_no_ext <- trimws(sub("\\.png$", "", tolower(file_names)))
        allowed_ids_lc <- trimws(tolower(allowed_ids))
        
        match_idx <- match(allowed_ids_lc, ids_no_ext)
        
        # filter valid matches only
        valid_idx <- which(!is.na(match_idx))
        selected_files <- all_plot_files[match_idx[valid_idx]]
        
      }
      
      
      
      temp_dir <- tempdir()
      temp_rmd <- file.path(temp_dir, "protocol_temp.Rmd")
      template_path <- "www/protocol_template.Rmd"
      file.copy(template_path, temp_rmd, overwrite = TRUE)
      
      temp_fig_dir <- file.path(temp_dir, "figures")
      if (!dir.exists(temp_fig_dir)) dir.create(temp_fig_dir)
      file.copy(selected_files, temp_fig_dir, overwrite = TRUE)
      
      plot_files_rel <- file.path("figures", basename(selected_files))
      
      # Clean up old protocol preview PDFs in temp_dir
      old_pdfs <- list.files(temp_dir, pattern = paste0("^protocol_preview_", session$token, "_.*\\.pdf$"), full.names = TRUE)
      if (length(old_pdfs) > 0) file.remove(old_pdfs)
      
      temp_pdf_file <- file.path(temp_dir, paste0("protocol_preview_", session$token, "_", as.integer(Sys.time()), ".pdf"))
      
      rmarkdown::render(
        input = temp_rmd,
        output_file = temp_pdf_file,
        params = list(
          data = df,
          plot_files = plot_files_rel
        ),
        envir = new.env(parent = globalenv()),
        quiet = TRUE,
        clean = TRUE
      )
      
      pdf_preview_path(temp_pdf_file)
    })
    
    output$pdf_preview_ui <- renderUI({
      req(pdf_preview_path())
      
      # Copy to www to serve via iframe
      preview_www_path <- file.path("www", paste0("protocol_preview_", session$token, ".pdf"))
      
      # Delete any old PDFs for this session in www/
      old_www_pdfs <- list.files("www", pattern = paste0("^protocol_preview_", session$token, ".*\\.pdf$"), full.names = TRUE)
      if (length(old_www_pdfs) > 0) {
        file.remove(old_www_pdfs)
      }
      
      file.copy(pdf_preview_path(), preview_www_path, overwrite = TRUE)
      
      tags$iframe(
        src = paste0("protocol_preview_", session$token, ".pdf"),
        style = "width:100%; height:700px;",
        frameborder = 0
      )
    })
    
  })
}

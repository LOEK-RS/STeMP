#' Upload Module - UI
#'
#' UI for uploading protocol CSV, model RDS, and geospatial data (gpkg),
#' including delete buttons for geospatial uploads.
#'
#' @param id Module namespace ID
#' @return UI elements for file uploads and status messages
#' @noRd
mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      shiny::column(2),
      shiny::column(8,
                    shiny::p(shiny::tags$b("(1) Upload STeMP protocol (.csv)")),
                    shiny::fileInput(ns("csv_upload"), label = NULL, accept = c(".csv")),
                    shiny::uiOutput(ns("csv_status")),
                    shiny::tags$hr(),

                    shiny::p(shiny::tags$b("(2) Upload model (.RDS)")),
                    shiny::fileInput(ns("model_upload"), "Upload model object (.RDS)", accept = ".rds"),
                    shiny::uiOutput(ns("model_status")),
                    shiny::tags$hr(),

                    shiny::p(shiny::tags$b("(3) Upload geospatial data (.gpkg)")),
                    mod_gpkg_upload_ui(ns("samples"), label = "Upload sampling locations"),
                    shiny::actionButton(ns("delete_samples"), "Delete Uploaded samples", style = "margin-bottom: 15px;"),
                    mod_gpkg_upload_ui(ns("training_area"), label = "Upload training area"),
                    shiny::actionButton(ns("delete_training_area"), "Delete Uploaded training area", style = "margin-bottom: 15px;"),
                    mod_gpkg_upload_ui(ns("prediction_area"), label = "Upload prediction area"),
                    shiny::actionButton(ns("delete_prediction_area"), "Delete Uploaded prediction area", style = "margin-bottom: 15px;")
      ),
      shiny::column(2)
    )
  )
}

#' Upload Module - Server
#'
#' Handles uploads for protocol CSV, model RDS, and geospatial gpkg files.
#' Provides status feedback and supports deleting uploaded geospatial data.
#'
#' @param id Module namespace ID
#' @return List of reactives for uploaded data objects:
#' \describe{
#'   \item{csv}{Reactive containing the protocol data frame or NULL}
#'   \item{model}{Reactive containing the loaded model object or NULL}
#'   \item{samples}{Reactive list from mod_gpkg_upload_server for sample points}
#'   \item{training_area}{Reactive list from mod_gpkg_upload_server for training area polygons}
#'   \item{prediction_area}{Reactive list from mod_gpkg_upload_server for prediction area polygons}
#' }
#' @noRd
mod_upload_server <- function(id, output_dir) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # to use dplyr:
    element <- NULL

    # Reactive for protocol CSV data
    csv_data <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$csv_upload, {
      shiny::req(input$csv_upload)
      tryCatch({
        df <- utils::read.csv(input$csv_upload$datapath) |>
          dplyr::mutate(element_id = normalize_id(element))
        csv_data(df)

        output$csv_status <- shiny::renderUI({
          shiny::tags$p("CSV loaded successfully", style = "color: blue;")
        })
      }, error = function(e) {
        output$csv_status <- shiny::renderUI({
          shiny::tags$p("Error loading CSV file", style = "color: red;")
        })
      })
    })

    # Reactive for model RDS object
    model_object <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$model_upload, {
      shiny::req(input$model_upload)
      tryCatch({
        obj <- readRDS(input$model_upload$datapath)
        model_object(obj)

        output$model_status <- shiny::renderUI({
          shiny::tags$p("Model loaded successfully", style = "color: blue;")
        })
      }, error = function(e) {
        output$model_status <- shiny::renderUI({
          shiny::tags$p("Error loading RDS file", style = "color: red;")
        })
      })
    })

    # Geospatial uploads via nested modules
    samples <- mod_gpkg_upload_server("samples", geom_types_expected = c("POINT", "MULTIPOINT"))
    training_area <- mod_gpkg_upload_server("training_area", geom_types_expected = c("POLYGON", "MULTIPOLYGON"))
    prediction_area <- mod_gpkg_upload_server("prediction_area", geom_types_expected = c("POLYGON", "MULTIPOLYGON"))

    # Delete buttons to clear geospatial uploads and reset UI
    shiny::observeEvent(input$delete_samples, {
      samples$data(NULL)
      shinyjs::reset("samples-upload")
      delete_plot_png("sampling_locations", output_dir)
      delete_plot_png("geodist_sampling_area", output_dir)
      delete_plot_png("geodist_prediction_area", output_dir)
      shiny::showNotification("Samples upload deleted.", type = "message")
    })


    shiny::observeEvent(input$delete_training_area, {
      training_area$data(NULL)
      shinyjs::reset("training_area-upload")
      delete_plot_png("sampling_area", output_dir)
      delete_plot_png("geodist_sampling_area", output_dir)
      shiny::showNotification("Training area upload deleted.", type = "message")
    })

    shiny::observeEvent(input$delete_prediction_area, {
      prediction_area$data(NULL)
      shinyjs::reset("prediction_area-upload")
      delete_plot_png("prediction_area", output_dir)
      delete_plot_png("geodist_prediction_area", output_dir)
      shiny::showNotification("Prediction area upload deleted.", type = "message")
    })

    # Return reactives for use outside module
    list(
      csv = csv_data,
      model = model_object,
      samples = samples,
      training_area = training_area,
      prediction_area = prediction_area
    )
  })
}

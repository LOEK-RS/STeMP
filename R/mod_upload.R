mod_upload_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    shinyjs::useShinyjs(),
    fluidRow(
      column(2),
      column(8,
             p(tags$b("(1) Upload STeMP protocol (.csv)")),
             fileInput(ns("csv_upload"), label = NULL, accept = c(".csv")),
             uiOutput(ns("csv_status")),
             tags$hr(),
             
             p(tags$b("(2) Upload model (.RDS)")),
             fileInput(ns("model_upload"), "Upload model object (.RDS)", accept = ".rds"),
             uiOutput(ns("model_status")),
             tags$hr(),
             
             p(tags$b("(3) Upload geospatial data (.gpkg)")),
             mod_gpkg_upload_ui(ns("samples"), label = "Upload sampling locations"),
             actionButton(ns("delete_samples"), "Delete Uploaded samples", style = "margin-bottom: 15px;"),
             mod_gpkg_upload_ui(ns("training_area"), label = "Upload training area"),
             actionButton(ns("delete_training_area"), "Delete Uploaded training area", style = "margin-bottom: 15px;"),
             mod_gpkg_upload_ui(ns("prediction_area"), label = "Upload prediction area"),
             actionButton(ns("delete_prediction_area"), "Delete Uploaded prediction area", style = "margin-bottom: 15px;"),
             
      ),
      column(2)
    )
  )
}


mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # CSV upload handling
    csv_data <- reactiveVal(NULL)
    
    observeEvent(input$csv_upload, {
      req(input$csv_upload)
      tryCatch({
        csv_data(read.csv(input$csv_upload$datapath) |> 
                   dplyr::mutate("element_id" = normalize_id(element)))
        output$csv_status <- renderUI({
          tags$p("CSV loaded successfully", style = "color: blue;")
        })
      }, error = function(e) {
        output$csv_status <- renderUI({
          tags$p("Error loading CSV file", style = "color: red;")
        })
      })
    })
    
    # RDS model upload handling
    model_object <- reactiveVal(NULL)
    observeEvent(input$model_upload, {
      req(input$model_upload)
      tryCatch({
        model_object(readRDS(input$model_upload$datapath))
        output$model_status <- renderUI({
          tags$p("Model loaded successfully", style = "color: blue;")
        })
      }, error = function(e) {
        output$model_status <- renderUI({
          tags$p("Error loading RDS file", style = "color: red;")
        })
      })
    })
    
    # Always instantiate all geospatial uploads
    samples <- mod_gpkg_upload_server("samples", geom_types_expected = c("POINT", "MULTIPOINT"))
    training_area <- mod_gpkg_upload_server("training_area", geom_types_expected = c("POLYGON", "MULTIPOLYGON"))
    prediction_area <- mod_gpkg_upload_server("prediction_area", geom_types_expected = c("POLYGON", "MULTIPOLYGON"))
    
    # Delete buttons:
    observeEvent(input$delete_samples, {
      samples$data(NULL)
      shinyjs::reset("samples-upload")
      showNotification("Samples upload deleted.", type = "message")
    })
    
    observeEvent(input$delete_training_area, {
      training_area$data(NULL)
      shinyjs::reset("training_area-upload")
      showNotification("Training area upload deleted.", type = "message")
    })
    
    observeEvent(input$delete_prediction_area, {
      prediction_area$data(NULL) 
      shinyjs::reset("prediction_area-upload")
      showNotification("Prediction area upload deleted.", type = "message")
    })
    
    # Return all reactive outputs
    return(list(
      csv = csv_data,
      model = model_object,
      samples = samples,
      training_area = training_area,
      prediction_area = prediction_area
    ))
  })
}

# UI: Upload only
mod_gpkg_upload_ui <- function(id, label = "Upload file") {
  ns <- NS(id)
  
  tagList(
    fileInput(ns("upload"), label, accept = c(".gpkg"))
  )
}

# Server: Upload only
mod_gpkg_upload_server <- function(id, geom_types_expected = c("POINT", "MULTIPOINT")) {
  moduleServer(id, function(input, output, session) {
    data <- reactiveVal(NULL)
    valid <- reactiveVal(FALSE)
    
    observeEvent(input$upload, {
      req(input$upload)
      valid(FALSE)
      data(NULL)
      
      sp_data <- tryCatch({
        st_read(input$upload$datapath, quiet = TRUE)
      }, error = function(e) {
        showNotification("Could not read .gpkg file.", type = "error")
        NULL
      })
      
      if (!is.null(sp_data)) {
        geom_type <- unique(st_geometry_type(sp_data))
        if (!all(geom_type %in% geom_types_expected)) {
          showNotification(paste0("Geometry must be one of: ", paste(geom_types_expected, collapse = ", ")), type = "error")
          valid(FALSE)
        } else {
          data(sp_data)
          valid(TRUE)
        }
      }
    })
    
    
    return(list(
      data = data,
      valid = valid
    ))
  })
}

# mod_prediction_panel.R

mod_prediction_panel_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    uiOutput(ns("prediction_collapse_ui"))
  )
}

mod_prediction_panel_server <- function(id, o_objective_1_val, protocol_data, geo_metadata = NULL,
                                        uploaded_values = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Validate geo metadata
    valid_geo_samples_metadata <- validate_geo_metadata(geo_metadata, "has_samples")
    valid_geo_prediction_area_metadata <- validate_geo_metadata(geo_metadata, "has_prediction_area")
    valid_geo_all_metadata <- validate_geo_metadata(geo_metadata, c("has_samples", "has_prediction_area"))
    
    prediction_data <- reactive({
      req(protocol_data())
      df <- protocol_data()
      df[df$section == "Prediction", ]
    })
    
    subsections <- reactive({
      unique(prediction_data()$subsection)
    })
    
    # Render geodist plot server side
    observe({
      req(o_objective_1_val() == "Model and prediction")
      df <- prediction_data()
      meta_geo_all_list <- valid_geo_all_metadata()
      if (is.null(meta_geo_all_list)) return()
      
      pid <- df$element_id[df$element_type == "geodist_plot_prediction"]
      if (length(pid) == 1) {
        render_geodist_plot_server(
          output = output,
          element_id = ns(pid),
          geo_metadata = meta_geo_all_list,
          objective = "Model and prediction"
        )
      }
    })
    
    output$prediction_collapse_ui <- renderUI({
      req(o_objective_1_val() == "Model and prediction")
      
      df <- prediction_data()
      subs <- subsections()
      
      if (nrow(df) == 0) {
        return(tags$p("No prediction data available"))
      }
      
      meta_geo_prediction_area_list <- valid_geo_prediction_area_metadata()
      if (is.null(meta_geo_prediction_area_list)) meta_geo_prediction_area_list <- list()
      
      uploaded_df <- uploaded_values()
      
      panels <- lapply(subs, function(subsec) {
        sub_df <- df[df$subsection == subsec, ]
        
        inputs <- lapply(seq_len(nrow(sub_df)), function(i) {
          row <- sub_df[i, ]
          
          # Override value with uploaded value if available
          if (!is.null(uploaded_df)) {
            uploaded_val <- uploaded_df$value[uploaded_df$element_id == row$element_id]
            if (length(uploaded_val) == 1 && !is.null(uploaded_val) && nzchar(uploaded_val)) {
              row$value <- uploaded_val
            }
          }
          
          if (row$element_type == "prediction_area_plot") {
            if (!is.null(valid_geo_prediction_area_metadata())) {
              render_prediction_area_plot(
                element_id = ns(row$element_id),
                element = row$element,
                geo_metadata = meta_geo_prediction_area_list,
                ns = ns
              )
            } else {
              NULL
            }
          } else if (row$element_type == "geodist_plot_prediction") {
            if (o_objective_1_val() == "Model and prediction" && !is.null(valid_geo_all_metadata())) {
              render_geodist_plot(
                element_id = ns(row$element_id),
                element = row$element,
                ns = ns
              )
            } else {
              NULL
            }
          } else {
            render_input_field(
              element_type = row$element_type,
              element_id = ns(row$element_id),
              label = row$element,
              o_objective_1 = o_objective_1_val(),
              suggestions = row$suggestions,
              info_text = row$info_text,
              geo_metadata = geo_metadata,
              ns = ns,
              row = row
            )
          }
        })
        
        bsCollapsePanel(title = subsec, do.call(tagList, inputs), style = "primary")
      })
      
      do.call(bsCollapse, panels)
    })
    
    # Server side for prediction_area_plot
    observe({
      req(o_objective_1_val() == "Model and prediction")
      df <- prediction_data()
      meta_geo_prediction_area_list <- valid_geo_prediction_area_metadata()
      if (is.null(meta_geo_prediction_area_list)) return()
      
      pid <- df$element_id[df$element_type == "prediction_area_plot"]
      if (length(pid) == 1) {
        render_prediction_area_plot_server(
          output = output,
          element_id = ns(pid),
          geo_metadata = meta_geo_prediction_area_list,
          what = "prediction_area_sf"
        )
      }
    })
    
    # Collect prediction input values reactively
    inputs_reactive <- reactive({
      df <- prediction_data()
      vals <- lapply(df$element_id, function(id) {
        val <- input[[id]]
        if (is.null(val) || (is.character(val) && val == "")) {
          NA
        } else {
          val
        }
      })
      
      data.frame(
        section = df$section,
        subsection = df$subsection,
        element = df$element,
        value = unlist(vals, use.names = FALSE),
        stringsAsFactors = FALSE
      )
    })
    
    return(list(
      prediction_inputs = reactive(inputs_reactive())
    ))
  })
}

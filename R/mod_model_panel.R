# mod_model_panel.R
# Module to render inputs grouped by subsections inside collapsible panels
# Supports optional automatic filling from model_metadata reactive input

mod_model_panel_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    uiOutput(ns("model_collapse_ui")),  # container for the full collapsible UI
  )
}


mod_model_panel_server <- function(id, protocol_data, model_metadata = NULL, geo_metadata = NULL, o_objective_1_val,
                                   geodist_sel = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Update sampling_design selectInput only if geodist_sel() is not NULL
    observe({
      selected_val <- geodist_sel()
      if (!is.null(selected_val)) {
        updateSelectInput(session, "sampling_design", selected = selected_val)
      }
    })
    
    valid_model_metadata <- validate_model_metadata(model_metadata, "has_model")
    valid_geo_samples_metadata <- validate_geo_metadata(geo_metadata, "has_samples")
    valid_geo_training_area_metadata <- validate_geo_metadata(geo_metadata, "has_training_area")
    valid_geo_all_metadata <- validate_geo_metadata(geo_metadata, c("has_samples", "has_training_area"))
    
    model_data <- reactive({
      req(protocol_data())
      df <- protocol_data()
      df[df$section == "Model", ]
    })
    
    subsections <- reactive({
      unique(model_data()$subsection)
    })
    
    
    output$model_collapse_ui <- renderUI({
      df <- model_data()
      subs <- subsections()
      
      if (nrow(df) == 0) {
        return(tags$p("No model data available"))
      }
      
      meta_model_list <- valid_model_metadata()
      if (is.null(meta_model_list)) meta_model_list <- list()
      meta_geo_samples_list <- valid_geo_samples_metadata()
      if (is.null(meta_geo_samples_list)) meta_geo_samples_list <- list()
      meta_geo_training_area_list <- valid_geo_training_area_metadata()
      if (is.null(meta_geo_training_area_list)) meta_geo_training_area_list <- list()
      
      panels <- lapply(subs, function(subsec) {
        sub_df <- df[df$subsection == subsec, ]
        
        inputs <- lapply(seq_len(nrow(sub_df)), function(i) {
          row <- sub_df[i, ]
          
          if (row$element_type == "sample_plot") {
            if(!is.null(valid_geo_samples_metadata())) {
            render_samples_plot(
              element_id = ns(row$element_id),
              element = row$element,
              geo_metadata = meta_geo_samples_list,
              ns = ns
            ) } else {
              NULL
            }
            
          } else if (row$element_type == "training_area_plot") {
            if(!is.null(valid_geo_training_area_metadata())) {
              render_training_area_plot(
                element_id = ns(row$element_id),
                element = row$element,
                geo_metadata = meta_geo_training_area_list,
                ns = ns
              )
            } else {
              NULL
            }
            
          } else if (row$element_type == "geodist_plot_training") {
            if (o_objective_1_val() == "Model only" && !is.null(valid_geo_all_metadata())) {
              render_geodist_plot(
                element_id = ns(row$element_id),
                element = row$element,
                ns = ns
              )
            } else {
              NULL
            }
            
          } else if (row$element_type == "sampling_design") {
            selected_val <- NULL
            # Only call geodist_sel() here, and wrap in try()
            try({
              if (!is.null(geodist_sel())) {
                selected_val <- geodist_sel()
              }
            }, silent = TRUE)
            render_design(
              element_id = ns(row$element_id),
              element = row$element,
              selected = selected_val,
              info_text = row$info_text
            )
            
          } else {
            render_input_field(
              element_type = row$element_type,
              element_id = ns(row$element_id),
              label = row$element,
              o_objective_1 = o_objective_1_val(),
              suggestions = row$suggestions,
              info_text = row$info_text,
              model_metadata = meta_model_list,
              geo_metadata = meta_geo_samples_list
            )
          }
        })
        
        bsCollapsePanel(title = subsec, do.call(tagList, inputs), style = "primary")
      })
      
      do.call(bsCollapse, panels)
    })
    
    # Server-side render for samples plot
    observe({
      df <- model_data()
      meta_geo_samples_list <- valid_geo_samples_metadata()
      if (is.null(meta_geo_samples_list)) meta_geo_samples_list <- list()
      
      sample_plot_ids <- df$element_id[df$element_type == "sample_plot"]
      
      #message("🧪 sample_plot_ids: ", ns(sample_plot_ids))
      render_samples_plot_server(output, ns(sample_plot_ids), meta_geo_samples_list, what = "samples_sf")
    })
    
    # Server-side render for training area plot
    observe({
      df <- model_data()
      meta_geo_training_area_list <- valid_geo_training_area_metadata()
      if (is.null(meta_geo_training_area_list)) meta_geo_training_area_list <- list()
      
      training_area_plot_ids <- df$element_id[df$element_type == "training_area_plot"]
      
      #message("🧪 training_area_plot_ids: ", ns(training_area_plot_ids))
      render_training_area_plot_server(output, ns(training_area_plot_ids), meta_geo_training_area_list, what = "training_area_sf")
    })
    
    # Server-side render for geodist plot (if o_objective_1_val is "Model only")
    observe({
      req(o_objective_1_val())
      if (o_objective_1_val() == "Model only") {
        df <- model_data()
        meta_geo_all_list <- valid_geo_all_metadata()
        if (is.null(meta_geo_all_list)) meta_geo_all_list <- list()

        pid <- df$element_id[df$element_type == "geodist_plot_training"]
        if (length(pid) == 1) {
          #message("🗺️ Rendering geodist plot for: ", ns(pid))
          render_geodist_plot_server(
            output = output,
            element_id = ns(pid),
            geo_metadata = meta_geo_all_list,
            objective = "Model only"
          )
        }
      }
    })
    
    
    # Integrate render_design_server for all sampling_design inputs to update them reactively
    observe({
      req(model_data())
      df <- model_data()
      design_id <- df$element_id[df$element_type == "sampling_design"]
      
      if (length(design_id) == 1) {
        render_design_server(
          input = input,
          output = output,
          session = session,
          element_id = design_id,
          geodist_sel = geodist_sel
        )
      }
    })
    
    
    # Outputs
    inputs_reactive <- reactive({
      df <- model_data()
      vals <- lapply(df$element_id, function(id) input[[ns(id)]])
      names(vals) <- df$element_id
      vals
    })
    
    validation_method <- reactive({
      input[["m_validation_1"]]
    })
    
    sampling_design <- reactive({
      input[["d_response_11"]]
    })
    
    uncertainty_quantification <- reactive({
      input[["p_eval_4"]]
    })
    
    predictor_types <- reactive({
      input[["d_predictors_1"]]
    })
    
    return(list(
      "model_inputs" = inputs_reactive,
      "validation_method" = validation_method,
      "sampling_design" = sampling_design,
      "uncertainty_quantification" = uncertainty_quantification,
      "predictor_types" = predictor_types
    ))
  })
}


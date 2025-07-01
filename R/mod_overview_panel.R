mod_overview_panel_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("overview_ui")),  # dynamic UI from CSV
    radioButtons(ns("o_objective_1"), "Objective:",
                 choices = c("Model and prediction", "Model only"),
                 selected = "Model and prediction")
  )
}


mod_overview_panel_server <- function(id, protocol_data, uploaded_values = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    overview_data <- reactive({
      df <- protocol_data()
      req(df)
      df[df$section == "Overview", ]
    })
    
    output$overview_ui <- renderUI({
      df <- overview_data()
      req(nrow(df) > 0)
      
      # Get uploaded values data.frame (element_id, value) or NULL
      uploaded_df <- uploaded_values()
      
      ui_list <- lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]
        if (!is.null(uploaded_df) && all(c("element_id", "value") %in% names(uploaded_df))) {
          uploaded_val <- uploaded_df$value[uploaded_df$element_id == row$element_id]
          if (length(uploaded_val) == 1 && !is.null(uploaded_val) && nzchar(uploaded_val)) {
            row$value <- uploaded_val
          }
        }
        render_input_field(
          element_type = row$element_type,
          element_id = ns(row$element_id),
          label = row$element,
          o_objective_1 = input$o_objective_1,
          suggestions = row$suggestions,
          info_text = row$info_text,
          model_metadata = NULL,
          geo_metadata = NULL,
          ns = ns,
          row = row
        )
      })
      
      do.call(tagList, ui_list)
    })
    
    inputs_reactive <- reactive({
      df <- overview_data()
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
      o_objective_1 = reactive(input$o_objective_1),
      overview_inputs = inputs_reactive
    ))
  })
}

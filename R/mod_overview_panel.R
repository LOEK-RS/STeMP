mod_overview_panel_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("overview_ui")),  # dynamic UI from CSV
    radioButtons(ns("o_objective_1"), "Objective:",
                 choices = c("Model and prediction", "Model only"),
                 selected = "Model and prediction")
  )
}


mod_overview_panel_server <- function(id, protocol_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Load CSV once and filter for Overview section
    overview_data <- reactive({
      df <- protocol_data()
      df[df$section == "Overview", ]
    })
    
    # 2. Render UI dynamically using helper functions
    output$overview_ui <- renderUI({
      df <- overview_data()
      req(nrow(df) > 0)
      
      ui_list <- lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]
        render_input_field(
          element_type = row$element_type,
          element_id = ns(row$element_id),
          label = row$element,
          suggestions = row$suggestions,
          info_text = row$info_text 
        )
      })
      
      
      do.call(tagList, ui_list)
    })
    
    # 3. Return all input values from dynamic inputs
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

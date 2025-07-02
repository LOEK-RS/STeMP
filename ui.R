#' User Interface for the STeMP Application
#'
#' Defines the main UI layout including CSS, JavaScript,
#' navigation bar with tabs, and embedded module UIs.
#'
#' @return A Shiny UI definition object.
#' @export
ui <- tagList(
  # Include custom CSS and JavaScript in the head tag
  tags$head(
    # Load external CSS file for styling
    tags$style(HTML(readChar("www/stemp.css", file.info("www/stemp.css")$size))),
    
    # Placeholder for tooltip CSS (can be extended)
    tags$style(HTML("/* tooltip CSS placeholder */")),
    
    # Enable Bootstrap tooltips on elements with data-toggle="tooltip"
    tags$script(HTML("
      $(document).ready(function() {
        $('[data-toggle=\"tooltip\"]').tooltip({ container: 'body' });
       });
    ")),
    
    # Custom CSS styles for info icons and hover effects
    tags$style(HTML("
      .info-hover-icon {
        margin-left: 5px;
        color: #007BFF;
        cursor: pointer;
      }
    ")),
    tags$style(HTML("
      .info-label-icon {
        display: inline-flex;
        align-items: center;
        gap: 6px;
      }
      .info-hover-icon {
        display: none;
        color: #007BFF;
        margin-left: 4px;
      }
      .input-label-icon:hover .info-hover-icon {
        display: inline;
      }
    "))
  ),
  
  # Enable shinyjs for enhanced interactivity
  useShinyjs(),
  
  # UI module for displaying warnings
  mod_warnings_ui("warnings"),
  
  # Main navigation bar with multiple tabs
  navbarPage(
    id = "navbar",
    windowTitle = "STeMP v0.9",
    title = div(
      div(
        id = "github_logo", 
        a(img(src = "github_logo_40px.png"), href = "https://github.com/LOEK-RS/STeMP", target = "_blank")
      ),
      "STeMP v0.9"
    ),
    position = "fixed-top",
    theme = shinytheme("united"),
    selected = "about",
    
    # About tab explaining what STeMP is
    tabPanel("What is STeMP?", value = "about", mod_about_ui("about")),
    
    # How-to-use tab with instructions
    tabPanel("How to use this app", value = "howto", mod_howto_ui("howto")),
    
    # Create protocol tab with sidebar layout
    tabPanel("Create a protocol", value = "create", 
             sidebarLayout(
               sidebarPanel(
                 style = "position:fixed; width: 16%;",
                 width = 2,
                 mod_sidebar_ui("sidebar")
               ),
               mainPanel(
                 mod_create_protocol_ui("protocol")
               )
             )
    ),
    
    # Protocol viewer tab
    tabPanel("Protocol viewer", value = "viewer", mod_viewer_ui("viewer")),
    
    # Upload/import data tab
    tabPanel("Upload / Import", value = "import", mod_upload_ui("upload"))
  )
)



ui <- tagList(
  tags$head(
    tags$style(HTML(readChar("www/stemp.css", file.info("www/stemp.css")$size))),
    tags$style(HTML("/* tooltip CSS placeholder */")),
    tags$script(HTML("
      $(document).ready(function () {
        $('[data-toggle=\"tooltip\"]').tooltip();
      });
    ")),
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
  useShinyjs(),
  
  navbarPage(
    id = "navbar",
    windowTitle = "STeMP v0.9",
    title = div(
      div(id = "github_logo", 
          a(img(src = "github_logo_40px.png"), href = "https://github.com/LOEK-RS/STeMP", target = "_blank")),
      "STeMP v0.9"
    ),
    position = "fixed-top",
    theme = shinytheme("united"),
    selected = "about",
    
    tabPanel("What is STeMP?", value = "about", mod_about_ui("about")),
    
    tabPanel("How to use this app", value = "howto", mod_howto_ui("howto")),
    
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
            
    )),
    
    tabPanel("Protocol viewer", value = "viewer", mod_viewer_ui("viewer")),
    
    tabPanel("Upload / Import", value = "import", mod_upload_ui("upload")),
  )
)

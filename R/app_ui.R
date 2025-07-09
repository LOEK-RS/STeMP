# R/app_ui.R

#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}` (do not remove).
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # add external resources (CSS/JS, favicon, etc.)
    golem_add_external_resources(),

    # now your UI
    navbarPage(
      id = "navbar",
      windowTitle = "STeMP v0.9",
      title = div(
        div(
          id = "github_logo",
          a(img(src = "www/github_logo_40px.png", height = "40px"),
            href = "https://github.com/LOEK-RS/STeMP",
            target = "_blank"
          )
        ),
        "STeMP v0.9"
      ),
      position = "fixed-top",
      theme = shinythemes::shinytheme("united"),
      selected = "about",

      # About
      tabPanel("What is STeMP?", value = "about", mod_about_ui("about")),

      # How-to
      tabPanel("How to use this app", value = "howto", mod_howto_ui("howto")),

      # Create protocol
      tabPanel(
        "Create a protocol", value = "create",
        sidebarLayout(
          sidebarPanel(
            style = "position:fixed; width: 16%;", width = 2,
            mod_sidebar_ui("sidebar")
          ),
          mainPanel(
            mod_create_protocol_ui("protocol")
          )
        )
      ),

      # Viewer
      tabPanel("Protocol viewer", value = "viewer", mod_viewer_ui("viewer")),

      # Upload/Import
      tabPanel("Upload / Import", value = "import", mod_upload_ui("upload"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This is internally used to add CSS/JS, favicon, etc.
#' @import shiny
#' @importFrom golem add_resource_path favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "STeMP"
    ),

    # embedded CSS from www/stemp.css
    tags$style(
      HTML(
        readChar(
          app_sys("app/www/stemp.css"),
          file.info(app_sys("app/www/stemp.css"))$size
        )
      )
    ),

    # tooltip CSS placeholder
    tags$style(HTML("/* tooltip CSS placeholder */")),

    # enable Bootstrap tooltips
    tags$script(HTML("
      $(document).ready(function() {
        $('[data-toggle=\"tooltip\"]').tooltip({ container: 'body' });
      });
    ")),

    # info-icon hover styles
    tags$style(HTML("
      .info-hover-icon { margin-left:5px; color:#007BFF; cursor:pointer; display:none; }
      .input-label-icon:hover .info-hover-icon { display:inline; }
      .info-label-icon { display:inline-flex; align-items:center; gap:6px; }
    "))
  )
}

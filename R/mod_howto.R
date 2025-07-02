#' How-To UI Module
#'
#' Displays instructions on how to create an STeMP protocol with helpful links.
#'
#' @param id Module namespace ID
#' @return UI elements with informational text and links
mod_howto_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(2),  # Left spacer column
      column(8,
             strong(
               p("How to create an STeMP protocol", 
                 style = "padding-top: 10px; font-size: 30px; font-weight: bold;")
             ),
             p("Enter all relevant information into the fields provided under 'Create a protocol'. Your progress in individual sections is displayed in the 
             side bar at the left. The 'Hide optional fields' switch on the left allows you to only display mandatory fields, which depend on the model objective chosen.
             A preview of your current protocol is available in the 'Protocol Viewer'.", 
               style = "font-size: 18px;"),
             
             p("You can always save your progress by clicking the download button on the left. We recommend always downloading the csv file as this will allow you to resume your work later. After downloading your protocol as csv, it is safe to close the Shiny app. For a better layout, you can also download the ODMAP protocol as word document. Please note that word documents cannot be uploaded again to the Shiny app.", 
               style = "font-size: 18px;"),
             
             p("You will be able to resume working on your protocol by choosing the Upload tab above and uploading your previously saved STeMP csv file. 
             In addition, you can import objects generated with the ", 
               a(href = 'https://cran.r-project.org/web/packages/rangeModelMetadata/index.html', 
                 'rangeModelsMetaData', target = "_blank", .noWS = "outside"),
               " R-package to autofill your STeMP protocol.", 
               style = "font-size: 18px;"),
             
             p("Should you have any conceptual questions or feedback regarding the STeMP protocol, or any technical questions and feedback regarding the Shiny app, please do not hesitate to visit our ", 
               a(href = 'https://github.com/LOEK-RS/STeMP', 
                 'Github repository', target = "_blank", .noWS = "outside"), 
               " or contact us directly at ", 
               a(href = 'mailto:', '', .noWS = "outside"), ".", 
               style = "font-size: 18px;")
      ),
      column(2)  # Right spacer column
    )
  )
}

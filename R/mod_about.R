mod_about_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(2),
      column(8, 
             p("STeMP", style = "padding-top: 10px; font-size: 30px; font-weight:bold;"),
             p("Spatio-temporal modelling is a key method in the geosciences. Recently, the increasing availability of large datasets and
                 new methods gave rise to the application of methods of machine-learning in spatio-temporal modelling.
                 However, these models are often not sufficiently reported, and reproducing them often remains challenging.",
               style= "font-size: 18px;"),
             p("Reproducibility and transparency of spatio-temporal models are key for
                 interpreting, evaluating and reproducing them. This can be achieved by standardized model protocols.
                 While there exist such protocols for machine-learning models in general (e.g., Model Cards, REFORMs), as well as for specific domains 
                 like species distribution modelling (ODMAP), to date such protocols are lacking in the general field of spatio-temporal modelling.", 
               style= "font-size: 18px;"), 
             img(src = "workflow.png", width = "60%", style="display: block; margin-left: auto; margin-right: auto; min-width: 500px;"), br(),
             p("We propose a protocol for spatio-temporal models to fill this gap: STeMP. This protocol contains three major sections.
                  To facilitate community engagement, the protocol is hosted on", a("GitHub", href = "https://github.com/LOEK-RS/STeMP", target = "_blank"), ". Any missing points or other feedback can be 
                  submitted as a pull request, and the community can vote if it should be included in the protocol.",
               style= "font-size: 18px;"),
             p("This Shiny web application helps to implement ", style= "font-size: 18px;"),
             em(p("Please cite as follows:", style = "font-size: 18px;")),
             p("", style= "font-size: 18px;"),
      ),
      column(2)
    )
  )
}
  
  
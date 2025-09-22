#' UI Module for About Page of STeMP Application
#'
#' Provides an informative UI for the STeMP protocol and application,
#' explaining its purpose, motivation, and community engagement.
#'
#' @param id A character string specifying the module's namespace ID.
#'
#' @return A Shiny UI element (fluidPage) for the About page.
#' @noRd
mod_about_ui <- function(id) {
	ns <- shiny::NS(id)

	shiny::fluidPage(
		shiny::fluidRow(
			shiny::column(2),
			shiny::column(
				8,
				shiny::p("STeMP", style = "padding-top: 10px; font-size: 30px; font-weight: bold;"),

				shiny::p(
					"Spatio-temporal modelling is a key method in the geosciences. Recently, the increasing availability of large datasets and
          new methods gave rise to the application of methods of machine-learning in spatio-temporal modelling.
          However, these models are often not sufficiently reported, and reproducing them often remains challenging.",
					style = "font-size: 18px;"
				),

				shiny::p(
					"Reproducibility and transparency of spatio-temporal models are key for
          interpreting, evaluating and reproducing them. This can be achieved by standardized model protocols.
          While there exist such protocols for machine-learning models in general (e.g., Model Cards, REFORMs), as well as for specific domains
          like species distribution modelling (ODMAP), to date such protocols are lacking in the general field of spatio-temporal modelling.",
					style = "font-size: 18px;"
				),

				shiny::img(
					src = "www/workflow.png",
					width = "60%",
					style = "display: block; margin-left: auto; margin-right: auto; min-width: 500px;"
				),
				shiny::br(),

				shiny::p(
					"We propose a protocol for spatio-temporal models to fill this gap: STeMP. This protocol contains three major sections.
          To facilitate community engagement, the protocol is hosted on ",
					shiny::a("GitHub", href = "https://github.com/LOEK-RS/STeMP", target = "_blank"),
					". Any missing points or other feedback can be
          submitted as a pull request, and the community can vote if it should be included in the protocol.",
					style = "font-size: 18px;"
				),

				shiny::p("This Shiny web application helps to implement ", style = "font-size: 18px;"),

				shiny::em(
					shiny::p("Please cite as follows:", style = "font-size: 18px;")
				),

				shiny::p("", style = "font-size: 18px;")
			),
			shiny::column(2)
		)
	)
}

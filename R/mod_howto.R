#' How-To UI Module
#'
#' Displays instructions on how to create an STeMP protocol with helpful links.
#'
#' @param id Module namespace ID
#' @return UI elements with informational text and links
#' @noRd
mod_howto_ui <- function(id) {
	ns <- shiny::NS(id)

	shiny::fluidPage(
		shiny::fluidRow(
			shiny::column(2), # Left spacer column
			shiny::column(
				8,
				shiny::strong(
					shiny::p("How to create an STeMP protocol", style = "padding-top: 10px; font-size: 30px; font-weight: bold;")
				),
				shiny::p(
					"Enter all relevant information into the fields provided under ",
					shiny::em("Create a protocol"),
					".
                    Progress bars for the overall, as well as the progress made in individual sections is shown in the sidebar.
                    The ",
					shiny::em("Hide optional fields"),
					" switch on the left allows you to only display mandatory fields, 
                    which depend on the model objective chosen. A preview of your current protocol is available in the ",
					shiny::em("Protocol viewer"),
					".",
					style = "font-size: 18px;"
				),

				shiny::p(
					"You can always save your progress by clicking the ",
					shiny::em("download"),
					" button on the left. 
                    It is possible to download a .csv table containing the elements and their values, as well as a .pdf file containing the 
                    table and the figures, or just the figures as .zip folder",
					style = "font-size: 18px;"
				),

				shiny::p(
					"It is possible to resume your work by uploading your previously saved STeMP .csv file in the ",
					shiny::em("Upload/Import"),
					" tab. There, you can also upload model objects (currently only .RDS files) to automatically
                    fill some of the protocol fields based on the model metadata. Also, it is possible to upload the sampling locations,
                    the training area and the prediction area as .gpkg files and generate plots from them. If sampling locations and the prediction
                    area (if the objective is ",
					shiny::em("Model and prediction"),
					"), or sampling locations (if the objective is ",
					shiny::em("Model only"),
					") are uploaded, the ",
					shiny::em("sampling design"),
					"is automatically calculated 
                    and a geodistance plot is generated.",
					style = "font-size: 18px;"
				),

				shiny::p(
					"For questions, reporting bugs, or proposing/discussing new features/elements, please visit the Github page
                     of the protocol ",
					shiny::a(
						href = 'https://github.com/LOEK-RS/STeMP',
						"https://github.com/LOEK-RS/STeMP",
						target = "_blank",
						.noWS = "outside"
					),
					" or contact us directly at ",
					shiny::a(href = 'mailto:', 'jan.linnenbrink@uni-muenster.de', .noWS = "outside"),
					".",
					style = "font-size: 18px;"
				)
			),
			shiny::column(2) # Right spacer column
		)
	)
}

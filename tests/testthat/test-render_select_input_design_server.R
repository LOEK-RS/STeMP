test_that("resolve_design_value prefers derived by default when both present", {
	expect_equal(
		resolve_design_value(uploaded_value = "stratified", derived_value = "random"),
		"random"
	)
})

test_that("resolve_design_value prefers uploaded when prefer_uploaded is TRUE", {
	expect_equal(
		resolve_design_value(uploaded_value = "stratified", derived_value = "random", prefer_uploaded = TRUE),
		"stratified"
	)
})

test_that("resolve_design_value ignores prefer_uploaded when only one source present", {
	expect_equal(resolve_design_value(NULL, "random", prefer_uploaded = TRUE), "random")
	expect_equal(resolve_design_value("stratified", NULL, prefer_uploaded = FALSE), "stratified")
	expect_null(resolve_design_value(NULL, NULL, prefer_uploaded = TRUE))
})

test_that("render_select_input_design_server last-upload-wins ordering", {
	update_calls <- list()

	local_mocked_bindings(
		updateSelectInput = function(session, inputId, selected, ...) {
			update_calls[[length(update_calls) + 1L]] <<- list(
				inputId = inputId,
				selected = selected
			)
		},
		.package = "shiny"
	)

	local_mocked_bindings(
		delay = function(ms, expr) force(expr),
		.package = "shinyjs"
	)

	geodist_sel <- shiny::reactiveVal(NULL)
	uploaded_value <- shiny::reactiveVal(NULL)

	shiny::testServer(
		function(input, output, session) {
			render_select_input_design_server(
				input = input,
				session = session,
				element_id = shiny::reactive("sampling_design"),
				geodist_sel = geodist_sel,
				uploaded_value = uploaded_value
			)
		},
		expr = {
			# Geodata first: geodata wins
			geodist_sel("random")

			session$setInputs(ui_rendered = 1)

			expect_equal(
				update_calls[[length(update_calls)]]$selected,
				"random"
			)

			# CSV uploaded after geodata: CSV wins
			update_calls <<- list()

			uploaded_value("stratified")

			session$setInputs(ui_rendered = 2)

			expect_equal(
				update_calls[[length(update_calls)]]$selected,
				"stratified"
			)

			# Geodata re-uploaded after CSV: geodata wins again
			update_calls <<- list()

			geodist_sel("clustered")

			session$setInputs(ui_rendered = 3)

			expect_equal(
				update_calls[[length(update_calls)]]$selected,
				"clustered"
			)

			# Geodata removed, CSV still present: CSV takes over
			update_calls <<- list()

			geodist_sel(NULL)

			session$setInputs(ui_rendered = 4)

			expect_equal(
				update_calls[[length(update_calls)]]$selected,
				"stratified"
			)

			# CSV removed, no geodata: field clears
			update_calls <<- list()

			uploaded_value(NULL)

			session$setInputs(ui_rendered = 5)

			expect_equal(
				update_calls[[length(update_calls)]]$selected,
				""
			)

			# New geodata: geodata fills field
			update_calls <<- list()

			geodist_sel("random")

			session$setInputs(ui_rendered = 6)

			expect_equal(
				update_calls[[length(update_calls)]]$selected,
				"random"
			)

			# CSV uploaded last: CSV wins
			update_calls <<- list()

			uploaded_value("clustered")

			session$setInputs(ui_rendered = 7)

			expect_equal(
				update_calls[[length(update_calls)]]$selected,
				"clustered"
			)
		}
	)
})

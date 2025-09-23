test_that("notification appears for clustered + random CV", {
	skip_if_not_installed("shinytest2")
	skip_if(Sys.getenv("RUN_UI_TESTS") != "true")

	app <- shinytest2::AppDriver$new(
		app_dir = ".",
		variant = shinytest2::platform_variant(),
		seed = 123
	)
	on.exit(app$stop(), add = TRUE)

	app$set_inputs(`protocol-model-sampling_design` = "clustered")
	app$set_inputs(`protocol-model-validation_strategy` = "Random Cross-Validation")

	app$wait_for_js("document.querySelectorAll('.shiny-notification').length > 0")
	txt <- app$get_js("document.querySelector('.shiny-notification .shiny-notification-content').innerText")

	expect_match(txt, "optimistic", ignore.case = TRUE)
})

test_that("clustered + random CV triggers warning flag", {
	testServer(
		mod_warnings_server,
		args = list(
			sampling_design = reactive("clustered"),
			validation_method = reactive("Random Cross-Validation"),
			evaluation_method = reactive("Random Cross-Validation"),
			uncertainty_quantification = reactive("None"),
			predictor_types = reactive(character(0)),
			show_warnings = reactive(TRUE)
		),
		{
			# Run observers
			session$flushReact()

			expect_true(!is.null(warning_flags$clustered_random_cv))
		}
	)
})

test_that("random + spatial CV triggers warning flag", {
	testServer(
		mod_warnings_server,
		args = list(
			sampling_design = reactive("random"),
			validation_method = reactive("Spatial Cross-Validation"),
			evaluation_method = reactive("Spatial Cross-Validation"),
			uncertainty_quantification = reactive("None"),
			predictor_types = reactive(character(0)),
			show_warnings = reactive(TRUE)
		),
		{
			session$flushReact()
			expect_true(!is.null(warning_flags$random_clustered_cv))
		}
	)
})

test_that("clustered + spatial proxies triggers warning flag", {
	testServer(
		mod_warnings_server,
		args = list(
			sampling_design = reactive("clustered"),
			validation_method = reactive("Random Cross-Validation"),
			evaluation_method = reactive("Random Cross-Validation"),
			uncertainty_quantification = reactive("None"),
			predictor_types = reactive(c("Spatial Proxies")),
			show_warnings = reactive(TRUE)
		),
		{
			session$flushReact()
			expect_true(!is.null(warning_flags$clustered_proxies))
		}
	)
})

test_that("clustered + no uncertainty quantification triggers warning flag", {
	testServer(
		mod_warnings_server,
		args = list(
			sampling_design = reactive("clustered"),
			validation_method = reactive("Spatial Cross-Validation"),
			evaluation_method = reactive("Spatial Cross-Validation"),
			uncertainty_quantification = reactive("None"),
			predictor_types = reactive(character(0)),
			show_warnings = reactive(TRUE)
		),
		{
			session$flushReact()
			expect_true(!is.null(warning_flags$clustered_noAssessment))
		}
	)
})

test_that("Warnings raised for inapporpriate evaluation strategy", {
	testServer(
		mod_warnings_server,
		args = list(
			sampling_design = reactive("random"),
			validation_method = reactive("Spatial Cross-Validation"),
			evaluation_method = reactive("Spatial Cross-Validation"),
			uncertainty_quantification = reactive("None"),
			predictor_types = reactive(character(0)),
			show_warnings = reactive(TRUE)
		),
		{
			session$flushReact()
			expect_true(!is.null(warning_flags$random_clustered_ev))
		}
	)
})

test_that("CV for model selection and final prediction assessment triggers warning", {
	testServer(
		mod_warnings_server,
		args = list(
			sampling_design = reactive("clustered"),
			validation_method = reactive("Spatial Cross-Validation"),
			evaluation_method = reactive("Spatial Cross-Validation"),
			uncertainty_quantification = reactive("None"),
			predictor_types = reactive(character(0)),
			show_warnings = reactive(TRUE)
		),
		{
			session$flushReact()
			expect_true(!is.null(warning_flags$both_cv))
		}
	)
})

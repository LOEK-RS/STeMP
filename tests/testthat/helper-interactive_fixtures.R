# Inspection helpers for htmlwidget objects.
#
# leaflet stores its layers as positional arguments in `m$x$calls`, and the
# positions have moved between releases. Asserting on the strings the widget
# carries is stable across those changes and is what the popups actually are.

collect_strings <- function(x) {
	if (is.character(x)) {
		return(x)
	}
	if (is.list(x)) {
		return(unlist(lapply(x, collect_strings), use.names = FALSE))
	}
	character(0)
}

leaflet_methods <- function(m) {
	vapply(m$x$calls, function(call) call$method, character(1))
}

widget_strings <- function(m) collect_strings(m$x$calls)

# Two observations at one location, one at another, three distinct timestamps.
interactive_samples <- function() {
	geo_points(
		c(7, 7, 9),
		c(51, 51, 52),
		time = as.POSIXct(c("2020-01-01", "2020-01-08", "2020-01-15"), tz = "UTC")
	)
}

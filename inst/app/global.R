#' Global script for STeMP Shiny app
#'
#' Loads required packages, sources utility functions,
#' and defines global variables used across the app.
#'
#' @import shiny shinyjs shinyWidgets shinythemes shinydashboard shinyBS ggplot2 sf CAST DT bslib
#' @export

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(shinyBS)
library(ggplot2)
library(sf)
library(CAST)
library(DT)
library(bslib)

# Source utility functions for app-wide use
# source("R/utils.R")

# Path to the protocol CSV dictionary used in the app
csv_path <- "www/stemp_dict.csv"

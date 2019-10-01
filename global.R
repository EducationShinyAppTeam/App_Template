library(shiny)
library(shinydashboard)

# Define modules object
modules <- reactiveValues()
namespace <- "app"
ns <- NS(namespace)

# Load app pages
pages <- lapply(list.files("pages", full.names = TRUE), source)
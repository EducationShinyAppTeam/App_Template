library(shiny)
library(shinydashboard)

library(boastUtils)

#' Define set namespace and define modules object
#' @seealso \link{https://shiny.rstudio.com/articles/modules.html}
namespace <- "app"
ns        <- NS(namespace)
modules   <- reactiveValues()

#' Load app pages
pages <- lapply(list.files("pages", full.names = TRUE), source)
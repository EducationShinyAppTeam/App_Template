library(shiny)
library(shinydashboard)

# @todo
pages <- list.files("pages", full.names = TRUE)
lapply(pages, source)
# @endtodo

# Define UI for App
ui <- list(
  includeCSS("https://educationshinyappteam.github.io/Style_Guide/theme/style.css"),
  dashboardPage(
    skin = "blue",
    dashboardHeader(
      title = "APP_TITLE",
      tags$li(class = "dropdown", tags$a(href='https://shinyapps.science.psu.edu/', icon("home"))),
      tags$li(class = "dropdown", actionLink("info", icon("info")))
    ),
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
        menuItem("Explore", tabName = "Explore", icon = icon("wpexplorer")), 
      )
    ),
    dashboardBody(
      tabItems(
        Overview,
      )
    )
  )
)

# Define server logic
server <- function(input, output) {

}

# Create Shiny App
shinyApp(ui = ui, server = server)
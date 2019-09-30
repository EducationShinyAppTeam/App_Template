library(shiny)

# Define UI for App
ui <- fluidPage(
  includeCSS("https://educationshinyappteam.github.io/Style_Guide/css/style.css"),
  headerPanel("New Application")
)

# Define server logic
server <- function(input, output) {

}

# Create Shiny App
shinyApp(ui = ui, server = server)
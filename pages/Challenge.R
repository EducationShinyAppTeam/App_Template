#' Shiny dashboard challenge tab item
Challenge <- tabItem(
  tabName = "Challenge",
  titlePanel("PAGE_TITLE"),
  sidebarLayout(
    sidebarPanel(
      tagList(
        sliderInput(ns("obs"), "Number of observations", 0, 1000, 500)
      )
    ),
    mainPanel(
      uiOutput(ns("ui"))
    )
  )
)

#' Server logic for challenge page
challenge <- function(input, output, session) {
  observeEvent(input$obs, {
    output$plot <- renderPlot({
      #' Use isolate() to avoid dependency on input$obs
      dist <- isolate(rnorm(input$obs))
      hist(dist)
    })
  })
  
  output$ui <- renderUI(plotOutput(ns("plot")))
}

#' Export module
modules$challenge <- challenge
#' Overview tab content
Overview <- tabItem(tabName = "Overview",
  h2("About"),
  p("APP_DESCRIPTION"),
  h2("Instructions"),
  tags$ol(
    tags$li("STEP_1"),
    tags$li("STEP_2"),
    tags$li("STEP_3"),
    tags$li("STEP_4")
  ),
  h2("Acknowledgements"),
  p("DEVELOPED_BY"),
  p("SPECIAL_THANKS")
)
# Overview tab content
Overview <- tabItem(tabName = "Overview",
  tags$a(
    href='https://science.psu.edu/',
    tags$img(src='https://educationshinyappteam.github.io/Style_Guide/theme/PSU_SCI_RGB_2C.png', class="psu_eberly_logo")
  ),
  h2("About"),
  p("APP_DESCRIPTION"),
  h2("Instructions:"),
  tags$ol(
    tags$li("STEP_1"),
    tags$li("STEP_2"),
    tags$li("STEP_3"),
    tags$li("STEP_4")
  ),
  h2("Acknowledgements:"),
  p("DEVELOPED_BY"),
  p("SPECIAL_THANKS")
)
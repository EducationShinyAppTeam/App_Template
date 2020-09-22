# Load Packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(ggplot2)

# App Meta Data----------------------------------------------------------------
APP_TITLE  <<- "[Sample App]"
APP_DESCP  <<- paste(
  "Description of the app",
  "use multiple lines to keep the description legible."
)
# End App Meta Data------------------------------------------------------------

# Load additional dependencies and setup functions
# source("global.R")

# Define UI for App
ui <- list(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css",
    href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
    #href = "boast.css") ## This is for Neil's testing purposes
  ),
  ## Create the app page
  dashboardPage(
    skin = "blue",
    ### Create the app header
    dashboardHeader(
      title = "Sample App", # You may use a shortened form of the title here
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(class = "dropdown",
              tags$a(href='https://github.com/EducationShinyAppTeam/BOAST',
                     icon("github"))),
      tags$li(class = "dropdown",
              tags$a(href='https://shinyapps.science.psu.edu/',
                     icon("home")))
    ),
    ### Create the sidebar/left navigation menu
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
        menuItem("Prerequisites", tabName = "Prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "Explore", icon = icon("wpexplorer")),
        menuItem("Challenge", tabName = "Challenge", icon = icon("gears")),
        menuItem("Game", tabName = "Game", icon = icon("gamepad")),
        menuItem("References", tabName = "References", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
      )
    ),
    ### Create the content
    dashboardBody(
      tabItems(
        #### Set up the Overview Page
        tabItem(
          tabName = "Overview",
          withMathJax(),
          h1("Sample Application for BOAST Apps"), # This should be the full name.
          p("This is a sample Shiny application for BOAST."),
          h2("Instructions"),
          p("This information will change depending on what you want to do."),
          tags$ol(
            tags$li("Review any prerequiste ideas using the Prerequistes tab."),
            tags$li("Explore the Exploration Tab."),
            tags$li("Challenge yourself."),
            tags$li("Play the game to test how far you've come.")
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Neil J.
            Hatfield  and Robert P. Carey, III.",
            br(),
            "We would like to extend a special thanks to the Shiny Program
            Students.",
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 5/13/2020 by NJH.")
          )
        ),
        #### Set up the Prerequisites Page
        tabItem(
          tabName = "Prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, please review the
            following:"),
          tags$ul(
            tags$li("Pre-req 1"),
            tags$li("Pre-req 2"),
            tags$li("Pre-req 3"),
            tags$li("Pre-req 4")
          ),
          p("Notice the use of an unordered list; users can move through the
            list any way they wish."),
          box(
            title = strong("Null Hypothesis Significance Tests (NHSTs)"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "In the Confirmatory Data Analysis tradition, null hypothesis
            significance tests serve as a critical tool to confirm that a
            particular theoretical model describes our data and to make a
            generalization from our sample to the broader population
            (i.e., make an inference). The null hypothesis often reflects the
            simpler of two models (e.g., 'no statistical difference',
            'there is an additive difference of 1', etc.) that we will use to
            build a sampling distribution for our chosen estimator. These
            methods let us test whether our sample data are consistent with this
            simple model (null hypothesis)."
          ),
          box(
            title = strong(tags$em("p"), "-values"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            "The probability that our selected estimator takes on a value at
            least as extreme as what we observed given our null hypothesis. If
            we were to carry out our study infinitely many times and the null
            hypothesis accurately modeled what we're studying, then we would
            expect for our estimator to produce a value at least as extreme as
            what we have seen 100*(p-value)% of the time. The larger the
            p-value, the more often we would expect our estimator to take on a
            value at least as extreme as what we've seen; the smaller, the less
            often."
          )
        ),
        #### Note: you must have at least one of the following pages. You might
        #### have more than one type and/or more than one of the same type. This
        #### will be up to you and the goals for your app.
        #### Set up an Explore Page
        tabItem(
          tabName = "Explore",
          withMathJax(),
          h2("Explore the Concept"),
          p("This page should include something for the user to do, the more
            active and engaging, the better. The purpose of this page is to help
            the user build a productive understanding of the concept your app
            is dedicated to."),
          p("Common elements include graphs, sliders, buttons, etc."),
          p("The following comes from the NHST Caveats App:"),
          ##### Example Exploration
          #---------------------------------------------------------------------
          #    Title: Caveats of NHST
          #    Author: Neil J. Hatfield
          #    Date: 10/15/19
          #    Code version: unknown
          #    Availability: https://github.com/EducationShinyAppTeam/
          #                 Significance_Testing_Caveats/tree/PedagogicalUpdate1
          #---------------------------------------------------------------------
          h2("The Multiple Testing Caveat"),
          p(
            "In this portion, you'll explore the relationship between the number
            of hypothesis tests you conduct and the number of results that would
            be declared as 'statistically significant'. You are able to control
            two aspects: 1) the number of hypothesis tests you want to simulate
            doing, and 2) the threshold for determining whether or not you would
            declare a test as 'statistically significant' (i.e., setting the
            value of \\(\\alpha_{UT}\\))."
          ),
          p(
            "Underlying this simulation is the notion that the null hypotheis is
            true. Thus, any p-value that is less than or equal to
            \\(\\alpha_{UT}\\) would lead a researcher to claim statistical
            significance and make a Type I error."
          ),
          p(
            "Use the controls to explore the relationship that exists between
            the number of hypothesis tests you conduct and the number of tests
            that would be declared 'statistically significant'."
          ),
          fluidRow(
            column(
              4,
              h3("Controls"),
              sliderInput(
                inputId = "mtcAlpha",
                label = "Set your threshold level, \\(\\alpha_{UT}\\):",
                min = 0.01,
                max = 0.25,
                value = 0.1,
                step = 0.01
              ),
              br(),
              sliderInput(
                inputId = "mtcTests",
                label = "Set the number of hypothesis tests conducted:",
                min = 0,
                max = 500,
                value = 5,
                step = 5
              )
            ),
            column(
              8,
              h3("Plot"),
              plotOutput("pplotMTC"),
              bsPopover(
                id = "pplotMTC",
                title = "Investigate!",
                content = "What happens to the number of statistically
                significant tests when you increase the number of tests?",
                placement = "top"
              )
            )
          ),
          br(),
          p(
            tags$em("Note"),
            ": The points above the horizontal line are all p-values that exceed
            your selected threshold. In other words, the points above the line
            represent the tests where you would decided that the null hypothesis
            provides a reasonable explanation for the data (i.e., 'fail to
            reject the null').  The points below or on the horizontal line are
            all p-values that are at or below your selected threshold. These
            points represent tests where you would decide that the null
            hypothesis doesn't adequately explain the data (i.e., 'reject the
            null')."
          )
          # End of Neil Hatfield's code-----------------------------------------
        ),
        #### Set up a Challenge Page
        tabItem(
          tabName = "Challenge",
          withMathJax(),
          h2("Challenge Yourself"),
          p("The general intent of a Challenge page is to have the user take
            what they learned in an Exploration and apply that knowledge in new
            contexts/situations. In essence, to have them challenge their
            understanding by testing themselves."),
          p("What this page looks like will be up to you. Something you might
            consider is to re-create the tools of the Exploration page and then
            a list of questions for the user to then answer.")
        ),
        #### Set up a Game Page
        tabItem(
          tabName = "Game",
          withMathJax(),
          h2("Practice/Test Yourself with [Type of Game]"),
          p("On this type of tab, you'll set up a game for the user to play.
            Game types include Tic-Tac-Toe, Matching, and a version Hangman to
            name a few. If you have ideas for new game type, please let us know.")
        ),
        #### Set up the References Page-REQUIRED
        tabItem(
          tabName = "References",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. (2019). boastUtils: BOAST Utilities. (v0.1.0).
            [R Package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018). shinydashboard: Create
            dashboards with 'Shiny'. (v0.7.1) [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019). shiny: Web application framework for R. (v1.4.0)
            [R Package]. Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Hatfield, N. J. (2019). Caveats of NHST. [Web App]. Available from
            https://github.com/EducationShinyAppTeam/Significance_Testing_Caveats
            /tree/PedagogicalUpdate1"
          ),
          p(
            class = "hangingindent",
            "Wickham, W. (2016). ggplot2: Elegant graphics for data analysis.
            [R Package]. Springer-Verlag New York. Available from
            https://ggplot2.tidyverse.org"
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  ## Define what each button does
  observeEvent(input$go1, {
    updateTabItems(session, "tabs", "Explore")
  })


  #-----------------------------------------------------------------------------
  #    Title: Caveats of NHST
  #    Author: Neil J. Hatfield
  #    Date: 10/15/19
  #    Code version: unknown
  #    Availability: https://github.com/EducationShinyAppTeam/
  #                 Significance_Testing_Caveats/tree/PedagogicalUpdate1
  #-----------------------------------------------------------------------------

  ## Listen for any inputs
  nMTC <- reactive({
    return(input$mtcTests)
  })
  aMTC <- reactive({
    return(input$mtcAlpha)
  })

  ## Code for any outputs
  #Plot for the Multiple Testing Caveat
  #General Logic: Create a user-specified number of hypothesis tests' p-values.
  #Compare those p-values to a user-specified threshold for significance.
  output$pplotMTC <- shiny::renderPlot({
    validate(need(input$mtcTests > 0,
                  message = "Please input a valid number of tests"))
    validate(need(input$mtcAlpha > 0,
                  message = "Please input a valid threshold"))
    a1 = aMTC() #Get threshold
    n1 = nMTC() #Get sample size
    x1 = 1:n1 #Create sample ids
    bp = 0 #Set counters
    rp = 0
    r = numeric(n1) #Create pvalue vector
    sim1 = rbinom(n = n1,
                  size = 1,
                  prob = 1 - a1) #Use Binomial to generate n2 Bernoulli trials;
    #Success means p-value will be greater than threshold
    for (w in 1:n1) {
      if (sim1[w] == 1) {
        r[w] = runif(1, (a1 + 0.001) , 0.999)
        bp = bp + 1
      }
      else{
        r[w] = runif(1, 0.0001, a1)
        rp = rp + 1
      }
    } #generate p-values
    #Generate the plot
    ggplot2::ggplot(data.frame(x1, r), aes(x = x1, y = r)) +
      ggplot2::geom_point(
        color = ifelse(r <= a1, "#F2665E", "#1E407C"),
        shape = 19,
        size = 3
      ) +
      ggplot2::geom_line(y = a1,
                color = "#3EA39E",
                size = 1) +
      labs(
        title = bquote(
          "The p-values for " ~ .(n1) ~ " hypothesis tests at " ~ alpha[UT] == .(a1)
        ),
        y = "p-value",
        x = "Test Number",
        caption = paste("There are", bp, "blue points and", rp, "red points")
      ) +
      ggplot2::theme(
        panel.background = element_rect(fill = 'white', colour = 'black'),
        plot.caption = element_text(size = 18),
        text = element_text(size = 18)
      ) +
      ggplot2::scale_y_continuous(expand = expansion(mult = 0, add = c(0, 0.05)),
                         limits = c(0, 1))
  })

  # End of Neil Hatfield's code-------------------------------------------------
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)

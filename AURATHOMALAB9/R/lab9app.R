#' Launch Train Lateness Shiny Application
#'
#' Opens an interactive Shiny dashboard to explore train lateness probabilities and areas.
#'
#' @export
lab9app <- function() {

  ui <- fluidPage(
    titlePanel("Train lateness"),
    sidebarLayout(
      sidebarPanel(
        # slider input
        sliderInput("ab_points", "a and b points, P(a <= Y <= b):",
                    min = -10, max = 10, value = c(-1.5557, 1.7335))
      ),
      mainPanel(
        # plot output
        plotOutput("lateness_plot",
                   brush = brushOpts(id = "plot_brush", resetOnNew = TRUE),
                   click = "plot_click"),
        # text output
        verbatimTextOutput("area_text")
      )
    )
  )

  server <- function(input, output, session) {
    # Add your server logic here to handle inputs, plots, and text output
  }

  shinyApp(ui = ui, server = server)
}

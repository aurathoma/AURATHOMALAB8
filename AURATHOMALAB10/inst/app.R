library(shiny)
library(dplyr)

# Load data (ensure "DDT.csv" is in your working directory)
ddt <- read.csv("DDT.csv")
df <- ddt %>% filter(SPECIES == "SMBUFFALO")
L <- df$LENGTH

ui <- fluidPage(
  titlePanel("Efficient Bootstrapping Simulation"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("iter",
                  "Number of iterations:",
                  min = 100,
                  max = 10000,
                  value = 1000,
                  step = 100),

      sliderInput("alpha",
                  "Alpha value:",
                  min = 0.01,
                  max = 0.2,
                  value = 0.05,
                  step = 0.01),

      selectInput(inputId = "fun",
                  label = "Function to calculate stats",
                  choices = c("mean", "var", "sd", "IQR"))
    ),

    mainPanel(
      plotOutput("bootPlot"),
      textOutput("ci")
    )
  )
)

server <- function(input, output) {

  # Reactive generation of the ONE simulation matrix
  sim_data <- reactive({
    n <- length(L)
    # Generate the full matrix of resamples ONCE for the simulation
    y <- sample(L, size = n * input$iter, replace = TRUE)
    matrix(y, nrow = n, ncol = input$iter, byrow = TRUE)
  })

  # Reactive application of the chosen function to the single simulation matrix
  v <- reactive({
    mat <- sim_data()
    # Apply different functions to the same simulation matrix
    xstat <- apply(mat, 2, match.fun(input$fun))
    ci <- quantile(xstat, c(input$alpha / 2, 1 - input$alpha / 2))
    list(xs = xstat, ci = ci)
  })

  output$bootPlot <- renderPlot({
    h <- hist(v()$xs, freq = FALSE, plot = FALSE, warn.unused = FALSE)
    hcol <- h$density / max(h$density)
    hist(v()$xs,
         xlab = paste("Statistic:", input$fun),
         ylab = "Density",
         main = paste0("SMBUFFALO Lengths (", input$fun, "), iterations = ", input$iter),
         col = rgb(hcol, hcol^2, 0))
  }, res = 96)

  output$ci <- renderText({
    paste("Confidence Interval bounds:", round(v()$ci[1], 4), ",", round(v()$ci[2], 4))
  })
}

shinyApp(ui = ui, server = server)

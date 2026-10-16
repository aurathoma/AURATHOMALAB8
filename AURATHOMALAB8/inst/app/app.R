#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Ensure Shiny can find the www directory relative to app.R
if (dir.exists("www")) {
  addResourcePath("www", "www")
} else if (dir.exists(file.path("inst", "app", "www"))) {
  addResourcePath("www", file.path("inst", "app", "www"))
}

ui <- navbarPage("Lab 8: Bivariate Transformations & Distributions",

                 # Tab 1: Theory & Math Proofs
                 tabPanel("Theory & Derivations",
                          fluidPage(
                            h3("Task 1 - 5: Mathematical Derivations"),
                            p("Below are the step-by-step proofs for the joint and marginal densities."),
                            # Display an image of your written math or handwritten notes saved in www/
                            img(src = "math_derivation.png", height = "400px"),
                            img(src = "task1.png",height = "400px"),
                            img(src = "task2.jpeg",height = "400px"),
                            img(src = "task3and4.jpeg", height = "400px"),
                            img(src = "task5and7.jpeg", height = "400px"),
                            img(src = "task8.jpeg", height = "400px"),
                            img(src = "task9and10.jpeg", height = "400px"),
                            hr(),
                            h4("Summary of Results"),
                            tags$ul(
                              tags$li("Joint Density g(a, b): Derived via Change of Variables"),
                              tags$li("Marginal Density g1(a): Uniform distribution"),
                              tags$li("Marginal Density g2(b): Gamma(2, 1) distribution")
                            )
                          )
                 ),

                 # Tab 2: Interactive Plots (Tasks 6 & 10)
                 tabPanel("Interactive Visualizations",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("b_param", "Explore Parameter / Scale:", min = 1, max = 5, value = 1),
                              helpText("Adjusting this updates the marginal density plot of B dynamically.")
                            ),
                            mainPanel(
                              h4("Task 6: Marginal Density of A g₁(a)"),
                              plotOutput("g1Plot"), # New plot for g1(a)
                              hr(),
                              h4("Task 6: Marginal Density of B g₂(b)"),
                              plotOutput("marginalPlot") # Existing plot for g2(b)
                            )
                          )
                 ),

                 # Tab 3: Task 10 CDF
                 tabPanel("Task 10: CDF of B",
                          fluidPage(
                            h3("Cumulative Distribution Function for B"),
                            p("Derived formula: F_B(b) = 1 - (b + 1)exp(-b)"),
                            plotOutput("cdfPlot") # Renders the Task 10 CDF plot here
                          )
                 ),

                 # Tab 4: Code Viewer
                 tabPanel("Source Code",
                          fluidPage(
                            h3("Package Functions"),
                            pre(
                              "# Example of underlying package function for g2(b)\n" ,
                              "calc_g2_b <- function(b) {\n  b * exp(-b)\n}"
                            )
                          )
                 )
)

server <- function(input, output, session) {

  # Plot for g1(a) (Task 6)
  output$g1Plot <- renderPlot({
    a <- seq(0, 1, length.out = 100)
    g1_a <- rep(1, length(a)) # Flat density of 1 on (0, 1)
    plot(a, g1_a, type = "l", col = "red", lwd = 2, ylim = c(0, 1.5),
         xlab = "a", ylab = "g_1(a)", main = "Marginal Density of A (Uniform)")
    grid()
  })

  # Plot for g2(b) (Task 6 & Interactive)
  output$marginalPlot <- renderPlot({
    b <- seq(0, 10, length.out = 100)
    g2_b <- b * exp(-input$b_param * b)
    plot(b, g2_b, type = "l", col = "blue", lwd = 2,
         xlab = "b", ylab = "g_2(b)", main = "Interactive Marginal Density of B (Gamma)")
    grid()
  })

  # Plot for Task 10 (Cumulative Distribution Function)
  output$cdfPlot <- renderPlot({
    b_vals <- seq(0, 10, length.out = 100)
    cdf_vals <- pgamma(b_vals, shape = 2, rate = 1)

    plot(b_vals, cdf_vals, type = "l", col = "darkgreen", lwd = 2,
         xlab = "b", ylab = "Cumulative Probability F_B(b)",
         main = "Cumulative Distribution Function of B ~ Gamma(2, 1)")
    grid()
  })
}
shinyApp(ui, server, options = list(launch.browser = TRUE))

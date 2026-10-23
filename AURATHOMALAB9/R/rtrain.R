#' Random Generation for Train Lateness
#'
#' Generates random samples from the custom train lateness distribution using inverse transform sampling, along with diagnostic plots.
#'
#' @param n Number of observations to generate.
#' @param u A probability value for the initial objective function curve plot.
#' @return A numeric vector of random samples.
#' @export
rtrain <- function(n, u = 0.4) {

  # Define the objective function using the exact polynomial expression minus u
  f <- function(x) {
    (75 * x - x^3 + 250) / 500 - u
  }

  # 1. First plot: Objective function curve from -5 to 1
  curve(f, c(-5, 1), xlab = "Y", ylab = "Objective function f", col = "black")
  abline(h = 0, col = "blue")

  # Find and print the root for the initial u value
  root_result <- uniroot(f, interval = c(-5, 5))
  print(root_result)

  # 2. Generate the random sample using replicate and uniroot
  sample_v <- replicate(n, {
    u_val <- runif(1) # Random uniform value between 0 and 1
    f_sample <- function(x) (75 * x - x^3 + 250) / 500 - u_val
    uniroot(f_sample, interval = c(-5, 5))$root
  })

  # 3. Second plot: Histogram of the simulation with the PDF overlay
  hist(sample_v, probability = TRUE, breaks = 40, col = "blue",
       main = "Simulation of the time a train is late", xlab = "Y", xlim = c(-5, 5))

  # Overlay the PDF curve (derivative of the CDF)
  curve((75 - 3 * x^2) / 500, from = -5, to = 5, col = "red", lwd = 2, add = TRUE)

  # Return vector of samples
  return(sample_v)
}

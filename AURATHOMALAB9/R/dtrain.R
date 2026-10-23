#' Probability Density Function for Train Lateness
#'
#' Calculates the probability density function (PDF) for the custom train lateness distribution.
#'
#' @param y A numeric vector of quantiles.
#' @return A numeric vector of probability densities.
#' @export
dtrain <- function(y) {
  # Derivative of the CDF
  d <- (75 - 3 * y^2) / 500

  # Handle boundary conditions for a PDF.
  d[y < -5 | y > 5] <- 0

  return(d)
}

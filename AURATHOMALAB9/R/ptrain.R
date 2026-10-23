#' Cumulative Distribution Function for Train Lateness
#'
#' Calculates the cumulative distribution function (CDF) for the custom train lateness distribution.
#'
#' @param y A numeric vector of quantiles.
#' @return A numeric vector of cumulative probabilities.
#' @export
ptrain <- function(y) {
  # F(y) calculation
  p <- (75 * y - y^3 + 250) / 500

  # y value boundaries
  p[y < -5] <- 0
  p[y > 5] <- 1

  # return p
  return(p)
}

#' Plot Histogram of Y
#'
#' Simulates values for Y and plots a density histogram overlaid with the theoretical curve.
#'
#' @name histY
#' @param iter Number of iterations (default is 1000).
#' @return A histogram plot of simulated Y values.
#' @export
#' @importFrom stats runif rexp
#' @importFrom graphics hist curve grid

utils::globalVariables("x")
histY <- function(iter = 1000) {
  X <- runif(iter, 0, 1)
  Y <- rexp(iter, rate = 1)
  B <- X + Y

  hist(B, probability = TRUE, main = "Histogram of y", xlab = "y", col = "lightgray", border = "white")
  curve(x * exp(-x), add = TRUE, col = "blue", lwd = 2)
  grid()
}

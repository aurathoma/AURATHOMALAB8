#' @param xlim Numeric vector of length 2 specifying the x-axis limits for the plot.
#'
#' @return A list containing:
#' \item{x}{Numeric vector of successive $x$ approximations generated during iteration.}
#' \item{y}{Numeric vector of function values evaluated at each $x$ approximation.}
#'
#' @export
#' @examples
#' g <- function(x) x^2 - 5*x + 6
#' mynewt_improved(x0 = 1, delta = 0.00001, f = g, xlim = c(0, 5))
#'
mynewt_improved <- function(...) { ... }

mynewt_improved <- function(x0, delta = 0.001, f, h = 0.0001, xlim = c(-x0, x0)) {
  d <- 100
  i <- 0
  x <- c()
  y <- c()
  x[1] <- x0
  y[1] <- f(x[1])

  while (d > delta & i < 1000) {
    i <- i + 1

    # Central difference approximation for the derivative (slope)
    fdash_approx <- (f(x[i] + h/2) - f(x[i] - h/2)) / h

    # Newton-Raphson step update
    x[i + 1] <- x[i] - f(x[i]) / fdash_approx
    y[i + 1] <- f(x[i + 1])
    d <- abs(y[i])
  }

  # Plotting convergence trajectory
  curve(f(x),
        xlim = xlim,
        xaxt = "n",
        main = "Newton-Raphson Algorithm (Numerical Derivative)")

  points(x,
         y,
         col = "Red",
         pch = 19,
         cex = 1.5)

  axis(1, x, round(x, 2), las = 2)
  abline(h = 0, col = "Red")

  segments(x[1:(i - 1)], y[1:(i - 1)], x[2:i], rep(0, i - 1), col = "Blue", lwd = 2)
  segments(x[2:i], rep(0, i - 1), x[2:i], y[2:i], lwd = 0.5, col = "Pink")

  list(x = x, y = y)
}

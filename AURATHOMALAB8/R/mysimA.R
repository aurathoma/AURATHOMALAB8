#' Simulate A = X / Y
#'
#' @param iter Number of iterations for the simulation.
#' @return A named list containing quantiles, sample mean, sample variance, and theoretical variance.
#' @export
#' @importFrom stats runif rexp quantile var
#' @importFrom graphics hist curve grid
mysimA <- function(iter = 1000) {
  X <- runif(iter, 0, 1)
  Y <- rexp(iter, rate = 1)
  A <- X / Y

  # Plot the histogram and theoretical flat line (Uniform density = 1 on (0,1))
  hist(A, probability = TRUE, main = "Histogram of Asim", xlab = "Asim",
       col = "lightgray", border = "white", ylim = c(0, 1.5))
  abline(h = 1, col = "blue", lwd = 2)
  grid()

  q <- quantile(A, probs = c(0.25, 0.5, 0.975))
  meanA <- mean(A)
  varA <- var(A)
  varAtheory <- 1/3 # Update with your exact derived theoretical variance if different

  list(q = q, meanA = meanA, varA = varA, varAtheory = varAtheory)
}

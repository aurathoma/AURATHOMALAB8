#' Simulate B = X + Y
#'
#' @param iter Number of iterations for the simulation.
#' @return A named list containing quantiles, sample mean, sample variance, and theoretical variance.
#' @export
#' @importFrom stats runif rexp quantile var
#' @importFrom graphics hist curve grid
mysimB <- function(iter = 1000) {
  X <- runif(iter, 0, 1)
  Y <- rexp(iter, rate = 1)
  B <- X + Y

  # Plot the histogram and theoretical density curve
  hist(B, probability = TRUE, main = "Histogram of Bsim", xlab = "Bsim",
       col = "lightgray", border = "white")
  curve(x * exp(-x), add = TRUE, col = "blue", lwd = 2) # Adjust curve formula to match your exact derivation if needed
  grid()

  q <- quantile(B, probs = c(0.25, 0.5, 0.975))
  meanB <- mean(B)
  varB <- var(B)
  varBtheory <- 2 # Update with your exact derived theoretical variance

  list(q = q, meanB = meanB, varB = varB, varBtheory = varBtheory)
}

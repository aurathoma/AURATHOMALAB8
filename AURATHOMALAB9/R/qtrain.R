#' Quantile Function for Train Lateness
#'
#' Calculates the quantiles (inverse CDF) for the custom train lateness distribution using root-finding.
#'
#' @param p A numeric vector of probabilities between 0 and 1.
#' @return A numeric vector of corresponding quantiles.
#' @export
qtrain <- function(p) {
  # Validate that probabilities are between 0 and 1
  if (any(p < 0 | p > 1)) {
    stop("Probabilities must be between 0 and 1.")
  }

  # Use sapply to vectorize uniroot
  sapply(p, function(prob) {
    if (prob == 0) return(-5)
    if (prob == 1) return(5)

    # Define the objective function
    f <- function(y) {
      ptrain(y) - prob
    }

    # Find the root within the interval
    uniroot(f, interval = c(-5, 5))$root
  })
}

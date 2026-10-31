#' Refined Grid Approximation for Maximum Likelihood Estimation
#'
#' Performs a two-stage grid search to estimate the maximum likelihood parameter.
#' It first evaluates a coarse parameter grid, locates the initial peak, and then
#' constructs a high-resolution sub-grid around that peak to yield a more precise estimate.
#'
#' @param lfun A log-likelihood function (e.g., \code{\link{logpoiss}} or \code{\link{logbin}}).
#' @param x A numeric vector of observed sample data.
#' @param param A numeric vector representing the initial coarse grid of candidate parameter values.
#' @param delta A numeric scalar defining the grid spacing increment for the refined search. Defaults to \code{0.0001}.
#'
#' @details
#' The function operates in two steps:
#' \enumerate{
#'   \item Evaluates \code{lfun} across the broad parameter vector \code{param} using \code{\link{outer}}
#'         and sums log-likelihoods via \code{\link{apply}} to identify the initial index peak \code{i}.
#'   \item Generates a fine-grained sequence \code{param2} spanning from \code{param[i-2]} to \code{param[i+2]}
#'         with step size \code{delta}, re-evaluates the log-likelihood surface, and identifies the refined MLE.
#' }
#' It displays side-by-side plots using \code{\link{layout}}: the left plot shows the global surface,
#' while the right plot displays the zoomed-in refined surface.
#'
#' @return A numeric scalar representing the refined maximum likelihood estimate (MLE).
#'
#' @examples
#' # Poisson log-likelihood helper function
#' logpoiss <- function(x, param) log(dpois(x, lambda = param))
#'
#' # Sample data: radioactive decay counts
#' y <- c(5, 5, 6, 7)
#' param_grid <- seq(0, 15, length = 1000)
#'
#' # Run two-stage grid search
#' mymld(lfun = logpoiss, x = y, param = param_grid, delta = 0.0001)
#'
#' @export
mymld <- function(lfun, x, param, delta = 0.0001) {

  z <- outer(x, param, lfun)
  y <- apply(z, 2, sum)
  i <- max(which(y == max(y)))

  param2 <- seq(param[i-2], param[i+2], by = delta)
  zz <- outer(x, param2, lfun)
  yy <- apply(zz, 2, sum)
  ii <- max(which(yy == max(yy)))

  layout(matrix(c(1, 2), nrow = 1, ncol = 2, byrow = TRUE))

  plot(param, y, col = "Blue", type = "l", lwd = 2,
       ylab = "Log. Lik.", xlab = expression(theta))
  abline(v = param[i], lwd = 2, col = "Red")
  axis(3, param[i], round(param[i], 2))
  points(param[i], y[i], pch = 19, cex = 1.5, col = "Black")

  plot(param2, yy, col = "Blue", type = "l", lwd = 2,
       ylab = "Log. Lik.", xlab = expression(theta), las = 2)
  abline(v = param2[ii], lwd = 2, col = "Red")
  val <- round(param2[ii], abs(log10(delta)))
  axis(3, param2[ii], val)
  points(param2[ii], yy[ii], pch = 19, cex = 1.5, col = "Black")

  layout(1)

  v <- param2[ii]
  return(v)
}

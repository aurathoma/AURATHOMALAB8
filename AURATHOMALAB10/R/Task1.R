#' Maximum Likelihood Estimation via Grid Approximation
#'
#' Evaluates a one-parameter log-likelihood function across a specified grid of
#' parameter values, plots the resulting log-likelihood curve, highlights the
#' maximum likelihood estimate (MLE), and returns summary details.
#'
#' @param lfun A function that calculates log-likelihood values. Standard
#'   examples include \code{\link{logbin}}, \code{\link{logpoiss}}, or \code{\link{logexp}}.
#' @param x A numeric vector of observed sample data.
#' @param param A numeric vector representing the grid of candidate parameter values.
#' @param ... Additional graphical parameters passed directly to \code{\link{plot}}.
#'
#' @details
#' The function uses \code{\link{outer}} to evaluate \code{lfun} across all combinations
#' of data points in \code{x} and parameter values in \code{param}. The log-likelihoods
#' are summed across sample observations to obtain total log-likelihood values \code{y}.
#' The maximum likelihood estimate corresponds to the parameter value that maximizes \code{y}.
#' Slopes immediately adjacent to the peak are calculated to verify convergence.
#'
#' @return An invisible named \code{list} containing:
#' \item{i}{The integer index in \code{param} where the maximum log-likelihood occurs.}
#' \item{parami}{The parameter value corresponding to the maximum log-likelihood estimate (MLE).}
#' \item{yi}{The maximum log-likelihood value evaluated at \code{parami}.}
#' \item{slope}{A numeric vector of local slopes around the maximum point, or \code{"NA"} if close to the grid boundaries.}
#'
#' @examples
#' # Log-likelihood helper function for binomial data
#' logbin <- function(x, param) log(dbinom(x, prob = param, size = 10))
#'
#' # Grid approximation for x = 2 successes out of 10 trials
#' param_grid <- seq(0, 1, length = 1000)
#' mymaxlik(lfun = logbin, x = c(2), param = param_grid, xlab = expression(p), main = "Binomial MLE")
#'
#' @export
mymaxlik <- function(lfun, x, param, ...) {

  np <- length(param)
  z <- outer(x, param, lfun)
  y <- apply(z, 2, sum)

  plot(param, y, col = "Blue", type = "l", lwd = 2, ...)

  i <- max(which.max(y))

  abline(v = param[i], lwd = 2, col = "Red")
  points(param[i], y[i], pch = 19, cex = 1.5, col = "Black")
  axis(3, param[i], round(param[i], 2))

  ifelse(i - 3 >= 1 & i + 2 <= np,
         slope <- (y[(i - 2):(i + 2)] - y[(i - 3):(i + 1)]) / (param[(i - 2):(i + 2)] - param[(i - 3):(i + 1)]),
         slope <- "NA")

  return(invisible(list(i = i,
                        parami = param[i],
                        yi = y[i],
                        slope = slope)))
}


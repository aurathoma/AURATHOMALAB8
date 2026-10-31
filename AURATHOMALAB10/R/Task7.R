#' Bootstrap Resampling Confidence Intervals
#'
#' Non-parametrically resamples sample data with replacement to generate an empirical
#' bootstrap distribution for a specified statistic, constructs percentile confidence
#' intervals, plots a annotated histogram, and returns interval boundaries.
#'
#' @param iter An integer specifying the number of bootstrap resampling iterations.
#'   Defaults to \code{10000}.
#' @param x A numeric vector of observed sample data.
#' @param fun A character string or function specifying the statistic to evaluate on each
#'   resample (e.g., \code{"mean"}, \code{"median"}, \code{"sd"}). Defaults to \code{"mean"}.
#' @param alpha A numeric scalar defining the significance level for the confidence interval.
#'   Defaults to \code{0.05} (yielding a 95\% confidence interval).
#' @param cx A numeric scalar controlling the character expansion size for plot text labels.
#'   Defaults to \code{1.5}.
#' @param ... Additional graphical parameters passed directly to \code{\link{hist}}.
#'
#' @details
#' The function generates \code{iter} synthetic samples of size \eqn{n = \text{length}(x)}
#' drawn with replacement from \code{x}. The user-defined statistic \code{fun} is calculated
#' across each resample to build the bootstrap empirical distribution.
#'
#' Quantiles at \eqn{\alpha/2} and \eqn{1 - \alpha/2} are extracted using \code{\link{quantile}}
#' to form the empirical \eqn{(1-\alpha)\times 100\%} percentile confidence interval.
#'
#' A relative frequency histogram is displayed featuring:
#' \itemize{
#'   \item A solid vertical black line indicating the point estimate calculated from original sample \code{x}.
#'   \item A thick red horizontal segment at baseline representing the confidence interval span.
#'   \item Annotations marking the exact numerical confidence bounds and point estimate.
#' }
#'
#' @return An invisible named \code{list} containing:
#' \item{ci}{A named numeric vector of length 2 containing lower and upper percentile bounds.}
#' \item{fun}{The statistic function evaluated.}
#' \item{x}{The original sample data vector.}
#'
#' @examples
#' # 95% Bootstrap Interval for LMBASS weights in DDT dataset
#' \dontrun{
#' lmbass_weights <- ddt$WEIGHT[ddt$SPECIES == "LMBASS"]
#' myboot(iter = 10000, x = lmbass_weights, fun = "mean", alpha = 0.05,
#'        xlab = "LMBASS Weight", col = "Purple")
#' }
#'
#' @export
myboot <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...) {

  n <- length(x)

  y <- sample(x, n * iter, replace = TRUE)
  rs.mat <- matrix(y, nr = n, nc = iter, byrow = TRUE)
  xstat <- apply(rs.mat, 2, fun)

  ci <- quantile(xstat, c(alpha / 2, 1 - alpha / 2))

  para <- hist(xstat,
               freq = FALSE,
               las = 1,
               main = paste("Histogram of Bootstrap sample statistics\n",
                            "alpha=", alpha, " iter=", iter, sep = ""),
               ...)

  mat <- matrix(x, nr = length(x), nc = 1, byrow = TRUE)
  pte <- apply(mat, 2, fun)

  abline(v = pte, lwd = 3, col = "Black")
  segments(ci[1], 0, ci[2], 0, lwd = 4)
  text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "Red", cex = cx)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "Red", cex = cx)
  text(pte, max(para$density) / 2, round(pte, 2), cex = cx)

  return(invisible(list(ci = ci, fun = fun, x = x)))
}

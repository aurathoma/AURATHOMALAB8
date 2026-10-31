#' Maximum Likelihood Estimation for Normal Distribution Parameters (Mu and Sigma)
#'
#' Evaluates the two-parameter joint log-likelihood surface across grids of candidate
#' mean (\code{mu}) and standard deviation (\code{sig}) values for a normal sample.
#' Generates a 2D contour plot of the likelihood surface, overlays theoretical MLEs,
#' and returns the estimated parameters.
#'
#' @param x A numeric vector of sample data assumed to come from a Normal distribution.
#' @param mu A numeric vector defining the search grid for the mean parameter \eqn{\mu}.
#' @param sig A numeric vector defining the search grid for the standard deviation parameter \eqn{\sigma}.
#' @param ... Additional graphical parameters passed directly to \code{\link{contour}}.
#'
#' @details
#' The function constructs a likelihood matrix across all grid combinations of \code{mu}
#' and \code{sig}. For each candidate \code{sig[j]}, log-likelihoods across \code{mu} are
#' computed using \code{\link{outer}} with \code{\link{dnorm}} and summed across the sample
#' observations \code{x}. The overall maximum joint likelihood is identified, and a contour
#' plot displaying \eqn{L(\mu, \sigma) = \exp(\text{Log. Lik.})} is generated.
#'
#' Theoretical values—the sample mean \eqn{\bar{x}} and the MLE standard deviation
#' \eqn{\hat{\sigma} = \sqrt{\frac{n-1}{n}} \cdot s}—are drawn as solid green and red reference
#' lines, respectively.
#'
#' @return A named \code{list} containing:
#' \item{x}{The input sample vector.}
#' \item{muest}{The grid estimate for \eqn{\mu} corresponding to the peak likelihood.}
#' \item{sigest}{The grid estimate for \eqn{\sigma} corresponding to the peak likelihood.}
#' \item{maxl}{The maximum joint likelihood value evaluated at (\code{muest}, \code{sigest}).}
#'
#' @examples
#' # Sample data
#' x_data <- c(5, 7, 7, 8, 10)
#'
#' # Define search grids
#' mu_grid <- seq(4, 11, length = 1000)
#' sig_grid <- seq(0.5, 5, length = 1000)
#'
#' # Run 2D grid search
#' mymlnorm(x = x_data, mu = mu_grid, sig = sig_grid)
#'
#' @export
mymlnorm <- function(x, mu, sig, ...) {

  nmu <- length(mu)
  nsig <- length(sig)
  n <- length(x)
  zz <- c()

  lfun <- function(x, m, p) log(dnorm(x, mean = m, sd = p))

  for (j in 1:nsig) {
    z <- outer(x, mu, lfun, p = sig[j])
    y <- apply(z, 2, sum)
    zz <- cbind(zz, y)
  }

  maxl <- max(exp(zz))
  coord <- which(exp(zz) == maxl, arr.ind = TRUE)

  contour(mu, sig, exp(zz), las = 1, xlab = expression(mu), ylab = expression(sigma),
          axes = TRUE, main = expression(paste("L(", mu, ",", sigma, ")", sep = "")), ...)

  abline(v = mean(x), lwd = 2, col = "Green")
  abline(h = sqrt((n - 1) / n) * sd(x), lwd = 2, col = "Red")

  muest <- mu[coord[1]]
  sigest <- sig[coord[2]]

  abline(v = muest, h = sigest)

  return(list(x = x, muest = muest, sigest = sigest, maxl = maxl))
}

mynewt=function(x0,delta=0.001,f,fdash, xlim = c(-x0,x0)){
  d <- 100
  i <- 0
  x <- c()
  y <- c()
  x[1] <- x0
  y[1] <- f(x[1])
  while(d > delta & i < 1000){
    i <- i+1
    x[i+1] <- x[i] - f(x[i])/fdash(x[i])
    y[i+1] <- f(x[i+1])
    d <- abs(y[i])
  }
  #windows()
  curve(f(x),
        xlim = xlim,
        xaxt = "n",
        main = "Newton-Raphson Algorithm")

  points(x,
         y,
         col = "Red",
         pch = 19,
         cex = 1.5)

  axis(1,x,round(x,2),las=2)
  abline(h = 0,col = "Red")

  segments(x[1:(i-1)],y[1:(i-1)],x[2:i],rep(0,i-1),col="Blue",lwd=2)
  segments(x[2:i],rep(0,i-1),x[2:i],y[2:i],lwd=0.5,col="Pink")

  list(x = x,y = y)
}

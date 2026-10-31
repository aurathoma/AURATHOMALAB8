#' Root Finding via Newton-Raphson Method
#'
#' Computes the root of a function using the iterative Newton-Raphson algorithm.
#' Evaluates derivative steps dynamically via central difference numerical differentiation,
#' plots the convergence path with tangent line segments, and returns the iteration history.
#'
#' @param x0 A numeric scalar representing the initial starting guess for the root.
#' @param delta A numeric scalar defining the convergence tolerance threshold.
#'   The algorithm terminates when \eqn{|f(x_n)| < \text{delta}}. Defaults to \code{0.00001}.
#' @param f A function of one variable whose root \eqn{f(x) = 0} is to be found.
#' @param h A numeric scalar specifying the step size used for central difference
#'   numerical derivative calculations. Defaults to \code{0.0001}.
#' @param xlim A numeric vector of length 2 defining the x-axis plotting limits.
#'   Defaults to \code{c(-x0, x0)}.
#'
#' @details
#' The function applies the Newton-Raphson update formula:
#' \eqn{x_{n+1} = x_n - \frac{f(x_n)}{f'(x_n)}}.
#'
#' Rather than requiring an analytical derivative function, \eqn{f'(x_n)} is approximated
#' numerically using the central difference formula:
#' \eqn{f'(x) \approx \frac{f(x + h/2) - f(x - h/2)}{h}}.
#'
#' A plot of \eqn{f(x)} is generated, illustrating each iteration with blue tangent segments
#' dropping vertically (pink segments) to the updated estimate points marked with red dots.
#'
#' @return A named \code{list} containing:
#' \item{x}{A numeric vector containing the sequence of \eqn{x} estimates across iterations.}
#' \item{y}{A numeric vector containing the corresponding function values \eqn{f(x)} evaluated at each step.}
#'
#' @examples
#' # Define target polynomial function g(x) = x^2 - 5x + 6 (roots at x = 2 and x = 3)
#' g <- function(x) x^2 - 5*x + 6
#'
#' # Convergence to Root 1 (x = 2) starting from x0 = 1
#' res1 <- mynewt_improved(x0 = 1, delta = 0.00001, f = g, xlim = c(0, 5))
#' res1$x[length(res1$x)]
#'
#' # Convergence to Root 2 (x = 3) starting from x0 = 4
#' res2 <- mynewt_improved(x0 = 4, delta = 0.00001, f = g, xlim = c(0, 5))
#' res2$x[length(res2$x)]
#'
#' @export
myNRML <- function(x0,delta = 0.001,llik,xrange,parameter = "param"){
  f <- function(x) (llik(x+delta)-llik(x))/delta
  fdash <- function(x) (f(x+delta)-f(x))/delta
  d=1000
  i=0
  x=c()
  y=c()
  x[1]=x0
  y[1]=f(x[1])
  while(d > delta & i<100){
    i=i+1
    x[i+1]=x[i]-f(x[i])/fdash(x[i])
    y[i+1]=f(x[i+1])
    d=abs(y[i+1])
  }
  layout(matrix(1:2,nr=1,nc=2,byrow=TRUE),width=c(1,2))
  curve(llik(x), xlim=xrange,xlab=parameter,ylab="log Lik",main="Log Lik")
  curve(f(x),xlim=xrange,xaxt="n", xlab=parameter,ylab="derivative",main=  "Newton-Raphson Algorithm \n on the derivative")
  points(x,y,col="Red",pch=19,cex=1.5)
  axis(1,x,round(x,2),las=2)
  abline(h=0,col="Red")

  segments(x[1:(i-1)],y[1:(i-1)],x[2:i],rep(0,i-1),col="Blue",lwd=2)
  segments(x[2:i],rep(0,i-1),x[2:i],y[2:i],lwd=0.5,col="Green")

  list(x=x)}

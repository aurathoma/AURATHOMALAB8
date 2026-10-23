#' Visualize Train Lateness Area and Density
#'
#' Plots the probability density function and shades the region between two points a and b.
#'
#' @param a Lower bound quantile.
#' @param b Upper bound quantile.
#' @return The calculated probability area rounded to 4 decimal places.
#' @export
mytrain <- function(a, b) {
  # solve if y >= -5 and y <= 5
  f <- function(y) {
    ifelse(y >= -5 & y <= 5, (3 / 500) * (25 - y^2), 0)
  }

  # Define the CDF function F(y) to compute between values of a and b
  F_val <- function(y) {
    p <- (75 * y - y^3 + 250) / 500
    p[y < -5] <- 0
    p[y > 5] <- 1
    return(p)
  }

  # Calculate area P(a <= Y <= b) rounded to 4 decimal places
  area <- round(F_val(b) - F_val(a), 4)

  # Curve points for plot
  x_curve <- seq(-10, 10, length.out = 1000)
  y_curve <- f(x_curve)

  # Plot the density curve
  plot(
    x_curve, y_curve,
    type = "l",
    col = "blue",
    lwd = 2,
    xaxt = "n",
    xlab = "Y",
    ylab = "Density",
    main = "Time a train is late"
  )

  # Custom x-axis ticks
  axis(1, at = c(-10, -5, a, 0, b, 5, 10))

  # polygon between a and b
  x_shade <- seq(a, b, length.out = 500)
  y_shade <- f(x_shade)
  polygon(
    c(a, x_shade, b),
    c(0, y_shade, 0),
    col = "red",
    border = "black"
  )

  # create line curves
  lines(x_curve, y_curve, col = "blue", lwd = 2)

  # add text displaying area value
  text(
    x = b + 1.5,
    y = 0.10,
    labels = paste0("Area = ", round(area, 2))
  )

  # Return the result
  return(area)
}

#' Run the Lab 8 Shiny Application
#'
#' Launches an interactive Shiny application that displays the theoretical derivations,
#' marginal density plots for $A$ and $B$ (Task 6), and the cumulative distribution
#' function for $B$ (Task 10).
#'
#' @return No return value; this function opens an interactive Shiny app in your browser.
#' @export
#'
#' @examples
#' \dontrun{
#'   runLab8App()
#' }
runLab8App <- function() {
  appDir <- system.file("app", package = "AURATHOMALAB8")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing the package.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}


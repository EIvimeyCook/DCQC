#' @title Run the DCQC reviewing app
#' @description The SORTEE DCQC reviewing app allows data editors to review papers according to the SORTEE guidelines.
#' @return A .rtf of the DCQC review
#' @export

DCQC <- function() {
    shiny_env <- 1
    envir <- as.environment(shiny_env)

    appDir <- system.file("DCQC", package = "DCQC")
    shiny::runApp(appDir, display.mode = "normal")
  }
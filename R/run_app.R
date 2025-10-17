#Create R/run_app.R with this content (roxygen docs included):

#' Launch the CHIRPS&CHIRTS Downloader Shiny App
#'
#' This function runs the integrated CHIRPS Shiny application.
#'
#' @return Starts the Shiny app interface.
#' @export
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
run_app <- function() {
  app_dir <- system.file("app", package = "ChirptsDownloader")
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Try re-installing the package.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}

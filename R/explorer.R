#' Launch the BeSD Explorer Shiny app
#'
#' Opens the interactive BeSD Explorer dashboard in your default browser.
#' The app uses pre-computed \code{besd_sum} and \code{demo_sum} objects
#' stored in the app's \code{data/} directory.
#'
#' @export
launch_explorer <- function() {
  app_dir <- system.file("shiny/explorer", package = "rbesd")
  if (!nzchar(app_dir)) {
    stop("Could not find the BeSD Explorer app. ",
         "Make sure rbesd is installed correctly.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}

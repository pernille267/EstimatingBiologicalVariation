#' Run the Biological Variation Analysis Shiny Application
#'
#' Launches the full Shiny application. This is the main entry point
#' when the package is installed.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{shinyApp}}.
#' @return A Shiny app object (invisibly).
#' @export
run_app <- function(...) {
    # Locate the app directory shipped with the package

    app_dir <- system.file("app", package = "EstimatingBiologicalVariation")
    if (app_dir == "") {
        stop(
            "Could not find the app directory. ",
            "Make sure the package is installed correctly.",
            call. = FALSE
        )
    }

    # Profiling option for development
    options(bv.perf = TRUE)

    # Run the app
    shiny::runApp(appDir = app_dir, ...)
}

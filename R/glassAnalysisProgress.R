# Glass Analysis Progress
# ------------------------------------------------------------------------------
# A beautiful glassmorphism progress overlay for long-running analyses.
# Replaces shiny::withProgress with full control over display, timing, and ETA.
#
# Usage (drop-in replacement for withProgress/incProgress):
#
#   glassWithProgress(title = "Running Model...", {
#     glassIncProgress(0.1, detail = "Preparing data...")
#     # ... do work ...
#     glassIncProgress(0.5, detail = "Running MCMC sampler...")
#     # ... do work ...
#     glassIncProgress(1.0, detail = "Complete!")
#   })
#
# ------------------------------------------------------------------------------

#' HTML dependency for Glass Analysis Progress
#'
#' Include this in your UI to load the JS and CSS assets.
#' Uses tags$head since this app relies on addResourcePath rather than
#' being an installed R package.
#'
#' @return A tagList with script and stylesheet tags
#' @export
useGlassAnalysisProgress <- function() {
  htmltools::tagList(
    tags$head(
      tags$script(src = "assets/glass_analysis_progress.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "assets/glass_analysis_progress.css")
    )
  )
}

#' Open the Glass Analysis Progress overlay
#'
#' Sends a custom message to the client to show the progress overlay.
#'
#' @param title Header title displayed on the progress card
#' @param session Shiny session (auto-detected)
#'
#' @export
openGlassProgress <- function(title = "Running Analysis...",
                              session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage("glass-analysis-progress-open", list(
    title = title
  ))
}

#' Update the Glass Analysis Progress
#'
#' @param value Numeric 0–1 representing current progress
#' (absolute, not incremental)
#' @param detail Text describing the current sub-task
#' (e.g. "Running MCMC sampler...")
#' @param step Short step label shown top-right
#' (optional; auto-generated if NULL)
#' @param session Shiny session
#'
#' @export
updateGlassProgress <- function(value,
                                detail = NULL,
                                step = NULL,
                                session = shiny::getDefaultReactiveDomain()) {
  msg <- list(value = value)
  if (!is.null(detail)) msg$detail <- detail
  if (!is.null(step)) msg$step <- step
  session$sendCustomMessage("glass-analysis-progress-update", msg)
}

#' Close the Glass Analysis Progress overlay
#'
#' @param delay Milliseconds to wait before fade-out (lets user see 100%)
#' @param session Shiny session
#'
#' @export
closeGlassProgress <- function(delay = 800,
                               session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage("glass-analysis-progress-close", list(
    delay = delay
  ))
}

#' Signal an error on the Glass Analysis Progress overlay
#'
#' Shows the error state and auto-closes after `delay` ms.
#'
#' @param detail Error message to display
#' @param delay Auto-close delay in ms (0 = never auto-close)
#' @param session Shiny session
#'
#' @export
errorGlassProgress <- function(detail = "An error occurred.",
                               delay = 5000,
                               session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage("glass-analysis-progress-error", list(
    detail = detail,
    delay  = delay
  ))
}


# ==============================================================================
# Convenience wrappers that mirror the withProgress / incProgress interface
# ==============================================================================

#' Glass Analysis Progress wrapper (drop-in replacement for withProgress)
#'
#' Opens the progress overlay, evaluates the expression, and closes the overlay
#' when done — even on error. Inside `expr`, call `glassIncProgress()`
#' to update.
#'
#' @param expr Expression to evaluate (use curly braces)
#' @param title Title shown on the progress card
#' @param session Shiny session
#'
#' @return The result of evaluating `expr`
#' @export
glassWithProgress <- function(expr,
                              title = "Running Analysis...",
                              session = shiny::getDefaultReactiveDomain()) {
  # Store cumulative value in the session's userData so glassIncProgress can see it
  session$userData$glass_progress_value <- 0

  openGlassProgress(title = title, session = session)

  result <- tryCatch(
    expr = {
      force(expr)
    },
    error = function(e) {
      errorGlassProgress(detail = conditionMessage(e), session = session)
      stop(e)
    }
  )

  # Ensure we hit 100% visually
  updateGlassProgress(value = 1.0, detail = "Complete!", session = session)
  closeGlassProgress(delay = 1200, session = session)

  # Reset

  session$userData$glass_progress_value <- NULL

  invisible(result)
}


#' Increment Glass Analysis Progress (drop-in replacement for incProgress)
#'
#' When used inside `glassWithProgress`, this sets the progress to `value`
#' (absolute, 0–1) with an optional detail message. If you want truly
#' incremental behaviour, pass the running total yourself.
#'
#' @param value Absolute progress value 0–1
#' @param detail Description of the current sub-task
#' @param step Short step label (optional)
#' @param session Shiny session
#'
#' @export
glassIncProgress <- function(value,
                             detail = NULL,
                             step = NULL,
                             session = shiny::getDefaultReactiveDomain()) {
  # Value is treated as ABSOLUTE (matching how incProgress is typically used
  # in the codebase where values like 0.1, 0.2, ... 1.0 are passed).
  session$userData$glass_progress_value <- value
  updateGlassProgress(value = value, detail = detail, step = step, session = session)
}

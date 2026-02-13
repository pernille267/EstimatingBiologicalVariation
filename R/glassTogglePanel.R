#' Glass Toggle Panel
#'
#' A content panel that toggles visibility based on a button click (counter).
#' Unlike conditionalPanel, this supports CSS transitions (slide/fade).
#' 
#' MIGHT NEED FIX: Is it acting as a ghost element that appears when triggered,
#' or is it more of a wrapper that shows/hides its content? If it is the
#' latter, we need to fix it as it will reserve space even when hidden,
#' which is not ideal.
#'
#' @param triggerId The inputId of the button (glassButton)
#'  that toggles this panel.
#' @param show_when Optional. A specific value (character/numeric)
#' that the input at `triggerId` must match to show the panel.
#' If NULL (default), it assumes `triggerId` is a counter
#' (odd = show, even = hide) (button triggers).
#' @param ... Content to display inside the panel.
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
glassTogglePanel <- function(triggerId, show_when = NULL, ...) { # nolint
  # Build attributes list
  attribs <- list(
    class = "glass-toggle-panel",
    `data-trigger` = triggerId
  )

  # Add the matching condition if provided
  if (!is.null(show_when)) {
    attribs$`data-show-when` <- as.character(show_when)
  }

  # Create the HTML structure
  ui_structure <- do.call(htmltools::tags$div, c(attribs, list(
    htmltools::tags$div(
      class = "glass-toggle-content",
      htmltools::tagList(...)
    )
  )))

  # Attach Dependencies (CSS/JS)
  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-toggle-panel",
      version = "1.0.0",
      src = c(
        file = system.file("assets", package = "EstimatingBiologicalVariation")
      ),
      script = "glass_toggle_panel.js",
      stylesheet = "glass_toggle_panel.css"
    )
  )
}

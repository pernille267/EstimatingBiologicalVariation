#' Glass Result Card
#'
#' A container optimized for displaying tables and plots.
#' Includes a header toolbar for action buttons (Calculate, Download).
#'
#' @param inputId The id of the card.
#' @param title The title text.
#' @param ... Content (The Table or Plot).
#' @param toolbar UI elements to place in the header (e.g. buttons).
#' @param icon An optional icon().
#' @param footer Optional footer text or UI.
#' @param attached Logical. If TRUE, removes top border-radius to blend with a boxed tabset above.
#' @param width Width of the card.
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
glassResultCard <- function(inputId, title, ..., toolbar = NULL, icon = NULL, footer = NULL, attached = FALSE, width = "100%") {
  # Class Construction
  card_classes <- "glass-result-card"
  if (attached) card_classes <- paste(card_classes, "attached-top")

  # Icon Handling
  icon_html <- if (!is.null(icon)) {
    htmltools::tags$div(class = "glass-result-icon", icon)
  } else {
    NULL
  }

  # Toolbar Handling
  toolbar_html <- if (!is.null(toolbar)) {
    htmltools::tags$div(class = "glass-result-toolbar", toolbar)
  } else {
    NULL
  }

  # Footer Handling
  footer_html <- if (!is.null(footer)) {
    htmltools::tags$div(class = "glass-result-footer", footer)
  } else {
    NULL
  }

  # Structure
  ui_structure <- htmltools::tags$div(
    id = inputId,
    class = card_classes,
    style = paste0("width: ", width, ";"),

    # Header
    htmltools::tags$div(
      class = "glass-result-header",

      # Left: Icon + Title
      htmltools::tags$div(
        class = "glass-result-title-wrap",
        icon_html,
        htmltools::tags$h4(class = "glass-result-title", title)
      ),

      # Right: Action Buttons
      toolbar_html
    ),

    # Body
    htmltools::tags$div(
      class = "glass-result-body",
      htmltools::tagList(...)
    ),

    # Optional Footer
    footer_html
  )

  # Attach CSS (No JS needed for this simple card)
  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-result-card",
      version = "1.0.0",
      src = c(file = system.file("assets", package = "EstimatingBiologicalVariation")),
      script = "glass_result_card.js",
      stylesheet = "glass_result_card.css"
    )
  )
}

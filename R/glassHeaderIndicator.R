#' Glass Header Indicator
#'
#' A versatile indicator for the dashboard header. Give interactive feedback
#' to the user about what is going on in the app, so they feel informed and in control.
#'
#' @param inputId Unique ID.
#' @param type Type of indicator: "filter" (default),
#'  "reference", or "diagnostics".
#' @param tooltip_text Initial text to display on hover.
#' @param visible Logical. Should it be visible initially?
#'  Defaults to FALSE.
#'
#' @importFrom htmltools tagList tags htmlDependency HTML
#' @export
glassHeaderIndicator <- function(inputId,
                                 type = c("filter", "reference", "diagnostics"),
                                 icon_name = NULL,
                                 tooltip_text = "",
                                 visible = FALSE) {
  type <- match.arg(type)

  # Determine Inner Content based on Type ---
  inner_content <- switch(
    type,
    "filter" = htmltools::tags$div(
      class = "glass-indicator-icons",
      shiny::icon(
        "ruler-horizontal",
        class = "indicator-icon-main"
      ),
      shiny::icon(
        "filter",
        class = "indicator-icon-sub"
      )
    ),
    "reference" = htmltools::tags$div(
      class = "glass-indicator-text-icon",
      "R"
    ),
    "diagnostics" = htmltools::tags$div(
      class = "glass-indicator-icon-single",
      shiny::icon(
        if (!is.null(icon_name)) icon_name else "heart-pulse"
      )
    )
  )

  # Determine CSS Classes ---
  # Base class + Type class + Visibility
  classes <- paste("glass-header-indicator", type, if (!visible) "hidden" else "")

  # Determine UI Structure ---
  ui_structure <- htmltools::tags$div(
    id = inputId,
    class = classes,

    # The Content (Icon/Text)
    inner_content,

    # The Tooltip
    htmltools::tags$div(
      class = "glass-indicator-tooltip",
      tooltip_text
    )
  )

  # Attach Dependencies ---
  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-header-indicator",
      version = "1.0.0",
      src = c(
        file = system.file("assets", package = "EstimatingBiologicalVariation")
      ),
      script = "glass_header_indicator.js",
      stylesheet = "glass_header_indicator.css"
    )
  )
}

#' Update Glass Header Indicator
#'
#' Updates the state, text, and styling of the header indicator.
#'
#' @param session Shiny session.
#' @param inputId ID of the indicator.
#' @param visible Logical. Show or hide?
#' @param tooltip_text String. New text for tooltip.
#' @param status String. Optional status class
#'  ("success", "warning", "error", "info").
#'  Only applies to 'diagnostics' type to change color.
#' @param icon_name String. Optional. Name of a new icon to set
#'  (e.g. "triangle-exclamation").
#'  Mostly for 'diagnostics' type.
#' @export
updateGlassHeaderIndicator <- function(session, inputId, # nolint
                                       visible = NULL,
                                       tooltip_text = NULL,
                                       status = NULL,
                                       icon_name = NULL) {
  message <- list()
  if (!is.null(visible)) message$visible <- visible
  if (!is.null(tooltip_text)) message$tooltip_text <- tooltip_text
  if (!is.null(status)) message$status <- status
  if (!is.null(icon_name)) message$icon_name <- icon_name

  # Send E-mail to JS Handler to Update the Indicator
  session$sendInputMessage(inputId, message)
}

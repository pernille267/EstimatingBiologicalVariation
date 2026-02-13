#' Glass Toolbar
#'
#' A lightweight horizontal strip that displays a title on the left and
#' arbitrary UI controls (buttons, dropdowns, selectize inputs, radio
#' groups, etc.) on the right.  Designed to replace inner glassCards when
#' the only purpose is to host a title + a few inline controls above a
#' content area.
#'
#' @param title \code{character} string. The title text displayed
#'  on the left.
#' @param ... UI elements placed in the right-hand controls area.
#' @param icon An optional \code{shiny::icon()} shown before the title.
#' @param subtitle Optional secondary text below the title.
#' @param color Theme colour: \code{"purple"} (default) or \code{"green"}.
#' @param border Logical. Show a subtle border?
#'   Defaults to \code{TRUE}.
#' @param rounded Character. Where to round corners:
#'   \code{"all"} (default), \code{"top"} (only top corners),
#'   or \code{"none"}.
#'
#' @return An \code{htmltools::tagList} containing the toolbar HTML and
#'   its CSS dependency.
#'
#' @examples
#' glassToolbar(
#'   title = "Data Points",
#'   icon  = shiny::icon("chart-line"),
#'   glassButton("btn", "Click me")
#' )
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
glassToolbar <- function(title,
                         ...,
                         icon = NULL,
                         subtitle = NULL,
                         color = "purple",
                         border = TRUE,
                         rounded = "all") {

  # --- Icon ---
  icon_html <- if (!is.null(icon)) {
    htmltools::tags$div(class = "glass-toolbar-icon", icon)
  }

  # --- Title block ---
  title_inner <- htmltools::tags$span(
    class = "glass-toolbar-title-text",
    title
  )
  subtitle_html <- if (!is.null(subtitle)) {
    htmltools::tags$span(
      class = "glass-toolbar-subtitle",
      subtitle
    )
  }
  title_html <- htmltools::tags$div(
    class = "glass-toolbar-title-wrap",
    icon_html,
    htmltools::tags$div(
      class = "glass-toolbar-title-stack",
      title_inner,
      subtitle_html
    )
  )

  # --- Controls (right side) ---
  controls <- list(...)
  controls_html <- htmltools::tags$div(
    class = "glass-toolbar-controls",
    htmltools::tagList(controls)
  )

  # --- CSS classes ---
  final_class <- "glass-toolbar"
  if (color == "green") {
    final_class <- paste(
      final_class,
      "glass-toolbar-green"
    )
  }
  if (!border) {
    final_class <- paste(
      final_class,
      "glass-toolbar-borderless"
    )
  }
  if (rounded == "top") {
    final_class <- paste(
      final_class,
      "glass-toolbar-round-top"
    )
  }
  if (rounded == "none") {
    final_class <- paste(
      final_class,
      "glass-toolbar-round-none"
    )
  }

  # --- Build markup ---
  ui <- htmltools::tags$div(
    class = final_class,
    title_html,
    controls_html
  )

  htmltools::tagList(
    ui,
    htmltools::htmlDependency(
      name = "glass-toolbar",
      version = "1.0.0",
      src = c(
        file = system.file("assets", package = "EstimatingBiologicalVariation")
      ),
      stylesheet = "glass_toolbar.css"
    )
  )
}

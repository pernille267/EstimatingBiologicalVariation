#' Create a Flexible Glass Badge
#'
#' @description
#' Generates a customizable badge element with glassmorphism styling.
#' Supports various shapes, colors, animations, and interactive tooltips.
#'
#' @param label The text or icon to display inside the badge.
#' @param color A character string indicating the color theme.
#'  Options: "green", "orange", "blue", "red",
#'  "purple", "teal", "gray", "gold".
#'  Alternatively, can be a custom hex code.
#' @param shape The shape of the badge. Options: "pill", "circle",
#' "square", "star", "hexagon".
#' @param tooltip Optional text to display when interacting with the badge.
#' @param tooltip_mode How the tooltip is triggered. Options: "hover", "click".
#' @param glow Logical. If TRUE, the badge emits a subtle pulse animation.
#' @param size The size of the badge. Options: "sm", "md", "lg".
#'
#' @return A UI definition (HTML tag) for the badge.
#' @export
glassBadge <- function(label, # nolint
                       color = "blue",
                       shape = "pill",
                       tooltip = NULL,
                       tooltip_mode = "hover",
                       glow = FALSE,
                       size = "md") {
  # --- dependency injection ---
  # Ensure the assets are loaded (usually handled by useGlassLoader/UI,
  # but good to have the dependency object ready if needed)
  dep <- htmltools::htmlDependency(
    name = "glass-badge",
    version = "1.0.0",
    src = c(
      file = system.file("assets", package = "EstimatingBiologicalVariation")
    ),
    script = "glass_badge.js",
    stylesheet = "glass_badge.css"
  )

  # --- Class Construction ---
  base_class <- "glass-badge"
  color_class <- paste0("gb-color-", color)
  shape_class <- paste0("gb-shape-", shape)
  size_class <- paste0("gb-size-", size)
  glow_class <- if (glow) "gb-glow" else ""
  tooltip_class <- if (!is.null(tooltip)) {
    paste0("gb-tooltip-trigger-", tooltip_mode)
  } else {
    ""
  }

  final_class <- paste(
    base_class, color_class, shape_class, size_class, glow_class, tooltip_class
  )

  # --- Inline Style for Custom Hex Colors ---
  # If color isn't one of the presets, we apply it as a CSS variable
  known_colors <- c(
    "green",
    "orange",
    "blue",
    "red",
    "purple",
    "yellow",
    "gray",
    "gold"
  )
  style_attr <- NULL
  # Handle custom color (assuming it's a valid CSS color string)
  if (!color %in% known_colors) {
    style_attr <- sprintf("--gb-custom-color: %s;", color)
    final_class <- paste(final_class, "gb-color-custom")
  }

  # --- Assemble Tag ---
  tag_badge <- htmltools::tags$span(
    class = final_class,
    style = style_attr,
    htmltools::HTML(label)
  )

  # Append Tooltip if desired
  if (!is.null(tooltip)) {
    tag_badge <- htmltools::tagAppendAttributes(
      tag_badge,
      `data-tooltip` = tooltip
    )
  }

  htmltools::tagList(dep, tag_badge)
}

#' Glass Dropdown Input
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label Optional display label text.
#' @param label_icon Optional \code{icon()} for the label.
#' @param help_text Optional tooltip text to display on hover.
#' @param choices List of values to select from.
#'  Can be a named vector c("Label" = "val").
#' @param selected The initially selected value
#'  (must match the value, not the label).
#' @param disabled Logical. If \code{TRUE}, the dropdown is disabled on load.
#' @param color Color variant: "purple" (default) or "green".
#' @param width The width of the input.
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
glassDropdown <- function(inputId, # nolint
                          label = NULL,
                          label_icon = NULL,
                          help_text = NULL,
                          choices,
                          selected = NULL,
                          disabled = FALSE,
                          color = "purple",
                          width = "100%") {
  # --- Handle Named Vectors ---
  if (!is.null(names(choices))) {
    values <- as.character(choices)
    labels <- names(choices)
  } else {
    values <- as.character(choices)
    labels <- as.character(choices)
  }

  # --- Setting Default Selection ---
  if (is.null(selected)) {
    selected <- values[1]
  }
  selected_idx <- which(values == selected)[1]
  if (is.na(selected_idx)) {
    warning(
      paste(
        "Selected value '",
        selected,
        "' not found in choices. Defaulting to first."
      ),
      immediate. = TRUE
    )
    selected_idx <- 1
    selected <- values[1]
  }
  selected_label <- labels[selected_idx]

  # --- Handle Disabled State Classes ---
  container_cls <- "glass-dropdown-container"
  if (!is.null(color) && color %in% c("green", "purple")) {
    container_cls <- paste(container_cls, color)
  }
  btn_cls <- "glass-dropdown-btn"

  if (disabled) {
    container_cls <- paste(container_cls, "disabled")
    btn_cls <- paste(btn_cls, "disabled")
  }

  # --- Header Generation ---
  header_html <- NULL
  if (!is.null(label) || !is.null(label_icon)) {
    header_html <- htmltools::tags$div(
      class = "glass-dropdown-label-header",
      if (!is.null(label_icon)) {
        htmltools::tags$div(class = "glass-dropdown-label-icon", label_icon)
      },
      if (!is.null(label)) {
        htmltools::tags$span(class = "glass-dropdown-label-text", label)
      }
    )
  }

  # --- Generate Options ---
  options_html <- lapply(seq_along(values), function(i) {
    val <- values[i]
    lbl <- labels[i]
    is_selected <- (val == selected)
    class_string <- if (is_selected) {
      "glass-option selected"
    } else {
      "glass-option"
    }

    htmltools::tags$div(
      class = class_string,
      `data-value` = val,
      onclick = sprintf(
        "selectGlassOption('%s', '%s', '%s', this)",
        inputId,
        val,
        lbl
      ),
      lbl
    )
  })

  # Help Icon
  help_icon_html <- create_glass_help_icon(help_text, "dropdown")

  # --- Build UI (Wrap in a parent div to hold Header + Dropdown) ---
  ui_structure <- htmltools::tags$div(
    style = paste0("width: ", width, ";"),
    # Help Icon
    help_icon_html,
    # The Header & Header Icon
    header_html,

    # The Dropdown Container
    htmltools::tags$div(
      class = container_cls,
      style = "width: 100%;", # Fill the wrapper
      id = paste0("container-", inputId),
      `data-selected` = selected,
      htmltools::tags$button(
        id = paste0("btn-", inputId),
        class = btn_cls,
        onclick = sprintf("toggleGlassMenu('%s')", inputId),
        type = "button",
        htmltools::tags$span(
          id = paste0("label-", inputId),
          selected_label
        ),
        htmltools::tags$i(
          class = "fa fa-chevron-down",
          style = "float: right; font-size: 0.8em; margin-top: 4px;"
        )
      ),
      htmltools::tags$div(
        id = paste0("menu-", inputId),
        class = "glass-dropdown-menu",
        htmltools::tagList(options_html)
      )
    )
  )

  # --- Attach Dependencies ---
  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-dropdown",
      version = "1.0.0",
      src = c(
        file = system.file("assets", package = "EstimatingBiologicalVariation")
      ),
      script = "glass_dropdown.js",
      stylesheet = "glass_dropdown.css"
    )
  )
}

#' Update Glass Dropdown
#'
#' Updates the choices, selection, or disabled state of a glassDropdown on the client.
#'
#' @param session The Shiny session object.
#' @param inputId The id of the input object.
#' @param choices Optional. New list of choices (can be a named vector).
#' @param selected Optional. The value to select.
#' @param disabled Optional. TRUE to disable, FALSE to enable.
#' @param color Optional. Color variant: "green" or "purple".
#'
#' @export
updateGlassDropdown <- function(session, inputId, # nolint
                                choices = NULL,
                                selected = NULL,
                                disabled = NULL,
                                color = NULL) {
  fullId <- session$ns(inputId)
  message <- list(id = fullId)

  # Handle Choices (Split Labels/Values)
  if (!is.null(choices)) {
    if (!is.null(names(choices))) {
      values <- unname(as.character(choices))
      labels <- names(choices)
    } else {
      values <- as.character(choices)
      labels <- as.character(choices)
    }
    message$values <- values
    message$labels <- labels
  }

  # Handle Selection (NOT CHOICES!!)
  if (!is.null(selected)) {
    message$selected <- unname(as.character(selected))
  }

  # Handle Disabled State
  if (!is.null(disabled)) {
    message$disabled <- as.logical(disabled)
  }

  # Handle Color
  if (!is.null(color)) {
    message$color <- color
  }

  # Send E-mail to to JS
  session$sendCustomMessage("glass-dropdown-update", message)
}

#' Glass Action Button (Split Style) with Urgent Mode
#'
#' A custom, two-tone action button.
#' Left: White pill with Icon. Right: Purple gradient with Label.
#' Can be set to "Urgent" mode to display a bouncing arrow and text above.
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label The label to display.
#' @param icon An optional icon() to appear in the white section.
#' @param width The width of the button (e.g., '100%', '200px').
#' @param color A character string. "purple" (default), "green", or "white".
#' @param size Button size: "md" (default) or "sm" (compact).
#' @param icon_only Logical. If TRUE, hides the label section
#'  and shows only the icon.
#' @param icon_only_action Character. Hover behaviour for icon-only buttons:
#'   \code{"expand"} (default) smoothly reveals the label inside the capsule;
#'   \code{"tooltip"} shows the label as a floating tooltip below the button.
#' @param disabled Logical. Should the button be disabled on load?
#' @param urgent Logical. If TRUE, shows an animated arrow
#'  and text above the button.
#' @param urgent_text Character. The text to display when
#'  urgent (default "press me").
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
glassButton <- function(inputId, label, icon = NULL, width = NULL, # nolint
                        color = "purple", size = "md", icon_only = FALSE,
                        icon_only_action = "expand",
                        disabled = FALSE,
                        urgent = FALSE, urgent_text = "press me") {
  # Class String for the Button
  class_str <- "glass-btn"
  if (color == "green") class_str <- paste(class_str, "green")
  if (color == "white") class_str <- paste(class_str, "white")
  if (size == "sm") class_str <- paste(class_str, "glass-btn-sm")
  if (icon_only) {
    class_str <- paste(class_str, "glass-btn-icon-only")
    if (icon_only_action == "tooltip") {
      class_str <- paste(class_str, "glass-btn-icon-tooltip")
    }
  }
  if (disabled) class_str <- paste(class_str, "disabled")

  # Style String for the Button
  style_str <- ""
  if (!is.null(width)) style_str <- paste0("width: ", width, ";")

  # Icon Content
  icon_html <- if (!is.null(icon)) as.character(icon) else ""

  # Urgent Indicator Class
  urgent_class <- "urgent-indicator"
  if (urgent) urgent_class <- paste(urgent_class, "visible")

  # 1. Build the Urgent Indicator HTML
  urgent_html <- htmltools::tags$div(
    class = urgent_class,
    htmltools::tags$span(class = "urgent-text", urgent_text),
    htmltools::tags$div(
      class = "urgent-arrow",
      htmltools::HTML("&#9660;")
    ) # Down Arrow
  )

  # 2. Build the Button Structure
  btn_structure <- htmltools::tags$div(
    id = inputId,
    class = class_str,
    style = style_str,
    role = "button",

    # Part 1: The White Icon Area
    htmltools::tags$span(
      class = "glass-btn-icon",
      htmltools::HTML(icon_html)
    ),

    # Part 2: The Gradient Label Area
    htmltools::tags$span(
      class = "glass-btn-label",
      label
    )
  )

  # 3. Wrap them together
  # The wrapper handles the vertical stacking
  wrapper_attrs <- list(class = "glass-btn-wrapper")
  if (icon_only && icon_only_action == "tooltip") {
    wrapper_attrs[["data-tooltip"]] <- as.character(label)
  }
  ui_wrapper <- do.call(htmltools::tags$div, c(wrapper_attrs, list(
    urgent_html,
    btn_structure
  )))

  # Attach Dependencies
  htmltools::tagList(
    ui_wrapper,
    htmltools::htmlDependency(
      name = "glass-button",
      version = "1.0.0",
      src = c(
        file = system.file("assets", package = "EstimatingBiologicalVariation")
      ),
      script = "glass_button.js",
      stylesheet = "glass_button.css"
    )
  )
}

#' Glass Download Button (Custom Handler)
#'
#' A custom download button using the Glass theme.
#' NOTE: This does NOT use shiny's downloadHandler. It acts as an input button.
#' You must use \code{triggerGlassDownload()} on the server to send the file.
#'
#' @param inputId The input slot to track clicks and receive messages.
#' @param label The label to display.
#' @param icon An optional icon() to appear in the white section.
#' @param width The width of the button (e.g., '100%', '200px').
#' @param color A character string. "purple" (default), "green", or "white".
#' @param disabled Logical. Should the button be disabled on load?
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
glassDownloadButton <- function(inputId, # nolint
                                label = "Download",
                                icon = shiny::icon("download"),
                                width = NULL,
                                color = "purple",
                                disabled = FALSE) {
  # Build class string
  class_str <- "glass-download-btn"
  if (color == "green") {
    class_str <- paste(class_str, "green")
  }
  if (color == "white") {
    class_str <- paste(class_str, "white")
  }
  if (disabled) {
    class_str <- paste(class_str, "disabled")
  }

  # Build Style String
  style_str <- ""
  if (!is.null(width)) {
    style_str <- paste0("width: ", width, ";")
  }

  # Icon Handling
  icon_html <- if (!is.null(icon)) as.character(icon) else ""

  # Build HTML Structure
  # We use <a> but without href, creating a JS-controlled anchor.
  ui_structure <- htmltools::tags$a(
    id = inputId,
    class = class_str,
    style = style_str,
    role = "button",

    # Part 1: White Icon Area
    htmltools::tags$span(
      class = "glass-btn-icon",
      htmltools::HTML(icon_html)
    ),

    # Part 2: Gradient Label Area
    htmltools::tags$span(
      class = "glass-btn-label",
      label
    )
  )

  # Attach Dependencies
  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-button",
      version = "1.0.0",
      src = c(
        file = system.file("assets", package = "EstimatingBiologicalVariation")
      ),
      script = "glass_button.js",
      stylesheet = "glass_button.css"
    )
  )
}

#' Trigger Glass Download
#'
#' Sends a custom message to the glassDownloadButton to initiate a download.
#' Useful when generating files dynamically in an observeEvent.
#'
#' @param session Shiny session object.
#' @param inputId The ID of the glassDownloadButton.
#' @param url The URL of the file to download (path to www or temp file).
#' @param filename Optional. The name to save the file as.
#' @export
triggerGlassDownload <- function(session, inputId, # nolint
                                 url, filename = NULL) {
  message <- list(downloadUrl = url)
  if (!is.null(filename)) message$filename <- filename
  session$sendInputMessage(inputId, message)
}

#' Update Glass Button
#'
#' @param session A valid Shiny session object.
#' @param inputId The input slot that will be used to access the value.
#' @param label The label to display.
#' @param icon An optional icon() to appear in the white section.
#' @param color Update the color style ("purple", "green", "white").
#' @param disabled Logical. Should the button be disabled?
#' @param urgent Logical. Should the urgent animation be shown?
#' @param urgent_text Character. Update the urgent message text.
#' @export
updateGlassButton <- function(session, # nolint
                              inputId, # nolint
                              label = NULL,
                              icon = NULL,
                              color = NULL,
                              disabled = NULL,
                              urgent = NULL,
                              urgent_text = NULL) {
  message <- list()
  if (!is.null(label)) message$label <- label
  if (!is.null(icon)) message$icon <- as.character(icon)
  if (!is.null(color)) message$color <- color
  if (!is.null(disabled)) message$disabled <- as.logical(disabled)
  if (!is.null(urgent)) message$urgent <- as.logical(urgent)
  if (!is.null(urgent_text)) message$urgent_text <- as.character(urgent_text)

  session$sendInputMessage(inputId, message)
}

#' Update Glass Download Button
#'
#' Alias for updateGlassButton to clarify usage with download buttons.
#'
#' @param session A valid Shiny session object.
#' @param outputId The output slot
#'  (which is also the input ID for the button state).
#' @param label The label to display.
#' @param icon An optional icon() to appear in the white section.
#' @param color Update the color style ("purple", "green", "white").
#' @param disabled Logical. Should the button be disabled?
#' @export
updateGlassDownloadButton <- function(session, outputId, # nolint
                                      label = NULL,
                                      icon = NULL,
                                      color = NULL,
                                      disabled = NULL) {
  updateGlassButton(
    session, outputId,
    label = label, icon = icon, color = color, disabled = disabled
  )
}

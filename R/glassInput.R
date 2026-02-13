#' Glass Text Input
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param value Initial value.
#' @param label_icon Optional \code{icon()} to place next to the label.
#' @param help_text Text to show when hovering the help-icon (top right).
#' @param placeholder The placeholder text inside the input.
#' @param tooltip_empty Text to show on hover when input is empty.
#' @param tooltip_filled Text to show on hover when input has content.
#' @param width The width of the input (e.g., "auto").
#' @param unit Optional text to display on the right side
#'  (e.g. "kg", "mmol/mol").
#' @param color Color variant: "green" (default) or "purple".
#' @param disabled Logical. Should it be disabled on load?
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
glassTextInput <- function(inputId, # nolint
                           label,
                           value = "",
                           label_icon = NULL,
                           help_text = NULL,
                           placeholder = "",
                           tooltip_empty = "Enter text",
                           tooltip_filled = "Text entered",
                           width = "100%",
                           unit = NULL,
                           color = "green",
                           disabled = FALSE) {
  glassInput(
    inputId = inputId,
    label = label,
    value = value,
    type = "text",
    label_icon = label_icon,
    help_text = help_text,
    placeholder = placeholder,
    tooltip_empty = tooltip_empty,
    tooltip_filled = tooltip_filled,
    width = width,
    unit = unit,
    color = color,
    disabled = disabled,
    is_numeric = FALSE
  )
}

#' Glass Numeric Input
#'
#' @param inputId The input slot.
#' @param label Display label.
#' @param value Initial value.
#' @param min Minimum allowed value (for HTML attribute).
#' @param max Maximum allowed value (for HTML attribute).
#' @param step Step size.
#' @param label_icon Optional \code{icon()}.
#' @param help_text Text to show on help icon hover.
#' @param placeholder Placeholder text.
#' @param tooltip_empty Text on hover when empty.
#' @param tooltip_filled Text on hover when filled.
#' @param width Width of input.
#' @param unit Optional unit string (e.g. "mg/L").
#' @param accept A numeric vector of length 2 \code{c(min, max)}
#' defining the acceptable range.
#' @param warning_unacceptable Text to display if value is outside
#' \code{accept} range.
#' @param color Color variant: "green" (default) or "purple".
#' @param disabled Logical.
#'
#' @export
glassNumericInput <- function(inputId, # nolint
                              label,
                              value,
                              min = NA,
                              max = NA,
                              step = NA,
                              label_icon = NULL,
                              help_text = NULL,
                              placeholder = "",
                              tooltip_empty = "Enter number",
                              tooltip_filled = "Number entered",
                              width = "100%",
                              unit = NULL,
                              accept = c(-Inf, Inf),
                              warning_unacceptable = "Value is outside acceptable range", # nolint
                              color = "green",
                              disabled = FALSE) {
  # Create range string for display if accept range is finite
  range_text <- NULL
  if (all(is.finite(accept))) {
    range_text <- paste0(accept[1], " - ", accept[2])
  } else if (is.finite(accept[1])) {
    range_text <- paste0("> ", accept[1])
  } else if (is.finite(accept[2])) {
    range_text <- paste0("< ", accept[2])
  }

  # Build the base UI
  ui <- glassInput(
    inputId = inputId,
    label = label,
    value = value,
    type = "number",
    label_icon = label_icon,
    help_text = help_text,
    placeholder = placeholder,
    tooltip_empty = tooltip_empty,
    tooltip_filled = tooltip_filled,
    width = width,
    unit = unit,
    color = color,
    disabled = disabled,
    is_numeric = TRUE,
    min = min,
    max = max,
    step = step,
    range_text = range_text # Pass range text to internal helper
  )

  # Add specific data attributes for numeric validation on the wrapper
  ui[[1]]$attribs$`data-min` <- accept[1]
  ui[[1]]$attribs$`data-max` <- accept[2]

  # Inject the warning message container inside
  warning_div <- htmltools::tags$div(
    class = "glass-input-warning-msg",
    htmltools::tags$i(class = "fa fa-exclamation-triangle"),
    htmltools::tags$span(warning_unacceptable)
  )

  # Append warning div to the binding container
  ui[[1]]$children[[length(ui[[1]]$children) + 1]] <- warning_div

  ui
}


#' Internal Helper to build Glass Input structure
#' @noRd
glassInput <- function(inputId, # nolint
                       label,
                       value,
                       type,
                       label_icon,
                       help_text,
                       placeholder,
                       tooltip_empty,
                       tooltip_filled,
                       width,
                       unit,
                       color,
                       disabled,
                       is_numeric,
                       min = NA,
                       max = NA,
                       step = NA,
                       range_text = NULL) {
  # Container Classes
  # but mostly kept distinct class names.
  binding_class <- "glass-input-binding"
  if (is_numeric) {
    binding_class <- paste(binding_class, "glass-numeric")
  }
  if (disabled) {
    binding_class <- paste(binding_class, "disabled")
  }

  # Use inner_id for the actual <input> element, and inputId for the outer container
  inner_id <- paste0(inputId, "_inner")

  # Header (Label + Icon + Range Text)
  header_html <- NULL
  if (!is.null(label) || !is.null(label_icon)) {
    header_html <- htmltools::tags$div(
      class = "glass-input-header",
      if (!is.null(label_icon)) {
        htmltools::tags$div(class = "glass-input-label-icon", label_icon)
      },
      if (!is.null(label)) {
        htmltools::tags$label(
          class = "glass-input-label-text",
          `for` = inputId,
          label
        )
      },

      # Range Text Display (Right of label)
      if (!is.null(range_text)) {
        htmltools::tags$span(class = "glass-input-range-text", range_text)
      }
    )
  }

  # 2. Help Icon (Top Right)
  help_icon_html <- NULL
  if (!is.null(help_text)) {
    help_icon_html <- htmltools::tags$div(
      class = "glass-input-help-icon",
      `data-help` = help_text,
      htmltools::tags$i(class = "fa fa-question-circle")
    )
  }

  # 3. Input Wrapper
  input_tag <- htmltools::tags$input(
    id = inner_id,
    type = type,
    class = "glass-native-input",
    value = value,
    placeholder = placeholder,
    disabled = if (disabled) "disabled" else NULL
  )

  if (is_numeric) {
    if (!is.na(min)) input_tag$attribs$min <- min
    if (!is.na(max)) input_tag$attribs$max <- max
    if (!is.na(step)) input_tag$attribs$step <- step
  }

  unit_html <- if (!is.null(unit)) {
    htmltools::tags$span(
      class = "glass-input-unit",
      unit
    )
  } else {
    NULL
  }

  # Build wrapper classes with color variant
  wrapper_classes <- "glass-input-wrapper"
  if (!is.null(color) && color %in% c("green", "purple")) {
    wrapper_classes <- paste(wrapper_classes, color)
  }

  wrapper_html <- htmltools::tags$div(
    class = wrapper_classes,
    `data-tip-empty` = tooltip_empty,
    `data-tip-filled` = tooltip_filled,
    `data-tooltip` = tooltip_empty, # Initial state
    input_tag,
    unit_html
  )

  # Combine elements
  ui_structure <- htmltools::tags$div(
    class = binding_class,
    style = paste0("width: ", width, ";"),
    id = inputId, # The binding attaches here ONLY
    help_icon_html,
    header_html,
    wrapper_html
  )

  # Attach Dependency
  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-input",
      version = "1.0.1",
      src = c(
        file = system.file("assets", package = "EstimatingBiologicalVariation")
      ),
      script = "glass_input.js",
      stylesheet = "glass_input.css"
    )
  )
}

#' Update Glass Text Input
#'
#' @param session A valid shiny session object.
#' @param inputId The local inputId (not the full ID)
#' @param label The label for the \code{glassTextInput(...)}
#' @param value The current text in the input field in
#'  \code{glassTextInput(...)}
#' @param label_icon The \code{icon(...)} just before the \code{label}
#' @param disabled A \code{logical} value.
#' @param color Color variant: "green" or "purple".
#'
#' @export
updateGlassTextInput <- function(session, inputId, # nolint
                                 label = NULL,
                                 value = NULL,
                                 label_icon = NULL,
                                 disabled = NULL,
                                 color = NULL) {
  fullId <- session$ns(inputId) # nolint
  message <- list(id = fullId)

  if (!is.null(label)) message$label <- label
  if (!is.null(value)) message$value <- value
  if (!is.null(label_icon)) message$label_icon <- as.character(label_icon)
  if (!is.null(disabled)) message$disabled <- as.logical(disabled)
  if (!is.null(color)) message$color <- color

  session$sendInputMessage(inputId, message)
}

#' Update Glass Numeric Input
#'
#' @param session A valid shiny session object
#' @param inputId The local inputId (not the full ID)
#' @param label The label for the \code{glassTextInput(...)}
#' @param value The current text in the input field in
#'  \code{glassTextInput(...)}
#' @param accept The acceptable interval for \code{value} to be in.
#' @param disabled A \code{logical} value.
#' @param color Color variant: "green" or "purple".
#'
#' @export
updateGlassNumericInput <- function(session, inputId, # nolint
                                    label = NULL,
                                    value = NULL,
                                    accept = NULL,
                                    disabled = NULL,
                                    color = NULL) {
  # Construct the full input ID using session namespace
  fullId <- session$ns(inputId) # nolint
  message <- list(id = fullId)

  if (!is.null(label)) message$label <- label
  if (!is.null(value)) message$value <- value
  if (!is.null(accept)) message$accept <- accept
  if (!is.null(disabled)) message$disabled <- as.logical(disabled)
  if (!is.null(color)) message$color <- color

  # Send E-mail to JS Handler to Update the Input
  session$sendInputMessage(inputId, message)
}

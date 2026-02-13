#' Glass Selectize Input
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label Optional display label text.
#' @param label_icon Optional \code{icon()} for the label.
#' @param help_text Optional tooltip text to display on hover.
#' @param choices List of values to select from.
#'  Can be a named vector c("Label" = "val").
#' @param selected The initially selected value(s).
#' @param multiple Logical. Allow multiple selections? Default FALSE.
#' @param placeholder Placeholder text when nothing is selected.
#' @param additionalPlaceholder Placeholder text when items
#'  are already selected (e.g., "Select another").
#' @param width The width of the input.
#' @param color Color variant: "purple" (default) or "green".
#' @param maxItems Maximum number of items that can be selected
#'  (only for multiple = TRUE).
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @importFrom jsonlite toJSON
#' @export
glassSelectizeInput <- function(inputId, # nolint
                                label = NULL,
                                label_icon = NULL,
                                help_text = NULL,
                                choices = NULL,
                                selected = NULL,
                                multiple = FALSE,
                                placeholder = 
                                  "Select an option...",
                                additionalPlaceholder = 
                                  "Select another...",
                                width = "100%",
                                color = "purple",
                                maxItems = NULL) {

  # --- Handle Choices ---
  if (!is.null(choices)) {
    if (!is.null(names(choices))) {
      values <- unname(as.character(choices))
      labels <- names(choices)
    } else {
      values <- as.character(choices)
      labels <- as.character(choices)
    }
  } else {
    values <- character(0)
    labels <- character(0)
  }

  # --- Handle Selection ---
  if (!is.null(selected)) {
    selected <- unname(as.character(selected))
  } else {
    selected <- character(0)
  }

  # --- Header Generation ---
  header_html <- NULL
  if (!is.null(label) || !is.null(label_icon)) {
    header_html <- htmltools::tags$div(
      class = "glass-selectize-label-header",
      if (!is.null(label_icon)) {
        htmltools::tags$div(
          class = "glass-selectize-label-icon",
          label_icon
        )
      },
      if (!is.null(label)) {
        htmltools::tags$span(
          class = "glass-selectize-label-text",
          label
        )
      }
    )
  }

  # Help Icon
  help_icon_html <- create_glass_help_icon(help_text, "selectize")

  # --- Build Options Data ---
  options_data <- lapply(seq_along(values), function(i) {
    list(
      value = as.character(values[i]),
      label = as.character(labels[i])
    )
  })

  # Serialize options to JSON (handle empty case)
  if (length(options_data) == 0) {
    json_options <- "[]"
  } else {
    json_options <- jsonlite::toJSON(options_data, auto_unbox = TRUE)
  }

  # Build wrapper classes with color variant
  wrapper_classes <- "glass-selectize-wrapper"
  if (!is.null(color) && color %in% c("green", "purple")) {
    wrapper_classes <- paste(wrapper_classes, color)
  }

  # --- Build UI Structure ---
  ui_structure <- htmltools::tags$div(
    style = paste0("width: ", width, ";"),
    class = wrapper_classes,
    help_icon_html,
    header_html,
    htmltools::tags$div(
      id = inputId,
      class = "glass-selectize-container",
      `data-selected` = jsonlite::toJSON(
        selected,
        auto_unbox = FALSE
      ),
      `data-options` = json_options,
      `data-multiple` = tolower(as.character(multiple)),
      `data-placeholder` = placeholder,
      `data-additional-placeholder` = additionalPlaceholder,
      `data-max-items` = if (!is.null(maxItems)) {
        maxItems
      } else {
        ""
      }
    )
  )

  # Attach Dependencies
  result <- htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-selectize",
      version = "1.0.0",
      src = c(
        file = system.file("assets", package = "EstimatingBiologicalVariation")
      ),
      script = "glass_selectize.js",
      stylesheet = "glass_selectize.css"
    )
  )

  return(result)
}

#' Update Glass Selectize Input
#'
#' Update a glass selectize input from the server.
#'
#' @param session The Shiny session object.
#' @param inputId The id of the selectize input to update.
#' @param choices Optional. New list of choices (can be a named vector).
#' @param selected Optional. The value(s) to select.
#' @param placeholder Optional. New placeholder text.
#'
#' @export
updateGlassSelectizeInput <- function(session, inputId, # nolint
                                      choices = NULL,
                                      selected = NULL,
                                      placeholder = NULL) {
  # Create the full input ID with namespace
  fullId <- session$ns(inputId)
  message <- list(id = fullId)

  # Handle Choices
  if (!is.null(choices)) {
    if (!is.null(names(choices))) {
      values <- unname(as.character(choices))
      labels <- names(choices)
    } else {
      values <- as.character(choices)
      labels <- as.character(choices)
    }

    options_data <- lapply(seq_along(values), function(i) {
      list(value = values[i], label = labels[i])
    })
    message$options <- options_data
  }

  # Handle Selection
  if (!is.null(selected)) {
    message$selected <- unname(as.character(selected))
  }

  # Handle Placeholder
  if (!is.null(placeholder)) {
    message$placeholder <- as.character(placeholder)
  }

  # Send E-mail to JS Handler to Update the Selectize Input
  session$sendCustomMessage("glass-selectize-update", message)
}

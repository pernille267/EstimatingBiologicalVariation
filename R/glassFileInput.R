#' Glass File Input
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
#' @param label_icon Optional icon() to place next to the label.
#' @param accept A character vector of MIME types or file extensions
#'  (e.g. c(".csv", ".xlsx")).
#' @param button_label The text on the browse button.
#' @param button_label_icon An shiny::icon() for the browse button.
#' @param placeholder The text to display before a file is selected.
#' @param tooltip_empty Text to show when hovering
#'  the text area before upload.
#' @param tooltip_filled Text to show when hovering
#'  the text area after file selection.
#' @param help_text Text to show when hovering the help-icon.
#' @param width The width of the input.
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
glassFileInput <- function(inputId, # nolint
                           label,
                           label_icon = NULL,
                           accept = NULL,
                           button_label = "Browse...",
                           button_label_icon = NULL,
                           placeholder = "No file selected",
                           tooltip_empty = "Please select a file",
                           tooltip_filled = "File selected",
                           help_text = NULL,
                           width = "100%") {
  accept_str <- if (!is.null(accept)) {
    gsub(" ", "", paste(accept, collapse = ","))
  } else {
    ""
  }
  assets_path <- system.file("assets", package = "EstimatingBiologicalVariation")

  ui <- htmltools::tags$div(
    class = "glass-file-binding form-group",
    id = inputId,
    style = paste0("width: ", width, ";"),

    # THE HEADER (Mimicking .glass-card-header)
    htmltools::tags$div(
      class = "glass-header-row",

      # Title Wrapper (Icon + Text)
      htmltools::tags$div(
        class = "glass-label-wrap",

        # Icon Bubble
        if (!is.null(label_icon)) {
          htmltools::tags$div(class = "label-icon", label_icon)
        },

        # Label Text
        if (!is.null(label)) {
          htmltools::tags$span(class = "label-text", label)
        }
      ),

      # Help Icon (Right aligned)
      if (!is.null(help_text)) {
        htmltools::tags$div(
          class = "glass-file-help-icon",
          `data-glass-help` = help_text,
          htmltools::tags$i(class = "fa fa-question-circle")
        )
      }
    ),

    # THE INPUT CONTAINER
    htmltools::tags$div(
      class = "glass-file-container",
      htmltools::tags$input(
        type = "file",
        class = "glass-native-input",
        style = "display: none;",
        accept = accept_str
      ),
      htmltools::tags$div(
        class = "glass-file-btn",
        if (!is.null(button_label_icon)) {
          htmltools::tags$span(
            class = "btn-icon",
            button_label_icon
          )
        },
        htmltools::tags$span(button_label)
      ),
      htmltools::tags$div(
        class = "glass-file-text-zone",
        `data-tooltip-empty` = tooltip_empty,
        `data-tooltip-filled` = tooltip_filled,
        `data-state` = "empty",
        htmltools::tags$span(
          class = "glass-file-filename",
          `data-placeholder` = placeholder,
          placeholder
        )
      ),
      htmltools::tags$div(
        class = "glass-progress-track",
        htmltools::tags$div(class = "glass-bar-fill")
      )
    )
  )

  htmltools::tagList(
    ui,
    htmltools::htmlDependency(
      name = "glass-file-input",
      version = "1.0.0",
      src = c(file = assets_path),
      script = "glass_file_input.js",
      stylesheet = "glass_file_input.css"
    )
  )
}

#' Helper to process Glass File in R
#'
#' @param input_data The value from input$yourId
#' @export
processGlassFile <- function(input_data) {
  if (is.null(input_data)) {
    return(NULL)
  }

  # The content comes as "data:text/csv;base64,....."
  # We need to strip the header
  parts <- strsplit(input_data$content, ",")[[1]]
  payload <- parts[2]

  # Create temp file
  tmp_path <- tempfile(
    fileext = paste0(".", tools::file_ext(input_data$name))
  )

  # Decode and write to temp file
  conn <- file(tmp_path, "wb")
  base64enc::base64decode(payload, output = conn)
  close(conn)

  # Return dataframe structure matching standard fileInput
  data.frame(
    name = input_data$name,
    size = input_data$size,
    type = input_data$type,
    datapath = tmp_path,
    stringsAsFactors = FALSE
  )
}

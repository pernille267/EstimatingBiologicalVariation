#' Glass Slider Input
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label Optional label for the slider input.
#' @param label_icon Optional \code{icon()} to display next to the label
#' @param help_text Optional tooltip text to display on hover.
#' @param choices Vector of values (numbers or strings).
#' @param selected The initially selected value.
#' @param unit Optional string to append to values (e.g. " dpi").
#' @param color Color variant: "purple" (default) or "green".
#' @param width The width of the input, e.g., '100%'.
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @importFrom jsonlite toJSON
#' @export
glassSlider <- function(inputId, # nolint
                        label = NULL,
                        label_icon = NULL,
                        help_text = NULL,
                        choices,
                        selected = NULL,
                        unit = "",
                        color = "purple",
                        width = "100%") {
  
  # Handle Choices and Selection
  if (is.null(selected)) {
    selected <- choices[1]
  }
  # This should be impossible due to validation in the JS,
  # but we check just in case
  if (!selected %in% choices) {
    stop(
      paste(
        "Selected value '",
        selected,
        "' must be in choices"
      )
    )
  }

  # Calculate selected index for JS (0-based)
  selected_idx <- which(choices == selected) - 1

  # Generate Labels with Specific Edge Classes
  labels_html <- lapply(seq_along(choices), function(i) {
    is_first <- (i == 1)
    is_last <- (i == length(choices))

    # Always show first and last labels!
    if (is_first || is_last) {
      extra_class <- ""
      if (is_first) {
        extra_class <- "glass-label-first"
      }
      if (is_last) {
        extra_class <- "glass-label-last"
      }

      # Position calculation
      pos_calc <- (i - 1) * (100 / (length(choices) - 1))

      htmltools::tags$div(
        class = paste("glass-slider-label", extra_class),
        style = paste0("left: ", pos_calc, "%;"),
        `data-index` = i - 1,
        paste0(choices[i], unit)
      )
    } else {
      NULL
    }
  })

  # --- Header Generation ---
  header_html <- NULL
  if (!is.null(label) || !is.null(label_icon)) {
    header_html <- htmltools::tags$div(
      class = "glass-slider-label-header",
      if (!is.null(label_icon)) {
        htmltools::tags$div(
          class = "glass-slider-label-icon",
          label_icon
        )
      },
      if (!is.null(label)) {
        htmltools::tags$span(
          class = "glass-slider-label-text",
          label
        )
      }
    )
  }

  # --- Help Icon Generation ---
  help_icon_html <- create_glass_help_icon(help_text, "slider")

  # Build wrapper classes with color variant
  wrapper_classes <- "glass-slider-container"
  if (!is.null(color) && color %in% c("green", "purple")) {
    wrapper_classes <- paste(wrapper_classes, color)
  }

  # --- Build UI (Wrap in a parent div to hold Header + Slider) ---
  # NEED FIX: form-group is a CSS class that is not under our control,
  # so we need to replace it with our own class at a later stage to avoid
  # conflicts with Bootstrap styles.
  ui_structure <- htmltools::tags$div(
    class = "form-group shiny-input-container",
    style = paste0(
      "width: ", width,
      "; margin-bottom: 5px;"
    ), # Temporary inline style for spacing for overriding Bootstrap's default
    # margin-bottom on form-group

    # Help Icon
    help_icon_html,

    # Header & Header Icon
    header_html,
    htmltools::tags$div(
      id = paste0("container-", inputId),
      class = wrapper_classes,
      `data-choices` = jsonlite::toJSON(unname(choices), auto_unbox = TRUE),
      `data-selected-index` = selected_idx,
      `data-unit` = unit,
      htmltools::tags$div(class = "glass-slider-track"),
      htmltools::tags$div(class = "glass-slider-fill"),
      htmltools::tags$div(
        class = "glass-slider-handle",
        htmltools::tags$div(
          class = "glass-slider-tooltip",
          paste0(selected, unit)
        )
      ),
      htmltools::tagList(labels_html)
    )
  )

  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-slider",
      version = "1.0.1",
      src = c(
        file = system.file("assets", package = "EstimatingBiologicalVariation")
      ),
      script = "glass_slider.js",
      stylesheet = "glass_slider.css"
    )
  )
}

# NEED ADD: Update function for glassSlider (similar to other inputs)
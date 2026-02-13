# UI Helper Functions
# ------------------------------------------------------------------------------

#' Create a parameter row for hyperparameter input
#'
#' @param id_stem Base ID for the input elements
#' @param label_latex LaTeX formatted label
#' @param val_expected Default expected value
#' @param min_expected Minimum expected value
#' @param max_expected Maximum expected value
#' @param step_expected Step size for expected value
#' @param val_weakness Default weakness value
#' @param min_weakness Minimum weakness value
#' @param max_weakness Maximum weakness value
#' @param step_weakness Step size for weakness value
#' @param ns Optional namespace function for module usage
#' @param show_headers Logical; if TRUE, displays column headers above the row
#' @return Shiny div with parameter inputs
#' @export
create_parameter_row <- function(id_stem, label_latex,
                                 val_expected, min_expected, max_expected, step_expected,
                                 val_weakness, min_weakness, max_weakness, step_weakness,
                                 ns = NULL, show_headers = FALSE) {
  # Apply namespace if provided
  id_fn <- if (!is.null(ns)) ns else identity

  glassRow(
    glassCol(
      width = 3,
      label = if (show_headers) "Parameter" else NULL,
      label_position = "center",
      label_underline = show_headers,
      h5(label_latex)
    ),
    glassCol(
      width = 4,
      label = if (show_headers) "Mean" else NULL,
      label_icon = if (show_headers) icon("chart-line") else NULL,
      label_position = "center",
      label_underline = show_headers,
      glassNumericInput(
        inputId = id_fn(paste0(id_stem, "_expected")),
        label = NULL,
        value = val_expected,
        min = min_expected,
        max = max_expected,
        step = step_expected
      )
    ),
    glassCol(
      width = 4,
      label = if (show_headers) "SD Factor" else NULL,
      label_icon = if (show_headers) icon("wave-square") else NULL,
      label_position = "center",
      label_underline = show_headers,
      glassNumericInput(
        inputId = id_fn(paste0(id_stem, "_weakness")),
        label = NULL,
        value = val_weakness,
        min = min_weakness,
        max = max_weakness,
        step = step_weakness
      )
    )
  )
}

#' Create navigation buttons for wizard steps
#'
#' @param prev_id ID for previous button (NULL to hide)
#' @param next_id ID for next button (NULL to hide)
#' @return Shiny div with navigation buttons
#' @export
create_wizard_nav_buttons <- function(prev_id = NULL, next_id = NULL) {
  shiny::div(
    class = "button-container",
    style = "display: flex; justify-content: space-between; margin-top: 20px;",
    if (!is.null(prev_id)) {
      glassButton(
        inputId = prev_id,
        label = "Previous",
        icon = icon("arrow-left"),
        color = "white"
      )
    } else {
      shiny::div()
    },
    if (!is.null(next_id)) {
      glassButton(
        inputId = next_id,
        label = "Next",
        icon = icon("arrow-right"),
        color = "purple"
      )
    }
  )
}

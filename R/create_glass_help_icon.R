#' Helper to generate help icon and tooltip HTML structure
#' MUST FIX: This obviously does not work as intended yet.
#' The tooltip icon placement is not correct. Ideally, it should
#' appear in the top-right corner of the glass element.
#' but it float somewhere else instead...
#'
#' @param help_text character string or NULL.
#' @param glass_element character string or NULL.
#' @return HTML tag object or NULL
#' @noRd
create_glass_help_icon <- function(help_text, glass_element) {
  if (is.null(help_text) || is.na(help_text) || help_text == "") {
    return(NULL)
  }

  if (is.null(glass_element) || is.na(glass_element) || glass_element == "") {
    return(NULL)
  }

  # NEED FIX: we need to add more classes.
  # What about glassButton, glassCard, glassCol?
  class_suffix <- switch(glass_element,
    "slider" = "glass-slider",
    "radio" = "glass-radio-btn",
    "dropdown" = "glass-dropdown",
    "selectize" = "glass-selectize",
    ""
  )

  htmltools::tags$div(
    class = paste0(class_suffix, "-", "help-icon"),
    # Using font-awesome circle-question icon
    htmltools::tags$i(class = "fa fa-circle-question"),
    htmltools::tags$div(
      class = paste0(class_suffix, "-", "tooltip-popup"),
      help_text
    )
  )
}

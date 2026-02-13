#' Glass Dashboard Card
#'
#' A clean, collapsible container with a glass-like theme.
#'
#' @param inputId The id of the card (useful for updates).
#' @param title The title text.
#' @param ... Content to put inside the card body.
#' @param icon An optional icon() to appear in the header.
#' @param toolbar Optional UI elements (e.g. buttons) to place
#'  in the header toolbar.
#' @param collapsible Logical. Can the user toggle visibility?
#' @param collapsed Logical. Should it start closed? (Only if collapsible=TRUE).
#' @param disabled Logical. Should the card be disabled (greyed out) on load?
#' @param attached Logical. If TRUE, removes top border-radius to blend with
#'  a boxed tabset above.
#' @param nested Logical. If TRUE, the card is styled as a nested section
#'  inside a parent glassCard: no margins, no shadow, no border-radius,
#'  and a subtler header to maintain visual hierarchy.
#' @param width The width (e.g., "100%").
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
glassCard <- function(inputId, # nolint
                      title,
                      ...,
                      icon = NULL,
                      toolbar = NULL,
                      collapsible = TRUE,
                      collapsed = FALSE,
                      disabled = FALSE,
                      attached = FALSE,
                      nested = FALSE,
                      width = "100%") {
  # Class Construction
  card_classes <- "glass-card"

  # Collapsible Classes
  if (isTRUE(collapsible)) {
    card_classes <- paste(card_classes, "collapsible")
    if (isTRUE(collapsed)) {
      card_classes <- paste(card_classes, "collapsed")
    }
  }

  # Disabled Class
  if (disabled) {
    card_classes <- paste(card_classes, "disabled")
  }

  # Attached Class
  if (attached) {
    card_classes <- paste(card_classes, "attached-top")
  }

  # Nested Class
  if (isTRUE(nested)) {
    card_classes <- paste(card_classes, "glass-card-nested")
  }

  # Handle Icon in Header (left side)
  icon_html <- if (!is.null(icon)) {
    htmltools::tags$div(
      class = "glass-card-icon",
      icon
    )
  } else {
    NULL
  }

  # Handle Toolbar (right side)
  toolbar_html <- if (!is.null(toolbar)) {
    htmltools::tags$div(
      class = "glass-card-toolbar",
      toolbar
    )
  } else {
    NULL
  }

  # Collapse Icon (Chevron)
  # It should only appear if collapsible is TRUE
  # Also, the direction (up/down) is handled via CSS based on state
  # NEED FIX: the icon circle gets squished in some zoom levels
  toggle_html <- if (isTRUE(collapsible)) {
    htmltools::tags$i(class = "fa fa-chevron-up glass-card-toggle")
  } else {
    NULL
  }

  # Build HTML Structure
  ui_structure <- htmltools::tags$div(
    id = inputId,
    class = card_classes,
    style = paste0("width: ", width, ";"),

    # Header
    htmltools::tags$div(
      class = "glass-card-header",

      # Title Side (Left)
      htmltools::tags$div(
        class = "glass-card-title-wrap",
        icon_html,
        htmltools::tags$h4(class = "glass-card-title", title)
      ),

      # Right Side: Toolbar (left) + Toggle (right)
      htmltools::tags$div(
        class = "glass-card-header-right",
        toolbar_html,
        toggle_html
      )
    ),

    # Card Body
    htmltools::tags$div(
      class = "glass-card-body",
      htmltools::tagList(...)
    )
  )

  # Attach Dependencies
  # Question to self: Why is it that some dependencies work
  # without using a use### function, but others don't?
  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-card",
      version = "1.0.0",
      src = c(
        file = system.file("assets", package = "EstimatingBiologicalVariation")
      ),
      script = "glass_card.js",
      stylesheet = "glass_card.css"
    )
  )
}

#' Update Glass Dashboard Card
#'
#' Update the title, collapse state, or disabled state of a glassCard.
#' NEED FIX: Should be able to update content, toolbar, icon, and
#' collapsible state as well in the future.
#'
#' @param session The Shiny session object.
#' @param inputId The id of the card.
#' @param title Optional. New title text.
#' @param collapsed Optional. TRUE/FALSE.
#' @param disabled Optional. TRUE/FALSE.
#'
#' @export
updateGlassCard <- function(session, inputId, # nolint
                            title = NULL,
                            collapsed = NULL,
                            disabled = NULL) {
  # Handle Namespace
  fullId <- session$ns(inputId) # nolint

  message <- list(id = fullId)

  if (!is.null(title)) message$title <- as.character(title)
  if (!is.null(collapsed)) message$collapsed <- as.logical(collapsed)
  if (!is.null(disabled)) message$disabled <- as.logical(disabled)

  session$sendCustomMessage(
    "glass-card-update",
    message
  )
}

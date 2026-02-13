#' Glass Layout Shell
#'
#' @param title App Title. Displayed in header.
#' @param branding UI element for top-left. Typically a logo image.
#'  Can also include text, but that is less common.
#' @param sidebar UI element from `glassSidebar`. The navigation sidebar.
#'  Vertical icon-based navigation between `glassRoute` pages.
#' @param header_items UI elements for header. Typically status indicators,
#'  user profile menu, etc.
#' @param ... `glassRouter` content. The actual pages of the app.
#'  Corresponds to `tabName` in `glassNavItem`.
#'
#' @importFrom htmltools tags tagList htmlDependency
#' @export
glassPage <- function(title, branding, sidebar, header_items = NULL, ...) { # nolint

  ui <- htmltools::tags$div(
    class = "glass-shell",
    # Brand Area (Logo or App Name)
    htmltools::tags$div(
      class = "glass-brand",
      branding
    ),
    # Application Header
    htmltools::tags$div(
      class = "glass-header",
      htmltools::tags$div(class = "glass-app-title", title),
      htmltools::tags$div(class = "glass-header-extras", header_items)
    ),
    # Sidebar
    sidebar,
    # Content Router
    htmltools::tags$div(
      class = "glass-content-area container-fluid",
      htmltools::tagList(...)
    )
  )

  # Put it all together with dependencies
  htmltools::tagList(
    # Dependencies
    htmltools::htmlDependency(
      name = "glass-layout",
      version = "1.0.0",
      src = c(
        file = system.file("assets", package = "EstimatingBiologicalVariation")
      ),
      script = "glass_layout.js",
      stylesheet = c(
        "glass_layout.css",
        "general_styling.css"
      )
    ),
    # Common dependencies
    htmltools::tags$head(
      htmltools::tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css" # nolint
      ),
    ),
    ui
  )
}

#' Glass Sidebar Container
#'
#' @param inputId Shiny input ID to track navigation state.
#' @param ... `glassNavItem` elements.
#' @export
glassSidebar <- function(inputId, ...) { # nolint
  htmltools::tags$div(
    class = "glass-sidebar",
    `data-input-id` = inputId,
    ...
  )
}

#' Glass Navigation Item (Icon Button)
#'
#' @param tabName The ID of the page this button opens.
#' @param icon The icon to display.
#' @param title The tooltip text (or flyout header if flyout_items used).
#' @param active Logical. Set TRUE for the default start page.
#' @param flyout_items Optional list of `glassFlyoutItem()` elements.
#'  If provided, a flyout menu is created for the particular nav item.
#'  This may clean up the sidebar if there are many related pages.
#' @param card_id Optional. The full DOM ID of the glassCard whose title
#'  should update when a flyout item is selected. The title will become
#'  "base title \u2014 flyout label".
#' @export
glassNavItem <- function(tabName, # nolint
                         icon,
                         title,
                         active = FALSE,
                         flyout_items = NULL,
                         card_id = NULL) {
  classes <- "glass-nav-item"
  if (active) {
    classes <- paste(classes, "active")
  }

  # Inner content
  inner_content <- list(
    icon,
    htmltools::tags$div(class = "glass-nav-notification"),
    # Container for the sub-selection indicator (line + icon)
    htmltools::tags$div(class = "glass-sub-indicator")
  )

  # Check for Flyout Menu
  if (!is.null(flyout_items) && length(flyout_items) > 0) {
    flyout_menu <- htmltools::tags$div(
      class = "glass-nav-flyout",
      htmltools::tags$div(class = "flyout-header", title),
      htmltools::tagList(flyout_items)
    )
    inner_content <- c(inner_content, list(flyout_menu))
  }

  nav <- htmltools::tags$div(
    class = classes,
    `data-target` = tabName,
    `data-title` = title,
    inner_content
  )
  if (!is.null(card_id)) nav$attribs[["data-card-id"]] <- card_id
  nav
}

#' Glass Flyout Menu Item
#'
#' Acts in the same way as glassTabsetPanel's tabPanel.
#'
#' @param tabName The ID of the page/view to navigate to.
#' @param icon The icon to display.
#' @param label The text label.
#' @export
glassFlyoutItem <- function(tabName, # nolint
                            icon,
                            label,
                            tabset_id = NULL,
                            tab_value = NULL) {
  item <- htmltools::tags$div(
    class = "flyout-item",
    `data-target` = tabName,
    `data-label` = label,
    icon,
    label
  )
  if (!is.null(tabset_id)) {
    item$attribs[["data-tabset-id"]] <- tabset_id
  }
  if (!is.null(tab_value)) {
    item$attribs[["data-tab-value"]] <- tab_value
  }
  item
}

#' Glass Page (Route)
#'
#' A container for a single "tab" of content. Replaces tabPanel.
#'
#' @param title The ID used by navigation (must match tabName in navItem).
#' @param ... Content.
#' @export
glassRoute <- function(title, ...) { # nolint
  # Note: 'title' here acts as the ID/Value for routing
  htmltools::tags$div(
    class = "glass-page",
    `data-value` = title, # Used by JS router
    htmltools::tagList(...)
  )
}

#' Glass Grid Row
#'
#' A CSS Grid container that replaces `fluidRow`.
#' Automatically handles spacing (gap) between columns.
#'
#' @param ... Content (usually `glassCol` elements).
#' @param spacers Logical. If TRUE, subtle vertical gradient bars appear between
#'   adjacent columns. Useful for visually connecting related configuration
#'   panels. Default is FALSE.
#' @export
glassRow <- function(..., spacers = FALSE) { # nolint
  classes <- "glass-row"
  if (spacers) classes <- paste(classes, "glass-row-spacers")
  htmltools::tags$div(class = classes, ...)
}

#' Glass Grid Column
#'
#' A column within a `glassRow`.
#'
#' @param width Integer between 1 and 12. 12 is full width, 6 is half width.
#' @param label Optional text label for the column header. Default NULL.
#' @param label_icon Optional icon for the column header. Default NULL.
#' @param label_position Position of the label: "left", "center", or "right".
#' Default "left".
#' @param label_underline Logical. If TRUE, adds an underline to the label.
#' Default FALSE.
#' @param ... Content.
#' @export
glassCol <- function(width, # nolint
                     ...,
                     label = NULL,
                     label_icon = NULL, 
                     label_position = "left",
                     label_underline = FALSE) {
  
  # Clamp width between 1 and 12
  w <- max(1, min(12, as.integer(width)))

  # Build column content
  content <- list()

  # Add optional label header if label is provided
  if (!is.null(label)) {
    # Determine label position class
    position_class <- switch(label_position,
      "center" = "glass-col-label-center",
      "right" = "glass-col-label-right",
      "glass-col-label-left" # default
    )

    # Add underline class if needed
    label_classes <- c("glass-col-label-header", position_class)
    if (label_underline) {
      label_classes <- c(label_classes, "glass-col-label-underline")
    }

    # Build label header elements
    label_elements <- list()

    # Add icon if provided
    if (!is.null(label_icon)) {
      label_elements <- c(label_elements, list(
        htmltools::tags$div(class = "glass-col-label-icon", label_icon)
      ))
    }

    # Add label text
    label_elements <- c(label_elements, list(
      htmltools::tags$div(class = "glass-col-label-text", label)
    ))

    # Create label header
    label_header <- htmltools::tags$div(
      class = paste(label_classes, collapse = " "),
      label_elements
    )

    content <- c(content, list(label_header))
  }

  # Add user content
  content <- c(content, list(...))

  htmltools::tags$div(
    class = paste0("glass-col-", w),
    content
  )
}

#' Glass Horizontal Spacer
#'
#' A subtle gradient bar to visually connect vertically stacked cards.
#' Place between glassCard or glassResultCard elements.
#'
#' @return An HTML div element with the spacer styling.
#' @export
glassSpacer <- function() { # nolint
  htmltools::tags$div(class = "glass-spacer-h")
}

#' Glass Vertical Spacer
#'
#' A subtle gradient bar to visually connect side-by-side cards in a row.
#' Place between glassCol elements within a glassRow.
#'
#' @return An HTML div element with the vertical spacer styling.
#' @export
glassSpacerV <- function() { # nolint
  htmltools::tags$div(class = "glass-spacer-v")
}

#' Update Glass Sidebar Highlight
#'
#' Sends a custom message to the frontend to toggle the notification pulse
#' on a specific sidebar item. Useful for drawing user attention to a particular
#' sidebar tab. For example, when the user should move to a specific section.
#' This is purerly UX/UI related and does not change any app logic.
#'
#' @param session The Shiny session object.
#' @param tabName The `tabName` (data-target) of the item to highlight.
#' @param enable Logical. TRUE to start pulsing, FALSE to stop.
#'
#' @export
updateGlassSidebarHighlight <- function(session, tabName, enable = TRUE) { # nolint
  session$sendCustomMessage(
    type = "glass-sidebar-highlight",
    message = list(
      tabName = tabName,
      enable = enable
    )
  )
}

#' Sync Glass Flyout State
#'
#' Programmatically updates the card title and sidebar sub-indicator
#' to reflect a specific flyout selection. Useful when navigation occurs
#' via in-page buttons (e.g. wizard prev/next) rather than the flyout menu.
#'
#' @param session The Shiny session object.
#' @param navTarget The `tabName` (data-target) of the parent nav item.
#' @param flyoutLabel The label text of the flyout item to show.
#' @param flyoutIcon Font Awesome icon name (without "fa-" prefix) for
#'  the sub-indicator. E.g. "upload", "columns", "filter", "sliders".
#'
#' @export
syncGlassFlyout <- function(session, navTarget, flyoutLabel, flyoutIcon = NULL) { # nolint
  session$sendCustomMessage(
    type = "glass-sync-flyout",
    message = list(
      navTarget = navTarget,
      flyoutLabel = flyoutLabel,
      flyoutIcon = flyoutIcon
    )
  )
}

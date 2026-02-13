#' Glass Tabset Panel
#'
#' @param inputId The id of the input object.
#' @param ... One or more glassTabPanel() items.
#' @param selected The value of the tab to select initially.
#' @param color The active color theme: "purple" (default) or "green".
#' @param boxed Logical. If TRUE, the navigation bar gets a
#' distinct glass container style.
#' @param headless Logical. If TRUE, hides the navigation bar
#' (useful for side-nav control or wizards).
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
glassTabsetPanel <- function(inputId, # nolint
                             ...,
                             selected = NULL,
                             color = "purple",
                             boxed = FALSE,
                             headless = FALSE) {
  panels <- list(...)

  # ... (Navigation building continues unchanged)
  nav_items <- lapply(panels, function(panel) {
    val <- panel$attribs$`data-value`
    title <- panel$attribs$`data-title`
    icon_html <- panel$attribs$`data-icon`

    is_active <- FALSE
    if (!is.null(selected) && val == selected) {
      is_active <- TRUE
    } else if (is.null(selected) && val == panels[[1]]$attribs$`data-value`) {
      is_active <- TRUE
    }

    class_str <- if (is_active) "glass-tab-btn active" else "glass-tab-btn"

    htmltools::tags$div(
      class = class_str,
      `data-value` = val,
      onclick = sprintf("switchGlassTab('%s', '%s', this)", inputId, val),
      htmltools::HTML(paste0(icon_html, title))
    )
  })

  # Build Content
  content_panels <- lapply(panels, function(panel) {
    val <- panel$attribs$`data-value`
    is_active <- FALSE
    if (!is.null(selected) && val == selected) {
      is_active <- TRUE
    } else if (is.null(selected) && val == panels[[1]]$attribs$`data-value`) {
      is_active <- TRUE
    }

    if (is_active) {
      panel$attribs$class <- paste(
        panel$attribs$class,
        "active"
      )
    }

    return(panel) # nolint
  })

  # 3. Container Classes (Handle Color Theme)
  container_class <- "glass-tabset-container"
  if (color == "green") {
    container_class <- paste(container_class, "theme-green")
  }

  if (boxed) {
    container_class <- paste(container_class, "glass-tabset-boxed")
  }

  if (headless) {
    container_class <- paste(container_class, "headless")
  }

  ui_structure <- htmltools::tags$div(
    id = inputId,
    class = container_class,
    htmltools::tags$div(
      class = "glass-tab-nav",
      htmltools::tagList(nav_items)
    ),
    htmltools::tags$div(
      class = "glass-tab-content-wrapper",
      htmltools::tagList(content_panels)
    )
  )

  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-tabs",
      version = "1.0.0",
      src = c(file = system.file("assets", package = "EstimatingBiologicalVariation")),
      script = "glass_tabs.js",
      stylesheet = "glass_tabs.css"
    )
  )
}

#' Glass Tab Panel
#'
#' @param title A \code{character} string. The title of the glassTabPanel.
#' @param ... HTML input.
#' @param value A \code{character} string. The id of the glassTabPanel.
#' @param icon A \code{icon(...)} appearing just before \code{title}.
#'
#' @export
glassTabPanel <- function(title, ..., value = title, icon = NULL) { # nolint
  icon_html <- if (!is.null(icon)) {
    as.character(icon)
  } else {
    ""
  }
  htmltools::tags$div(
    class = "glass-tab-pane",
    `data-value` = value,
    `data-title` = title,
    `data-icon` = icon_html,
    htmltools::tagList(...)
  )
}

#' Update Glass Tabset Panel (Switch Tab)
#'
#' Sends a custom message to switch the active tab in a glassTabsetPanel.
#'
#' @param session The shiny session object.
#' @param inputId The ID of the tabset panel (ns included).
#' @param selected The value of the tab to select.
#'
#' @export
updateGlassTabsetPanel <- function(session, inputId, selected) { # nolint
  session$sendCustomMessage(
    type = "glass-tab-switch",
    message = list(
      inputId = inputId,
      value = selected
    )
  )
}

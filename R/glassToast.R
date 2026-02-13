#' Glass Toast Dependencies
#'
#' Returns the HTML dependency for the glass toast notification system.
#' Include this in your UI to enable toast notifications.
#'
#' @return An htmlDependency object.
#'
#' @importFrom htmltools htmlDependency
#' @export
useGlassToast <- function() { # nolint
  htmltools::htmlDependency(
    name = "glass-toast",
    version = "1.0.0",
    src = c(
      file = system.file("assets", package = "EstimatingBiologicalVariation")
    ),
    script = "glass_toast.js",
    stylesheet = "glass_toast.css"
  )
}

#' Show a Glass Toast Notification
#'
#' Display a floating toast notification in the corner of the screen.
#' This is a custom alternative to shiny::showNotification.
#'
#' @param message The message to display in the toast. Can include HTML.
#' @param title Optional title for the toast.
#' @param type The type of notification: "message", "success",
#' "warning", or "error".
#' @param duration Duration in milliseconds before auto-dismiss.
#' Set to NULL or 0
#'   for persistent notifications that require manual dismissal.
#' @param closeButton Logical. Whether to show a close button. Default TRUE.
#' @param id Optional unique ID for the toast.
#' If not provided, one is generated.
#'   Use this to later remove a specific toast with `removeGlassToast()`.
#' @param icon Optional custom icon HTML.
#' If not provided, a default icon is used
#'   based on the type.
#' @param session The Shiny session object.
#'
#' @return The toast ID (invisibly).
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' showGlassToast("Operation completed successfully!", type = "success")
#'
#' # With title and longer duration
#' showGlassToast(
#'   message = "Your file has been uploaded.",
#'   title = "Upload Complete",
#'   type = "success",
#'   duration = 5000
#' )
#'
#' # Persistent notification (no auto-dismiss)
#' showGlassToast(
#'   message = "Please review the results carefully.",
#'   title = "Attention Required",
#'   type = "warning",
#'   duration = NULL,
#'   id = "review-notice"
#' )
#'
#' # Later remove it
#' removeGlassToast("review-notice")
#' }
#'
#' @export
showGlassToast <- function(message,
                           title = NULL,
                           type = c("message",
                                    "success",
                                    "warning",
                                    "error"),
                           duration = 4000,
                           closeButton = TRUE,
                           id = NULL,
                           icon = NULL,
                           session = shiny::getDefaultReactiveDomain()) {
  type <- match.arg(type)

  # Generate unique ID if not provided
  # What is the probability of a collision with this method? I guess
  # impossible almost.
  if (is.null(id)) {
    id <- paste0(
      "toast-",
      as.integer(round(as.numeric(Sys.time()))) * 1000,
      "-",
      sample(1000:9999, 1)
    )
  }

  # Prepare the message
  email_to_js <- list(
    action = "show",
    id = id,
    message = as.character(message),
    type = type,
    closeButton = closeButton
  )

  if (!is.null(title)) {
    email_to_js$title <- as.character(title)
  }

  if (!is.null(duration) && duration > 0) {
    email_to_js$duration <- as.numeric(duration)
  }

  if (!is.null(icon)) {
    email_to_js$icon <- as.character(icon)
  }

  session$sendCustomMessage("glassToast", email_to_js)

  invisible(id)
}

#' Remove a Glass Toast Notification
#'
#' Remove a specific toast notification by its ID.
#'
#' @param id The ID of the toast to remove.
#' @param session The Shiny session object.
#'
#' @export
removeGlassToast <- function(id, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    "glassToast",
    list(
      action = "remove",
      id = id
    )
  )
}

#' Remove All Glass Toast Notifications
#'
#' Remove all currently visible toast notifications.
#'
#' @param session The Shiny session object.
#'
#' @export
removeAllGlassToasts <- function(session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage(
    "glassToast",
    list(
      action = "removeAll"
    )
  )
}

#' Glass D3 Plot Dependencies
#'
#' Loads D3.js (v7) and the glass_d3_plots JS/CSS assets.
#' Call this once in your UI (e.g., in app.R or a layout wrapper).
#'
#' @return An \code{htmltools::tagList} containing the required dependencies.
#' @export
useGlassD3Plot <- function() { # nolint
  d3_head <- htmltools::tags$head(
    htmltools::tags$script(src = "https://d3js.org/d3.v7.min.js")
  )

  asset_path <- system.file("assets", package = "EstimatingBiologicalVariation")

  glass_dep <- htmltools::htmlDependency(
    name = "glass-d3-plots",
    version = "1.0.0",
    src = c(file = asset_path),
    script = "glass_d3_plots.js",
    stylesheet = "glass_d3_plots.css"
  )

  htmltools::tagList(d3_head, glass_dep)
}


#' Glass D3 Plot Container (UI)
#'
#' Creates a \code{<div>} container in which the D3 plot will be rendered.
#'
#' @param outputId A unique ID for this chart container.
#' @param width CSS width. Defaults to \code{"100\%"}.
#' @param height CSS height. Defaults to \code{"600px"}.
#'
#' @return An \code{htmltools::tags$div}.
#' @export
glassD3PlotOutput <- function(outputId, # nolint
                              width = "100%",
                              height = "600px") {
  htmltools::tags$div(
    id = outputId,
    class = "gd3-plot-container",
    style = paste0(
      "width:", width,
      "; height:", height,
      "; position: relative;"
    )
  )
}


#' Send D3 Plot Data to Client (Server)
#'
#' A convenient wrapper that sends a named payload to the JavaScript-side
#' \code{GlassD3Plot.render()} via \code{session$sendCustomMessage}.
#'
#' @param session The Shiny session object.
#' @param outputId The \strong{namespaced} element ID
#'  (use \code{session$ns(outputId)}).
#' @param plot_type A character string identifying the plot type. One of:
#'   \code{"subject_cvi"}, \code{"cvi_vs_concentration"}, \code{"cvi_vs_rcv"},
#'   \code{"trace"}, \code{"posterior"}, or \code{"prior"}.
#' @param data A named list containing the plot data.
#' @param params A named list of visual parameters (title, subtitle, etc.).
#'
#' @return Invisible \code{NULL}. Called for its side effect of sending data.
#' @export
updateGlassD3Plot <- function(session, outputId, # nolint
                              plot_type,
                              data,
                              params = list()) {
  session$sendCustomMessage(
    type = "update_glass_d3_plot",
    message = list(
      id = outputId,
      payload = list(
        plot_type = plot_type,
        data = data,
        params = params
      )
    )
  )
  invisible(NULL)
}


#' Observe and Auto-Update a Glass D3 Plot (Server)
#'
#' A reactive observer that watches an expression and automatically pushes
#' new data to the client-side D3 chart whenever the expression changes.
#'
#' @param outputId The element ID used in \code{glassD3PlotOutput()}.
#' @param expr An expression returning a list with elements:
#'   \code{plot_type}, \code{data}, and optionally \code{params}.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} already quoted?
#'
#' @export
observeGlassD3Plot <- function(outputId, # nolint 
                               expr,
                               env = parent.frame(),
                               quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env, quoted)
  session <- shiny::getDefaultReactiveDomain()

  if (is.null(session)) {
    stop("observeGlassD3Plot() must be called inside a Shiny server function.")
  }

  target_id <- session$ns(outputId)

  shiny::observe({
    payload_list <- func()
    shiny::req(payload_list)

    plot_type <- payload_list$plot_type %||% "subject_cvi"
    data_list <- payload_list$data %||% list()
    params_list <- payload_list$params %||% list()

    session$sendCustomMessage(
      type = "update_glass_d3_plot",
      message = list(
        id = target_id,
        payload = list(
          plot_type = plot_type,
          data = data_list,
          params = params_list
        )
      )
    )
  })
}


`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Glass Model Overview Grid
#'
#' Creates a reusable 2×2 grid layout for Bayesian model overview tabs.
#' Contains four panels: Analysis Data, Sampler Configuration,
#' Sampler Diagnostics, and Recommendations. Each panel wraps a
#' namespaced \code{uiOutput} so the calling module can populate
#' the content from its own server function.
#'
#' @param ns A namespace function (returned by \code{NS()}) from the
#'   calling module. Used to namespace the four output placeholders.
#' @param color Theme colour variant. One of \code{"purple"} (default)
#'   or \code{"green"}.
#'
#' @return A \code{tagList} containing the overview grid markup with
#'   attached CSS and JS dependencies.
#'
#' @details
#' Output IDs created inside the grid (all within \code{ns()}):
#' \itemize{
#'   \item \code{overview_data_summary}
#'   \item \code{overview_sampler_config}
#'   \item \code{overview_diagnostics}
#'   \item \code{overview_advice}
#' }
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @importFrom shiny icon uiOutput NS
#' @export
glassModelOverviewGrid <- function(ns, color = "purple") { # nolint

    theme_class <- if (identical(color, "green")) {
        "model-overview-grid theme-green"
    } else {
        "model-overview-grid"
    }

    # Build the 2×2 grid -------------------------------------------------
    grid_ui <- tags$div(
        class = theme_class,

        # ── Top-left: Analysis Data Summary ──
        tags$div(
            class = "overview-panel",
            tags$div(
                class = "overview-panel-header",
                icon("database"),
                tags$span("Analysis Data")
            ),
            tags$div(
                class = "overview-panel-body",
                uiOutput(ns("overview_data_summary"))
            )
        ),

        # ── Top-right: Sampler Configuration ──
        tags$div(
            class = "overview-panel",
            tags$div(
                class = "overview-panel-header",
                icon("sliders-h"),
                tags$span("Sampler Configuration")
            ),
            tags$div(
                class = "overview-panel-body",
                uiOutput(ns("overview_sampler_config"))
            )
        ),

        # ── Bottom-left: Sampler Diagnostics ──
        tags$div(
            class = "overview-panel",
            tags$div(
                class = "overview-panel-header",
                icon("stethoscope"),
                tags$span("Sampler Diagnostics")
            ),
            tags$div(
                class = "overview-panel-body",
                uiOutput(ns("overview_diagnostics"))
            )
        ),

        # ── Bottom-right: Recommendations ──
        tags$div(
            class = "overview-panel",
            tags$div(
                class = "overview-panel-header",
                icon("lightbulb"),
                tags$span("Recommendations")
            ),
            tags$div(
                class = "overview-panel-body",
                uiOutput(ns("overview_advice"))
            )
        )
    )

    # Return UI + dependency bundle ---------------------------------------
    tagList(
        grid_ui,
        htmlDependency(
            name = "glass-model-overview",
            version = "1.0.0",
            src = c(file = system.file("assets", package = "EstimatingBiologicalVariation")),
            stylesheet = "glass_model_overview.css",
            script = "glass_model_overview.js"
        )
    )
}

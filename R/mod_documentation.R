# Documentation Module
# ------------------------------------------------------------------------------
#
# Displays application documentation and model descriptions.
#
# ------------------------------------------------------------------------------

#' Documentation Module UI
#'
#' @param id Namespace ID for the module
#' @return Shiny UI elements for the documentation page
mod_documentation_ui <- function(id) {
    ns <- NS(id)
    glassCard(
        inputId = ns("docs_card"),
        title = "Documentation",
        icon = icon("book"),
        glassRow(
            glassCol(
                12,
                h3("About This Application"),
                p("This application estimates biological variation using Bayesian statistical models."),
                h4("Models Available:"),
                tags$ul(
                    tags$li("Model 1: Simple NTT (Nested Two-way ANOVA)"),
                    tags$li("Model 2: NTTDFGAM (with Degrees of Freedom and Gamma distributions)")
                )
            )
        )
    )
}

#' Documentation Module Server
#'
#' @param id Namespace ID for the module
mod_documentation_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # Placeholder for future dynamic documentation content
    })
}

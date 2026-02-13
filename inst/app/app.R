# ******************************************************************************
# Biological Variation Analysis - Package App Launcher
# ******************************************************************************
#
# This file is the Shiny entry-point shipped inside inst/app/.
# When the package is installed, run_app() calls shiny::runApp() on this
# directory.  All R code is loaded from the package namespace — no sourcing
# needed.
# ******************************************************************************

# The UI and server are defined here because shiny::runApp() expects either
# app.R or ui.R + server.R in the target directory.

ui <- EstimatingBiologicalVariation:::app_ui()
server <- EstimatingBiologicalVariation:::app_server

shinyApp(ui = ui, server = server)

# Hyperparameters Module
# ------------------------------------------------------------------------------

#' Hyperparameters Module UI
#'
#' @param id Namespace ID for the module
#' @return Shiny UI elements
mod_hyperparameters_ui <- function(id) {
  ns <- shiny::NS(id)
  glassRow(
    glassCol(
      width = 6,
      glassCard(
        inputId = ns("hyperparams_card"),
        title = "Set Hyperparameters",
        icon = icon("sliders-h"),
        collapsed = FALSE,
        collapsible = FALSE,
        attached = TRUE,
        nested = TRUE,
        create_parameter_row(
          "beta",
          "\\(\\beta\\)",
          val_expected = 7,
          min_expected = -1000,
          max_expected = 1e6,
          step_expected = 0.01,
          val_weakness = 0.5,
          min_weakness = 0.1,
          max_weakness = 4,
          step_weakness = 0.1,
          ns = ns,
          show_headers = TRUE
        ),
        create_parameter_row(
          "cvi",
          "\\(\\mathrm{CV}_{I}(\\%)\\)",
          val_expected = 9.4,
          min_expected = 0,
          max_expected = 80,
          step_expected = 0.1,
          val_weakness = 2,
          min_weakness = 1e-2,
          max_weakness = 4,
          step_weakness = 0.1,
          ns = ns
        ),
        create_parameter_row(
          "cva",
          "\\(\\mathrm{CV}_{A}(\\%)\\)",
          val_expected = 2.5,
          min_expected = 0,
          max_expected = 20,
          step_expected = 0.1,
          val_weakness = 2,
          min_weakness = 1e-2,
          max_weakness = 12,
          step_weakness = 0.2,
          ns = ns
        ),
        create_parameter_row(
          "cvg",
          "\\(\\mathrm{CV}_{G}(\\%)\\)",
          val_expected = 8.0,
          min_expected = 0,
          max_expected = 200,
          step_expected = 0.1,
          val_weakness = 2,
          min_weakness = 1e-2,
          max_weakness = 4,
          step_weakness = 0.1,
          ns = ns
        ),
        create_parameter_row(
          "dfi",
          "\\(\\mathrm{df}_{I}\\)",
          val_expected = 20,
          min_expected = 2,
          max_expected = 1000,
          step_expected = 1,
          val_weakness = round(1 / sqrt(2), 2L),
          min_weakness = 1e-2,
          max_weakness = 12,
          step_weakness = 0.01,
          ns = ns
        ),
        create_parameter_row(
          "dfa",
          "\\(\\mathrm{df}_{A}\\)",
          val_expected = 20,
          min_expected = 2,
          max_expected = 1000,
          step_expected = 1,
          val_weakness = round(1 / sqrt(2), 2L),
          min_weakness = 1e-2,
          max_weakness = 12,
          step_weakness = 0.01,
          ns = ns
        ),
        create_parameter_row(
          "hbhr",
          "\\(\\mathrm{HBHR}(\\%)\\)",
          val_expected = 50,
          min_expected = 2,
          max_expected = 200,
          step_expected = 1,
          val_weakness = round(2 / 3, 2L),
          min_weakness = 1e-2,
          max_weakness = 12,
          step_weakness = 0.01,
          ns = ns
        )
      )
    ),
    glassCol(
      width = 6,
      glassCard(
        inputId = ns("advanced_opts_card"),
        title = "Advanced Modelling Options",
        icon = icon("microchip"),
        collapsible = TRUE,
        collapsed = TRUE,
        attached = TRUE,
        nested = TRUE,
        glassRadioButtons(
          inputId = ns("log_transformed"),
          label = "Apply Log Transformation",
          label_icon = icon("square-root-alt"),
          choices = c("Yes", "No"),
          selected = "No"
        ),
        glassSlider(
          inputId = ns("iter"),
          label = "Number of Iterations",
          label_icon = icon("repeat"),
          choices = c(seq(100, 2500, by = 100), seq(3000, 7500, by = 500)),
          selected = 2500
        ),
        glassSlider(
          ns("burn"),
          label = "Burnin Fraction",
          label_icon = icon("fire"),
          choices = seq(0, 100),
          selected = 50,
          unit = "%"
        ),
        glassSlider(
          ns("nchains"),
          label = "Number of Chains",
          label_icon = icon("link"),
          choices = seq(2, 16),
          selected = 4
        ),
        glassSlider(
          ns("adapt_delta"),
          label = "Acceptance Probability",
          label_icon = icon("percent"),
          choices = c(seq(75, 99), 99.5, 99.9),
          selected = 90,
          unit = "%"
        ),
        glassSlider(
          ns("max_treedepth"),
          label = "Maximum Treedepth",
          label_icon = icon("code-branch"),
          choices = seq(5, 20),
          selected = 12
        ),
        glassSlider(
          ns("number_of_cores"),
          label = "Number of Cores",
          label_icon = icon("microchip"),
          choices = seq(1, 16),
          selected = 4
        )
      )
    )
  )
}

#' Hyperparameters Module Server
#'
#' @param id Namespace ID for the module
#' @param filter_values Reactive list from filter module
#' (contains selected analyte)
#' @return List of reactive values for hyperparameters
mod_hyperparameters_server <- function(id, filter_values = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    # Auto-update hyperparameters based on selected analyte
    if (!is.null(filter_values)) {
      shiny::observeEvent(filter_values$filter_analyte(),
        {
          shiny::req(filter_values$filter_analyte())

          analyte <- filter_values$filter_analyte()
          if (!is.null(ANALYTE_PRIORS[[analyte]])) {
            priors <- ANALYTE_PRIORS[[analyte]]

            # Update all parameter inputs
            for (param_name in names(priors)) {
              input_id <- param_name
              if (input_id %in% c(
                "beta", "cvi", "cva", "cvg", "dfi", "dfa", "hbhr"
              )) {
                # These are in the main card - update both expected and weakness
                if (grepl("weakness", param_name)) {
                  param_base <- gsub("_weakness", "", param_name)
                  updateGlassNumericInput(
                    session = session,
                    inputId = paste0(param_base, "_weakness"),
                    value = priors[[param_name]]
                  )
                } else {
                  updateGlassNumericInput(
                    session = session,
                    inputId = paste0(param_name, "_expected"),
                    value = priors[[param_name]]
                  )
                }
              }
            }
          }
        },
        ignoreInit = TRUE
      )
    }

    # Return all hyperparameter values as reactives
    return( # nolint
      list(
        # Prior parameters
        beta_expected = shiny::reactive(input$beta_expected),
        beta_weakness = shiny::reactive(input$beta_weakness),
        cvi_expected = shiny::reactive(input$cvi_expected),
        cvi_weakness = shiny::reactive(input$cvi_weakness),
        cva_expected = shiny::reactive(input$cva_expected),
        cva_weakness = shiny::reactive(input$cva_weakness),
        cvg_expected = shiny::reactive(input$cvg_expected),
        cvg_weakness = shiny::reactive(input$cvg_weakness),
        dfi_expected = shiny::reactive(input$dfi_expected),
        dfi_weakness = shiny::reactive(input$dfi_weakness),
        dfa_expected = shiny::reactive(input$dfa_expected),
        dfa_weakness = shiny::reactive(input$dfa_weakness),
        hbhr_expected = shiny::reactive(input$hbhr_expected),
        hbhr_weakness = shiny::reactive(input$hbhr_weakness),

        # Advanced options
        log_transformed = shiny::reactive(isTRUE(input$log_transformed == "Yes")),
        iter = shiny::reactive(input$iter),
        burn = shiny::reactive(input$burn),
        nchains = shiny::reactive(input$nchains),
        adapt_delta = shiny::reactive(input$adapt_delta),
        max_treedepth = shiny::reactive(input$max_treedepth),
        number_of_cores = shiny::reactive(input$number_of_cores)
      )
    )
  })
}

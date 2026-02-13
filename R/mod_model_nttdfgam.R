# Model 2 - NTTDFGAM Module
# ------------------------------------------------------------------------------
#
# Handles the Model 2 (NTTDFGAM) analysis page:
#   - Run button to execute Bayesian NTTDFGAM analysis
#   - Overview tab via glassModelOverviewGrid (see glassModelOverview.R)
#   - Status indicator badge in the toolbar
#   - Results display: results table, subject plot,
#     and concentration plot
#
# ------------------------------------------------------------------------------

#' Model 2 (NTTDFGAM) Module UI
#'
#' @param id Namespace ID for the module
#' @return Shiny UI elements for the NTTDFGAM model results page
mod_model_nttdfgam_ui <- function(id) {
    ns <- NS(id)
    glassCard(
        inputId = ns("model2_card"),
        title = "Bayesian Models \u2014 NTTDFGAM",
        icon = icon("chart-area"),
        collapsible = FALSE,
        toolbar = tagList(
            uiOutput(ns("model_status_badge")),
            glassButton(
                inputId = ns("run_btn"),
                label = "Run NTTDFGAM Model",
                icon = icon("play"),
                color = "green"
            )
        ),
        glassTabsetPanel(
            inputId = ns("results_tabs"),
            selected = "overview",
            color = "green",
            glassTabPanel(
                title = "Overview",
                value = "overview",
                icon = icon("chart-area"),
                glassModelOverviewGrid(ns, color = "green")
            ),
            glassTabPanel(
                title = "Results Table",
                value = "table",
                icon = icon("table"),
                uiOutput(ns("results_table"))
            ),
            glassTabPanel(
                title = "Subject Plot",
                value = "subject",
                icon = icon("user"),
                glassD3PlotOutput(ns("subject_plot"), height = "600px")
            ),
            glassTabPanel(
                title = "Concentration Plot",
                value = "concentration",
                icon = icon("chart-line"),
                glassD3PlotOutput(ns("concentration_plot"), height = "600px")
            )
        )
    )
}

#' Model 2 (NTTDFGAM) Module Server
#'
#' @param id Namespace ID for the module
#' @param app_state reactiveValues object containing shared application state
mod_model_nttdfgam_server <- function(id, app_state) {
    moduleServer(id, function(input, output, session) {

        # =====================================================================
        # Run NTTDFGAM Model
        # =====================================================================

        observeEvent(input$run_btn, {
            tryCatch(
                {
                    app_state$model_nttdfgam_results <- run_bayesian_analysis(
                        model = app_state$compiled_model_nttdfgam,
                        model_name = "NTTDFGAM Model",
                        app_state = app_state
                    )

                    # Determine status based on warnings
                    if (length(app_state$model_nttdfgam_results$advice) > 0) {
                        app_state$model_nttdfgam_status <- "warning"
                    } else {
                        app_state$model_nttdfgam_status <- "success"
                    }
                },
                error = function(e) {
                    app_state$model_nttdfgam_status <- "error"
                    app_state$model_nttdfgam_error <- e$message
                    showGlassToast(
                        message = paste("NTTDFGAM model failed:", e$message),
                        title = "Analysis Error",
                        type = "error",
                        duration = NULL
                    )
                }
            )
        }, ignoreInit = TRUE)

        # =====================================================================
        # Status Badge
        # =====================================================================

        output$model_status_badge <- renderUI({
            status <- app_state$model_nttdfgam_status
            if (is.null(status)) status <- "not_run"

            badge_config <- switch(status,
                "not_run" = list(
                    class = "status-not-run",
                    icon  = "circle",
                    label = "Not yet run"
                ),
                "success" = list(
                    class = "status-success",
                    icon  = "circle-check",
                    label = "Successful"
                ),
                "warning" = list(
                    class = "status-warning",
                    icon  = "triangle-exclamation",
                    label = "Completed with warnings"
                ),
                "error" = list(
                    class = "status-error",
                    icon  = "circle-xmark",
                    label = "Failed"
                ),
                "stale" = list(
                    class = "status-stale",
                    icon  = "rotate",
                    label = "Rerun recommended"
                ),
                list(class = "status-not-run", icon = "circle", label = "Not yet run")
            )

            tags$div(
                class = paste("model-status-badge", badge_config$class),
                icon(badge_config$icon),
                tags$span(badge_config$label)
            )
        })

        # =====================================================================
        # Overview Panel: Analysis Data Summary (Top-Left)
        # =====================================================================

        output$overview_data_summary <- renderUI({
            data <- app_state$analysis_data

            if (is.null(data) || nrow(data) == 0) {
                return(tags$div(
                    class = "overview-awaiting",
                    icon("database"),
                    tags$span(class = "awaiting-text", "No analysis data available"),
                    tags$span(class = "awaiting-subtext",
                              "Complete the Analysis Setup to populate this panel")
                ))
            }

            n_subjects <- data.table::uniqueN(data$SubjectID)
            n_total_samples <- nrow(unique(data[, .(SubjectID, SampleID)]))
            avg_samples_per_subj <- round(n_total_samples / max(n_subjects, 1), 1)
            avg_reps_per_sample <- round(
                nrow(data) / max(n_total_samples, 1), 1
            )
            n_observations <- nrow(data)

            analyte_info  <- app_state$analyte_name %||% "\u2014"
            material_info <- app_state$material_name %||% "\u2014"
            sex_info      <- if (!is.null(app_state$selected_sex) &&
                                  length(app_state$selected_sex) > 0) {
                paste(app_state$selected_sex, collapse = ", ")
            } else {
                "\u2014"
            }
            group_info <- if (!is.null(app_state$group_name) &&
                              nzchar(app_state$group_name)) {
                app_state$group_name
            } else {
                "\u2014"
            }

            stat_row <- function(label, value) {
                tags$div(
                    class = "overview-stat-row",
                    tags$span(class = "overview-stat-label", label),
                    tags$span(class = "overview-stat-value", value)
                )
            }

            tagList(
                tags$div(class = "overview-section-title", "Design"),
                stat_row("Subjects", n_subjects),
                stat_row("Avg. samples / subject", avg_samples_per_subj),
                stat_row("Avg. replicates / sample", avg_reps_per_sample),
                stat_row("Total observations", format(n_observations, big.mark = ",")),
                tags$div(class = "overview-section-title", "Selections"),
                stat_row("Analyte", analyte_info),
                stat_row("Material", material_info),
                stat_row("Sex", sex_info),
                stat_row("Group", group_info)
            )
        })

        # =====================================================================
        # Overview Panel: Sampler Configuration (Top-Right)
        # =====================================================================

        output$overview_sampler_config <- renderUI({
            hyper_rows <- tagList(
                tags$tr(
                    tags$td("\\(\\beta\\)"),
                    tags$td(app_state$hyper_beta %||% "\u2014"),
                    tags$td(app_state$hyper_beta_weakness %||% "\u2014")
                ),
                tags$tr(
                    tags$td("\\(\\mathrm{CV}_I\\)"),
                    tags$td(app_state$hyper_cvi %||% "\u2014"),
                    tags$td(app_state$hyper_cvi_weakness %||% "\u2014")
                ),
                tags$tr(
                    tags$td("\\(\\mathrm{CV}_A\\)"),
                    tags$td(app_state$hyper_cva %||% "\u2014"),
                    tags$td(app_state$hyper_cva_weakness %||% "\u2014")
                ),
                tags$tr(
                    tags$td("\\(\\mathrm{CV}_G\\)"),
                    tags$td(app_state$hyper_cvg %||% "\u2014"),
                    tags$td(app_state$hyper_cvg_weakness %||% "\u2014")
                ),
                tags$tr(
                    tags$td("\\(\\mathrm{df}_I\\)"),
                    tags$td(app_state$hyper_dfi %||% "\u2014"),
                    tags$td(app_state$hyper_dfi_weakness %||% "\u2014")
                ),
                tags$tr(
                    tags$td("\\(\\mathrm{df}_A\\)"),
                    tags$td(app_state$hyper_dfa %||% "\u2014"),
                    tags$td(app_state$hyper_dfa_weakness %||% "\u2014")
                ),
                tags$tr(
                    tags$td("\\(\\mathrm{HBHR}\\)"),
                    tags$td(app_state$hyper_hbhr %||% "\u2014"),
                    tags$td(app_state$hyper_hbhr_weakness %||% "\u2014")
                )
            )

            stat_row <- function(label, value) {
                tags$div(
                    class = "overview-stat-row",
                    tags$span(class = "overview-stat-label", label),
                    tags$span(class = "overview-stat-value", value)
                )
            }

            log_tf <- if (isTRUE(app_state$log_transformed)) "Yes" else "No"
            n_iter <- app_state$num_iterations %||% 2500
            warmup_pct <- round((app_state$burn_in_fraction %||% 0.5) * 100)
            warmup_iter <- floor(n_iter * (app_state$burn_in_fraction %||% 0.5))

            tagList(
                tags$div(class = "overview-section-title", "Hyperparameters"),
                tags$table(
                    class = "overview-config-table",
                    tags$thead(
                        tags$tr(
                            tags$th("Parameter"),
                            tags$th("Mean"),
                            tags$th("SD Factor")
                        )
                    ),
                    tags$tbody(hyper_rows)
                ),
                tags$div(class = "overview-section-title", "Advanced Options"),
                stat_row("Log-transformed", log_tf),
                stat_row("Iterations", format(n_iter, big.mark = ",")),
                stat_row(
                    "Warmup",
                    paste0(format(warmup_iter, big.mark = ","),
                           " (", warmup_pct, "%)")
                ),
                stat_row("Chains", app_state$num_chains %||% 4),
                stat_row(
                    "Adapt delta",
                    paste0(round((app_state$adapt_delta %||% 0.9) * 100, 1), "%")
                ),
                stat_row("Max treedepth", app_state$max_treedepth %||% 12),
                stat_row("Cores", app_state$num_cores %||% 4)
            )
        })

        # =====================================================================
        # Overview Panel: Sampler Diagnostics (Bottom-Left)
        # =====================================================================

        output$overview_diagnostics <- renderUI({
            results <- app_state$model_nttdfgam_results

            if (is.null(results)) {
                return(tags$div(
                    class = "overview-awaiting",
                    icon("stethoscope"),
                    tags$span(class = "awaiting-text",
                              "Diagnostics will appear after the model is run"),
                    tags$span(class = "awaiting-subtext",
                              "Click \"Run NTTDFGAM Model\" to start the analysis")
                ))
            }

            diag <- results$sampler_diagnostics
            advice <- results$advice

            if (length(advice) == 0) {
                return(tagList(
                    tags$div(
                        class = "overview-diag-success",
                        icon("circle-check"),
                        tags$span("Sampling completed successfully \u2014 no warnings detected")
                    ),
                    tags$div(style = "margin-top: 14px;"),
                    build_diag_stats_ui(diag)
                ))
            }

            tagList(
                build_diag_stats_ui(diag),
                if (length(diag$poor_rhat_params) > 0) {
                    tags$div(
                        class = "overview-diag-params",
                        tags$div(class = "diag-params-title",
                                 "Parameters with poor convergence:"),
                        tags$span(paste(
                            lapply(diag$poor_rhat_params, function(p) {
                                as.character(tags$code(p))
                            }),
                            collapse = ", "
                        ) |> HTML())
                    )
                },
                if (length(diag$low_neff_params) > 0) {
                    tags$div(
                        class = "overview-diag-params",
                        style = "margin-top: 8px;",
                        tags$div(class = "diag-params-title",
                                 "Parameters with low effective sample size:"),
                        tags$span(paste(
                            lapply(diag$low_neff_params, function(p) {
                                as.character(tags$code(p))
                            }),
                            collapse = ", "
                        ) |> HTML())
                    )
                }
            )
        })

        # =====================================================================
        # Overview Panel: Recommendations (Bottom-Right)
        # =====================================================================

        output$overview_advice <- renderUI({
            results <- app_state$model_nttdfgam_results

            if (is.null(results)) {
                return(tags$div(
                    class = "overview-awaiting",
                    icon("lightbulb"),
                    tags$span(class = "awaiting-text",
                              "Recommendations will appear after the model is run"),
                    tags$span(class = "awaiting-subtext",
                              "Based on sampler diagnostics and convergence checks")
                ))
            }

            advice <- results$advice
            diag <- results$sampler_diagnostics

            if (length(advice) == 0) {
                return(tags$div(
                    class = "overview-advice-item advice-ok",
                    icon("circle-check"),
                    tags$span(
                        tags$strong("All clear."),
                        " The sampler converged without issues.",
                        " Chains are well-mixed, effective sample sizes are adequate,",
                        " and no divergent transitions were detected.",
                        " You can trust these results."
                    )
                ))
            }

            advice_cards <- tagList()

            if (!is.null(advice$divergences)) {
                advice_cards <- tagList(advice_cards, tags$div(
                    class = "overview-advice-item advice-critical",
                    icon("triangle-exclamation"),
                    tags$span(
                        tags$strong("Divergent transitions detected"),
                        paste0(
                            " (", diag$n_divergent, " of ",
                            format(diag$total_post_warmup, big.mark = ","),
                            " post-warmup iterations, ", diag$divergent_pct, "%). "
                        ),
                        "Results may be biased. ",
                        tags$strong("Increase the Acceptance Probability"),
                        " (adapt delta) in Analysis Setup > Advanced Modelling Options. ",
                        "If this persists, check data for severe outliers or use stronger priors."
                    )
                ))
            }

            if (!is.null(advice$rhat)) {
                advice_cards <- tagList(advice_cards, tags$div(
                    class = "overview-advice-item advice-critical",
                    icon("link-slash"),
                    tags$span(
                        tags$strong("Chains have not converged"),
                        paste0(" (max R-hat = ", diag$max_rhat, "). "),
                        "The sampling chains disagreed on the solution. ",
                        tags$strong("Increase the number of iterations"),
                        " to give the sampler more time to find a stable solution."
                    )
                ))
            }

            if (!is.null(advice$ess_bulk)) {
                advice_cards <- tagList(advice_cards, tags$div(
                    class = "overview-advice-item advice-warn",
                    icon("chart-bar"),
                    tags$span(
                        tags$strong("Low Bulk Effective Sample Size. "),
                        "Central estimates (means, medians) may lack precision. ",
                        tags$strong("Increase the number of iterations. "),
                        "If other warnings (divergences, R-hat) exist, resolve those first."
                    )
                ))
            }

            if (!is.null(advice$ess_tail)) {
                advice_cards <- tagList(advice_cards, tags$div(
                    class = "overview-advice-item advice-warn",
                    icon("arrows-left-right-to-line"),
                    tags$span(
                        tags$strong("Low Tail Effective Sample Size. "),
                        "Credible interval boundaries may be imprecise. ",
                        tags$strong("Increase the number of iterations. "),
                        "Resolve any divergence or convergence issues first, as they are likely root causes."
                    )
                ))
            }

            if (!is.null(advice$treedepth)) {
                advice_cards <- tagList(advice_cards, tags$div(
                    class = "overview-advice-item advice-info",
                    icon("code-branch"),
                    tags$span(
                        tags$strong("Maximum treedepth exceeded"),
                        paste0(
                            " (", diag$n_max_treedepth,
                            " iterations, ", diag$treedepth_pct, "%). "
                        ),
                        "This is an efficiency issue, not a validity issue. ",
                        "The analysis was slower than necessary. ",
                        tags$strong("Increase Maximum Treedepth"),
                        " in Advanced Modelling Options if runtime is a concern."
                    )
                ))
            }

            advice_cards
        })

        # =====================================================================
        # Result Renderers (other tabs)
        # =====================================================================

        output$results_table <- renderUI({
            req(app_state$model_nttdfgam_results)
            renderGlassTable(
                data = app_state$model_nttdfgam_results$table,
                sortable = TRUE
            )
        })

        observeEvent(app_state$model_nttdfgam_results, {
            req(app_state$model_nttdfgam_results)
            payload <- prepare_subject_cvi_d3(app_state$model_nttdfgam_results$results)
            updateGlassD3Plot(session, session$ns("subject_plot"),
                plot_type = payload$plot_type,
                data = payload$data,
                params = payload$params
            )
        })

        observeEvent(app_state$model_nttdfgam_results, {
            req(app_state$model_nttdfgam_results)
            payload <- prepare_cvi_vs_concentration_d3(app_state$model_nttdfgam_results$results)
            updateGlassD3Plot(session, session$ns("concentration_plot"),
                plot_type = payload$plot_type,
                data = payload$data,
                params = payload$params
            )
        })

        # =====================================================================
        # Prevent output suspension for overview panels
        # =====================================================================
        outputOptions(output, "overview_data_summary",   suspendWhenHidden = FALSE)
        outputOptions(output, "overview_sampler_config", suspendWhenHidden = FALSE)
        outputOptions(output, "overview_diagnostics",    suspendWhenHidden = FALSE)
        outputOptions(output, "overview_advice",         suspendWhenHidden = FALSE)
    })
}

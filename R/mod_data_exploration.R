# Data Exploration Module
# ------------------------------------------------------------------------------
#
# Handles the Data Exploration page:
#   - Tab 1: Data Overview (glassTable, read-only)
#   - Tab 2: Exclude/Include Data Points (interactive scatter + clickable table)
#   - Tab 3: Prior Distribution Plots (D3 density)
#   - Tab 4: ANOVA & Descriptive Statistics (plots + tables)
#   - Tab 5: Bootstrap Estimates (bv_anova_bootstrap_ci)
#
# Pipeline: filtered_data − excluded_rows = analysis_data
#
# ------------------------------------------------------------------------------

#' Data Exploration Module UI
#'
#' Builds the user interface for the Data Exploration page, which provides
#' five tabbed views inside a single [glassCard()]:
#'
#' \enumerate{
#'   \item **Data Overview** — read-only [renderGlassTable()] of the current
#'         analysis data.
#'   \item **Exclude / Include** — interactive D3 scatter plot and clickable
#'         table for toggling individual observations in or out of the analysis
#'         set. Includes per-subject filtering and combined/faceted view modes.
#'   \item **Prior Distributions** — D3 density plots of the current
#'         hyperparameter priors (\eqn{\beta}, CV\subscript{I}, CV\subscript{A},
#'         CV\subscript{G}, etc.).
#'   \item **ANOVA & Descriptive Statistics** — dot-plots, grand/subject/sample
#'         summary tables, ANOVA variance-component plots and tables.
#'   \item **Bootstrap Estimates** — non-parametric bootstrap confidence
#'         intervals via [bv_anova_bootstrap_ci()], with a comparison table
#'         against the classical ANOVA (Burdick–Graybill) intervals.
#' }
#'
#' Navigation between tabs is driven by the headless [glassTabsetPanel()]
#' whose tab values are exposed through [mod_data_exploration_flyout_items()].
#'
#' @param id Character string. Shiny namespace ID for the module, used to
#'   scope all input/output IDs via [shiny::NS()].
#'
#' @return A [shiny::tagList()] containing the complete exploration UI,
#'   including an [htmltools::htmlDependency()] for the module-specific
#'   JavaScript and CSS assets (`glass_data_exploration.js`,
#'   `glass_data_exploration.css`).
#'
#' @seealso [mod_data_exploration_server()] for the companion server logic,
#'   [mod_data_exploration_flyout_items()] for sidebar navigation entries.
#'
#' @keywords internal
#' @importFrom htmltools htmlDependency
#' @importFrom shiny icon NS uiOutput
mod_data_exploration_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # Exploration-specific CSS / JS assets
    htmltools::htmlDependency(
      name = "glass-data-exploration",
      version = "1.0.0",
      src = c(
        file = system.file("assets", package = "EstimatingBiologicalVariation")
      ),
      script = "glass_data_exploration.js",
      stylesheet = "glass_data_exploration.css"
    ),
    glassCard(
      inputId = ns("explore_card"),
      title = "Data Exploration",
      icon = icon("magnifying-glass-chart"),
      glassTabsetPanel(
        inputId = ns("explore_tabs"),
        selected = "overview",
        color = "purple",
        headless = TRUE,

        # ==== Tab 1: Data Overview ====
        glassTabPanel(
          title = "Data Overview",
          value = "overview",
          icon = icon("table"),
          glassRow(
            glassCol(12, uiOutput(ns("overview_table_ui")))
          )
        ),

        # ==== Tab 2: Exclude / Include ====
        glassTabPanel(
          title = "Exclude / Include",
          value = "exclusion",
          icon = icon("filter-circle-xmark"),
          glassTabsetPanel(
            inputId = ns("exclusion_subtabs"),
            selected = "excl_plot",
            color = "green",

            # ---- Sub-tab: Plot View ----
            glassTabPanel(
              title = "Plot View",
              value = "excl_plot",
              icon = icon("chart-line"),
              glassToolbar(
                title = "Data Points \u2014 Click to Exclude / Include",
                icon = icon("chart-line"),
                subtitle = "Each point is one measurement",
                rounded = "top",
                uiOutput(ns("subject_filter_ui"), inline = TRUE),
                uiOutput(ns("view_mode_ui"), inline = TRUE)
              ),
              glassD3PlotOutput(ns("exclusion_scatter"), height = "450px")
            ),

            # ---- Sub-tab: Table View ----
            glassTabPanel(
              title = "Table View",
              value = "excl_table",
              icon = icon("table"),
              glassToolbar(
                title = "Analysis Data \u2014 Click to Exclude / Include",
                icon = icon("database"),
                subtitle = "Click a row to exclude or restore it.",
                rounded = "top",
                uiOutput(ns("table_subject_filter_ui"), inline = TRUE),
                uiOutput(ns("exclusion_counter_ui")),
                glassButton(
                  inputId = ns("restore_all_btn"),
                  label = "Restore All",
                  icon = icon("rotate-left"),
                  color = "green",
                  size = "sm",
                  icon_only = TRUE,
                  icon_only_action = "tooltip"
                )
              ),
              uiOutput(ns("main_table_ui"))
            )
          )
        ),

        # ==== Tab 3: Prior Distributions ====
        glassTabPanel(
          title = "Prior Distributions",
          value = "priors",
          icon = icon("chart-area"),
          glassRow(
            glassCol(12, glassD3PlotOutput(ns("prior_plot"), height = "700px"))
          )
        ),

        # ==== Tab 4: ANOVA & Descriptive Stats ====
        glassTabPanel(
          title = "ANOVA & Descriptive Stats",
          value = "anova",
          icon = icon("square-poll-vertical"),
          glassTabsetPanel(
            inputId = ns("anova_subtabs"),
            selected = "desc_plots",
            color = "green",

            # ---- Sub-tab: Descriptive Plots ----
            glassTabPanel(
              title = "Descriptive Plots",
              value = "desc_plots",
              icon = icon("chart-bar"),
              glassRow(
                glassCol(
                  12,
                  label = "Descriptive Statistics",
                  label_icon = icon("chart-bar"),
                  label_underline = TRUE,
                  glassD3PlotOutput(ns("descriptive_plot"), height = "450px")
                )
              )
            ),

            # ---- Sub-tab: Descriptive Tables ----
            glassTabPanel(
              title = "Descriptive Tables",
              value = "desc_tables",
              icon = icon("table"),
              glassRow(
                glassCol(
                  6,
                  label = "Grand Summary",
                  label_underline = TRUE,
                  uiOutput(ns("grand_summary_table_ui"))
                ),
                glassCol(
                  6,
                  label = "Per-Subject Summary",
                  label_underline = TRUE,
                  uiOutput(ns("subject_summary_table_ui"))
                )
              )
            ),

            # ---- Sub-tab: ANOVA Plot ----
            glassTabPanel(
              title = "ANOVA Plot",
              value = "anova_plot_tab",
              icon = icon("chart-column"),
              glassRow(
                glassCol(
                  12,
                  label = "ANOVA-based Estimates",
                  label_icon = icon("square-poll-vertical"),
                  label_underline = TRUE,
                  glassD3PlotOutput(ns("anova_plot"), height = "350px")
                )
              )
            ),

            # ---- Sub-tab: ANOVA Tables ----
            glassTabPanel(
              title = "ANOVA Tables",
              value = "anova_tables",
              icon = icon("table-cells"),
              glassRow(
                glassCol(
                  6,
                  label = "Variance Components",
                  label_underline = TRUE,
                  uiOutput(ns("anova_table_ui"))
                ),
                glassCol(
                  6,
                  label = "ANOVA Table",
                  label_underline = TRUE,
                  uiOutput(ns("anova_detail_table_ui"))
                )
              )
            )
          )
        ),

        # ==== Tab 5: Bootstrap Estimates ====
        glassTabPanel(
          title = "Bootstrap Estimates",
          value = "bootstrap",
          icon = icon("shuffle"),
          glassTabsetPanel(
            inputId = ns("bootstrap_subtabs"),
            selected = "boot_plot",
            color = "green",

            # ---- Sub-tab: Bootstrap Plot ----
            glassTabPanel(
              title = "Bootstrap Plot",
              value = "boot_plot",
              icon = icon("chart-column"),
              glassToolbar(
                title = "Bootstrap Variance Components",
                icon = icon("shuffle"),
                subtitle = "Non-parametric percentile bootstrap CIs",
                rounded = "top",
                glassDropdown(
                  inputId = ns("bootstrap_B"),
                  choices = c(
                    "1 000" = "1000",
                    "2 000" = "2000",
                    "5 000" = "5000",
                    "10 000" = "10000"
                  ),
                  selected = "2000",
                  color = "purple",
                  width = "130px"
                ),
                glassButton(
                  inputId = ns("run_bootstrap_btn"),
                  label = "Run Bootstrap",
                  icon = icon("play"),
                  color = "purple",
                  size = "sm"
                )
              ),
              glassRow(
                glassCol(
                  12,
                  glassD3PlotOutput(
                    outputId = ns("bootstrap_plot"),
                    height = "350px"
                  )
                )
              )
            ),

            # ---- Sub-tab: Bootstrap Table ----
            glassTabPanel(
              title = "Bootstrap Table",
              value = "boot_table",
              icon = icon("table"),
              glassRow(
                glassCol(
                  6,
                  label = "Bootstrap Variance Components",
                  label_underline = TRUE,
                  uiOutput(ns("bootstrap_table_ui"))
                ),
                glassCol(
                  6,
                  label = "Comparison: ANOVA vs Bootstrap",
                  label_underline = TRUE,
                  uiOutput(ns("bootstrap_comparison_table_ui"))
                )
              )
            )
          )
        )
      )
    )
  )
}

#' Data Exploration Module Server
#'
#' Server-side logic for the Data Exploration page.
#'
#' @details
#' ## Reactive pipeline
#'
#' The central data flow is:
#'
#' \deqn{\texttt{filtered\_data} - \texttt{excluded\_rows} =
#'  \texttt{analysis\_data}}
#'
#' `filtered_data` and `excluded_rows` are read from `app_state`; the derived
#' `analysis_data` is written back into `app_state` so that downstream modules
#' (modelling, results) operate on the same cleaned dataset.
#'
#' ## Key responsibilities
#'
#' \itemize{
#'   \item **Exclusion pipeline** — an [observe()] derives `analysis_data`
#'         whenever `filtered_data` or `excluded_rows` change.
#'   \item **Descriptive statistics** — a [shiny::reactive()] wrapping
#'         [compute_descriptive_stats()] that returns grand, subject, and
#'         sample summaries.
#'   \item **ANOVA estimates** — a [shiny::reactive()] wrapping
#'         [compute_anova_estimates()] (classical Burdick–Graybill CIs).
#'   \item **Bootstrap CIs** — triggered on button click; calls
#'         [bv_anova_bootstrap_ci()] with integer-coded data prepared for the
#'         C++ back-end.
#'   \item **Scatter plot** — renders an interactive D3 scatter via
#'         [updateGlassD3Plot()]; supports subject filtering and
#'         combined/faceted view modes.
#'   \item **Click handlers** — `observeEvent()` handlers for scatter-point
#'         clicks and table-row clicks that toggle rows between included and
#'         excluded status.
#'   \item **Prior density plots** — re-rendered whenever hyperparameter
#'         values in `app_state` change.
#' }
#'
#' All `renderUI` outputs use `suspendWhenHidden = FALSE` so that tab content
#' stays up-to-date even when the tab is not currently visible.
#'
#' @param id Character string. Shiny namespace ID for the module (must match
#'   the `id` passed to [mod_data_exploration_ui()]).
#' @param app_state A [shiny::reactiveValues()] object containing shared
#'   application state. The following fields are **read**:
#'   \describe{
#'     \item{`filtered_data`}{A [data.table::data.table()] of the current
#'       dataset after column mapping and any upstream filters have been
#'       applied. Must contain columns `SubjectID`, `SampleID`,
#'       `ReplicateID`, and `y`.}
#'     \item{`excluded_rows`}{A [data.table::data.table()] with columns
#'       `SubjectID`, `SampleID`, `ReplicateID` identifying rows to exclude.}
#'     \item{`log_transformed`}{Logical flag indicating whether values are on
#'       the log scale.}
#'     \item{`hyper_beta`, `hyper_cvi`, `hyper_cva`, `hyper_cvg`,
#'       `hyper_dfi`, `hyper_dfa`, `hyper_hbhr`}{Numeric hyperparameter
#'       centres used for prior density plots.}
#'     \item{`hyper_*_weakness`}{Numeric weakness/strength multipliers
#'       for each hyperparameter prior.}
#'   }
#'   The following field is **written**:
#'   \describe{
#'     \item{`analysis_data`}{The cleaned [data.table::data.table()] after
#'       exclusions have been applied (i.e., `filtered_data` minus
#'       `excluded_rows`).}
#'   }
#'
#' @return Called for its side effects (registers outputs and observers inside
#'   the Shiny module). No meaningful return value.
#'
#' @seealso [mod_data_exploration_ui()] for the companion UI,
#'   [compute_descriptive_stats()], [compute_anova_estimates()],
#'   [bv_anova_bootstrap_ci()] for the underlying computations.
#'
#' @keywords internal
#' @importFrom data.table copy rbindlist setkey
#' @importFrom shiny icon isolate moduleServer observe observeEvent reactive
#' @importFrom shiny reactiveVal renderUI req
mod_data_exploration_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Fix global binding issues
    SubjectID <- SampleID <- ReplicateID <- NULL # nolint
    `.row_idx` <- NULL

    # =========================================================================
    # Exclusion -> Analysis Data Pipeline
    # =========================================================================

    observe({
      .t <- bv_timer_start("explore::build_analysis_data")
      req(app_state$filtered_data)

      data <- data.table::copy(app_state$filtered_data)
      excluded <- app_state$excluded_rows

      if (nrow(excluded) == 0) {
        app_state$analysis_data <- data
        bv_timer_end(.t)
        return()
      }

      data.table::setkey(data, SubjectID, SampleID, ReplicateID)
      data.table::setkey(excluded, SubjectID, SampleID, ReplicateID)

      data <- data[!excluded]
      app_state$analysis_data <- data
      bv_timer_end(.t)
    })

    # =========================================================================
    # Computed Reactive Values
    # =========================================================================

    descriptive_stats <- reactive({
      .t <- bv_timer_start("explore::descriptive_stats")
      req(app_state$analysis_data, nrow(app_state$analysis_data) > 0)
      result <- compute_descriptive_stats(
        app_state$analysis_data,
        log_transformed = isTRUE(app_state$log_transformed)
      )
      bv_timer_end(.t)
      result
    })

    anova_results <- reactive({
      .t <- bv_timer_start("explore::anova_results")
      req(app_state$analysis_data, nrow(app_state$analysis_data) > 0)
      result <- tryCatch(
        compute_anova_estimates(
          data = app_state$analysis_data,
          log_transformed = isTRUE(app_state$log_transformed)
        ),
        error = function(e) NULL
      )
      bv_timer_end(.t)
      result
    })

    # =========================================================================
    # Tab 1 — Data Overview
    # =========================================================================

    output$overview_table_ui <- renderUI({
      .t <- bv_timer_start("explore::overview_table")
      req(app_state$analysis_data)
      data <- app_state$analysis_data
      result <- renderGlassTable(
        data = data,
        caption = paste0(
          "Showing <strong>",
          nrow(data),
          "</strong> observations across <strong>",
          length(unique(data$SubjectID)),
          "</strong> subjects"
        ),
        sortable = TRUE
      )
      bv_timer_end(.t)
      result
    })

    # =========================================================================
    # Tab 2 — Exclude / Include
    # =========================================================================

    # --- Subject filter UI (selectize in toolbar) ---
    output$subject_filter_ui <- renderUI({
      req(app_state$filtered_data)
      subjects <- sort(
        unique(
          as.character(app_state$filtered_data$SubjectID)
        )
      )
      glassSelectizeInput(
        inputId = ns("subject_filter"),
        choices = c(
          "All" = "all",
          stats::setNames(subjects, subjects)
        ),
        selected = "all",
        multiple = TRUE,
        placeholder = "All subjects",
        additionalPlaceholder = "Add...",
        color = "purple",
        width = "240px"
      )
    })

    # --- Table subject filter UI (selectize in toolbar) ---
    output$table_subject_filter_ui <- renderUI({
      req(app_state$filtered_data)
      subjects <- sort(
        unique(
          as.character(app_state$filtered_data$SubjectID)
        )
      )
      glassSelectizeInput(
        inputId = ns("table_subject_filter"),
        choices = c(
          "All" = "all",
          stats::setNames(subjects, subjects)
        ),
        selected = "all",
        multiple = TRUE,
        placeholder = "All subjects",
        additionalPlaceholder = "Add...",
        color = "purple",
        width = "240px"
      )
    })

    # --- View mode toggle UI ---
    output$view_mode_ui <- renderUI({
      req(app_state$filtered_data)
      n_subjects <- length(
        unique(app_state$filtered_data$SubjectID)
      )
      default_mode <- if (n_subjects <= 15) {
        "faceted"
      } else {
        "combined"
      }
      glassRadioButtons(
        inputId = ns("scatter_view_mode"),
        choices = c(
          "Combined" = "combined",
          "Faceted" = "faceted"
        ),
        selected = default_mode,
        color = "purple",
        size = "sm",
        icons = list(
          combined = icon("layer-group"),
          faceted  = icon("grip")
        ),
        icon_only = TRUE,
        width = "auto"
      )
    })

    # --- Interactive scatter plot ---
    # Primary trigger: filtered_data (or excluded_rows).
    # subject_filter & scatter_view_mode are READ but isolated so that the
    # renderUI rebuild of those toolbar widgets doesn't cascade into an
    # extra scatter re-render.  A dedicated observeEvent below re-fires the
    # scatter when those inputs genuinely change by user action.
    render_scatter <- function() {
      .t <- bv_timer_start("explore::scatter_plot")
      data <- app_state$filtered_data
      excluded <- app_state$excluded_rows

      # Subject filter (isolated — read current value without subscribing)
      selected_subjects <- isolate(input$subject_filter)
      if (!is.null(selected_subjects) &&
        length(selected_subjects) > 0 &&
        !("all" %in% selected_subjects)) {
        data <- data[as.character(SubjectID) %in% selected_subjects]
      }

      # View mode (isolated)
      n_subjects <- length(unique(app_state$filtered_data$SubjectID))
      default_view <- if (n_subjects <= 15) {
        "faceted"
      } else {
        "combined"
      }
      view_mode <- isolate(input$scatter_view_mode) %||% default_view

      scatter_data <- prepare_exploration_scatter_d3(
        data = data,
        excluded_rows = excluded,
        view_mode = view_mode
      )

      updateGlassD3Plot(
        session,
        outputId = ns("exclusion_scatter"),
        plot_type = scatter_data$plot_type,
        data = scatter_data$data,
        params = scatter_data$params
      )
      bv_timer_end(.t)
    }

    # Fire when data or exclusions change
    observe({
      req(app_state$filtered_data)
      # Touch these to subscribe to changes
      app_state$filtered_data
      app_state$excluded_rows
      render_scatter()
    })

    # Fire when user changes subject filter or view mode
    observeEvent(input$subject_filter, render_scatter(), ignoreInit = TRUE)
    observeEvent(input$scatter_view_mode, render_scatter(), ignoreInit = TRUE)

    # Handle clicks from scatter plot (JS -> Shiny.setInputValue)
    observeEvent(input$exclusion_scatter_click, {
      click_info <- input$exclusion_scatter_click
      req(click_info)

      dt <- app_state$filtered_data
      req(dt)

      # Match the clicked point back to data
      match_row <- dt[
        as.character(SubjectID) == as.character(click_info$SubjectID) &
          as.character(SampleID) == as.character(click_info$SampleID) &
          as.character(ReplicateID) == as.character(click_info$ReplicateID)
      ]
      req(nrow(match_row) > 0)

      row_key <- match_row[1, .(SubjectID, SampleID, ReplicateID)]
      excluded <- app_state$excluded_rows

      # Toggle: if already excluded -> restore; otherwise -> exclude
      if (nrow(excluded) > 0) {
        match_excl <- excluded[
          as.character(SubjectID) == as.character(row_key$SubjectID) &
            as.character(SampleID) == as.character(row_key$SampleID) &
            as.character(ReplicateID) == as.character(row_key$ReplicateID)
        ]
        if (nrow(match_excl) > 0) {
          remaining <- excluded[!(
            as.character(SubjectID) == as.character(row_key$SubjectID) &
              as.character(SampleID) == as.character(row_key$SampleID) &
              as.character(ReplicateID) == as.character(row_key$ReplicateID)
          )]
          app_state$excluded_rows <- remaining
          return()
        }
      }

      app_state$excluded_rows <- unique(
        rbindlist(list(excluded, row_key))
      )
    })

    # Handle clicks from analysis data table (toggle exclude/restore)
    observeEvent(input$main_table_row_click, {
      click_info <- input$main_table_row_click
      req(click_info, app_state$filtered_data)

      row_idx <- click_info$row
      action <- click_info$action
      data <- app_state$filtered_data

      # Apply the same subject filter so row_idx matches the displayed table
      selected_subjects <- input$table_subject_filter
      if (!is.null(selected_subjects) &&
        length(selected_subjects) > 0 &&
        !("all" %in% selected_subjects)) {
        data <- data[as.character(SubjectID) %in% selected_subjects]
      }

      req(row_idx > 0, row_idx <= nrow(data))

      row_key <- data[row_idx, .(SubjectID, SampleID, ReplicateID)]
      excluded <- app_state$excluded_rows

      if (identical(action, "restore")) {
        # Remove from excluded
        remaining <- excluded[!(
          as.character(SubjectID) == as.character(row_key$SubjectID) &
            as.character(SampleID) == as.character(row_key$SampleID) &
            as.character(ReplicateID) == as.character(row_key$ReplicateID)
        )]
        app_state$excluded_rows <- remaining
      } else {
        # Add to excluded
        app_state$excluded_rows <- unique(
          rbindlist(list(excluded, row_key))
        )
      }
    })

    # Restore-all button
    observeEvent(input$restore_all_btn, {
      app_state$excluded_rows <- data.table(
        SubjectID = character(0),
        SampleID = character(0),
        ReplicateID = character(0)
      )
    })

    # Main table (clickable rows — toggle exclude/restore)
    output$main_table_ui <- renderUI({
      req(app_state$filtered_data)
      data <- app_state$filtered_data
      excluded <- app_state$excluded_rows

      # Subject filter for table view
      selected_subjects <- input$table_subject_filter
      if (!is.null(selected_subjects) &&
        length(selected_subjects) > 0 &&
        !("all" %in% selected_subjects)) {
        data <- data[as.character(SubjectID) %in% selected_subjects]
      }

      # Compute which row indices are excluded
      excluded_indices <- integer(0)
      if (nrow(excluded) > 0) {
        data_keyed <- copy(data)
        data_keyed[, .row_idx := .I]
        setkey(data_keyed, SubjectID, SampleID, ReplicateID)
        excl_keyed <- copy(excluded)
        setkey(excl_keyed, SubjectID, SampleID, ReplicateID)
        matched <- data_keyed[excl_keyed, nomatch = NULL]
        excluded_indices <- sort(matched$.row_idx)
      }

      n_total <- nrow(data)
      n_excluded <- length(excluded_indices)
      n_analysis <- n_total - n_excluded

      caption_text <- if (n_excluded > 0) {
        paste0(
          n_total,
          " total rows &mdash; ",
          "<strong>",
          n_analysis,
          "</strong> in analysis, ",
          "<span style='color:#dc3545;'>",
          n_excluded,
          " excluded</span>"
        )
      } else {
        paste0(n_total, " rows in analysis")
      }

      tags$div(
        class = "explore-clickable-table",
        `data-ns` = ns("main_table_row_click"),
        `data-action` = "toggle",
        renderGlassTable(
          data = data,
          caption = caption_text,
          excluded_rows = excluded_indices,
          sortable = TRUE,
          rounded = "bottom",
          downloadable = TRUE,
          download_filename = paste0(
            "analysis_data_",
            Sys.Date()
          )
        )
      )
    })

    # Exclusion counter badge
    output$exclusion_counter_ui <- renderUI({
      n <- nrow(app_state$excluded_rows)
      if (n == 0) {
        return(NULL)
      }
      tags$div(
        class = "explore-exclusion-counter",
        glassBadge(
          label = paste0(n, " excluded"),
          color = "red",
          shape = "pill",
          glow = TRUE,
          size = "md"
        )
      )
    })

    # =========================================================================
    # Tab 3 — Prior Distributions
    # =========================================================================

    observe({
      .t <- bv_timer_start("explore::prior_distributions")
      req(
        app_state$hyper_beta,
        app_state$hyper_cvi,
        app_state$hyper_cva,
        app_state$hyper_cvg
      )

      strength_vec <- c(
        app_state$hyper_beta_weakness %||% 1,
        app_state$hyper_cvi_weakness %||% 1,
        app_state$hyper_cva_weakness %||% 1,
        app_state$hyper_cvg_weakness %||% 1,
        app_state$hyper_dfi_weakness %||% 1,
        app_state$hyper_dfa_weakness %||% 1,
        app_state$hyper_hbhr_weakness %||% 0.667
      )

      prior_data <- prepare_prior_density_d3(
        beta = app_state$hyper_beta,
        cvi = app_state$hyper_cvi %||% 10,
        cva = app_state$hyper_cva %||% 3,
        cvg = app_state$hyper_cvg %||% 20,
        dfi = app_state$hyper_dfi %||% 20,
        dfa = app_state$hyper_dfa %||% 20,
        hbhr = app_state$hyper_hbhr %||% 50,
        strength = strength_vec,
        log_transformed = isTRUE(app_state$log_transformed),
        model = "NTT",
        title = "Prior Density Plots",
        subtitle = "Based on current hyperparameter settings"
      )

      updateGlassD3Plot(
        session,
        outputId = ns("prior_plot"),
        plot_type = prior_data$plot_type,
        data = prior_data$data,
        params = prior_data$params
      )
      bv_timer_end(.t)
    })

    # =========================================================================
    # Tab 4 — ANOVA & Descriptive Statistics
    # =========================================================================

    # --- Descriptive dot-plot ---
    observe({
      .t <- bv_timer_start("explore::descriptive_dotplot")
      req(app_state$analysis_data)
      stats <- descriptive_stats()
      req(stats)

      plot_data <- prepare_descriptive_dotplot_d3(
        data = app_state$analysis_data,
        subject_means = stats$subject_summary
      )

      updateGlassD3Plot(
        session,
        outputId = ns("descriptive_plot"),
        plot_type = plot_data$plot_type,
        data = plot_data$data,
        params = plot_data$params
      )
      bv_timer_end(.t)
    })

    # Grand summary table
    output$grand_summary_table_ui <- renderUI({
      .t <- bv_timer_start("explore::grand_summary_table")
      stats <- descriptive_stats()
      caption_test <- paste0(
        "Overall summary statistics for the entire group. ",
        "Current grouping is: "
        # MUST ADD: Implement dynamic caption based on
        # current grouping (analyte, material, sex, etc.)
      )
      req(stats)
      result <- renderGlassTable(
        data = stats$grand_summary,
        sortable = FALSE,
        caption = caption_test,
        downloadable = TRUE,
        download_filename = paste0(
          "bvem_grouping_info_trans_info_grand_summary_",
          Sys.Date()
          # MUST ADD: Implement dynamic filename components based on
          # current grouping (analyte, material, sex, etc.)
        )
      )
      bv_timer_end(.t)
      result
    })

    # Per-subject summary table
    output$subject_summary_table_ui <- renderUI({
      .t <- bv_timer_start("explore::subject_summary_table")
      stats <- descriptive_stats()
      req(stats)
      caption_text <- paste0(
        "Overall summary statistics for the entire group. ",
        "Current grouping is: "
        # MUST ADD: Implement dynamic caption based on
        # current grouping (analyte, material, sex, etc.)
      )
      result <- renderGlassTable(
        data = stats$subject_summary,
        sortable = TRUE,
        caption = caption_text,
        downloadable = TRUE,
        download_filename = paste0(
          "bvem_grouping_info_trans_info_subject_summary_",
          Sys.Date(),
          ".csv"
        )
      )
      bv_timer_end(.t)
      result
    })

    # --- ANOVA plot ---
    observe({
      .t <- bv_timer_start("explore::anova_plot")
      anova <- anova_results()
      req(anova)

      plot_data <- prepare_anova_components_d3(anova)

      updateGlassD3Plot(
        session,
        outputId = ns("anova_plot"),
        plot_type = plot_data$plot_type,
        data = plot_data$data,
        params = plot_data$params
      )
      bv_timer_end(.t)
    })

    # ANOVA variance components table
    output$anova_table_ui <- renderUI({
      .t <- bv_timer_start("explore::anova_cv_table")
      anova <- anova_results()
      req(anova)
      result <- renderGlassTable(
        data = anova$cv_table,
        sortable = FALSE,
        downloadable = TRUE,
        download_filename = paste0(
          "bvem_grouping_info_trans_info_anova_cv_table_",
          Sys.Date()
        )
      )
      bv_timer_end(.t)
      result
    })

    # ANOVA detail table
    output$anova_detail_table_ui <- renderUI({
      .t <- bv_timer_start("explore::anova_detail_table")
      anova <- anova_results()
      req(anova)
      result <- renderGlassTable(
        data = anova$anova_table,
        sortable = FALSE,
        downloadable = TRUE,
        download_filename = paste0(
          "bvem_grouping_info_trans_info_anova_table_",
          Sys.Date()
        )
      )
      bv_timer_end(.t)
      result
    })

    # =========================================================================
    # Tab 5 — Bootstrap Estimates
    # =========================================================================

    # Reactive to store bootstrap results (run on button click)
    bootstrap_results <- reactiveVal(NULL)

    observeEvent(input$run_bootstrap_btn, {
      req(app_state$analysis_data, nrow(app_state$analysis_data) > 0)

      data_copy <- data.table::copy(app_state$analysis_data)

      # Integer-coded data for C++ back-end (including ReplicateID)
      subject_levels <- as.integer(
        as.factor(data_copy$SubjectID)
      )
      sample_levels <- as.integer(
        as.factor(
          paste0(data_copy$SubjectID, "___", data_copy$SampleID)
        )
      )
      replicate_levels <- as.integer(
        as.factor(
          paste0(
            data_copy$SubjectID, "___",
            data_copy$SampleID, "___",
            data_copy$ReplicateID
          )
        )
      )

      coded_data <- list(
        SubjectID = subject_levels,
        SampleID = sample_levels,
        ReplicateID = replicate_levels,
        y = as.numeric(data_copy$y)
      )

      num_replicates <- as.integer(input$bootstrap_B %||% 2000)

      tryCatch(
        {
          boot_result <- bv_anova_bootstrap_ci(
            data_orig = coded_data,
            B = num_replicates,
            level = 0.95,
            output_type_for_point_est = "cv",
            mult = 100
          )
          bootstrap_results(boot_result)
        },
        error = function(e) {
          message("[Bootstrap] bv_anova_bootstrap_ci failed: ", e$message)
          bootstrap_results(NULL)
        }
      )
    })

    # Render bootstrap D3 chart (reuses anova_components chart type)
    observe({
      boot_result <- bootstrap_results()
      req(boot_result)

      point_est <- boot_result$point_estimates
      conf_int <- boot_result$conf_intervals

      plot_data <- prepare_anova_components_d3(
        anova_results = list(
          CV_A = point_est$sigma_A,
          CV_I = point_est$sigma_I,
          CV_G = point_est$sigma_G,
          CV_A_lower = conf_int$sigma_A_CI[1],
          CV_A_upper = conf_int$sigma_A_CI[2],
          CV_I_lower = conf_int$sigma_I_CI[1],
          CV_I_upper = conf_int$sigma_I_CI[2],
          CV_G_lower = conf_int$sigma_G_CI[1],
          CV_G_upper = conf_int$sigma_G_CI[2]
        ),
        title = "Bootstrap Variance Components",
        subtitle = paste0(
          "Percentile bootstrap CIs (PBCIs) (",
          input$bootstrap_B %||% 2000,
          " replicates, 95% level)"
        )
      )

      updateGlassD3Plot(
        session,
        outputId = ns("bootstrap_plot"),
        plot_type = plot_data$plot_type,
        data = plot_data$data,
        params = plot_data$params
      )
    })

    # Bootstrap variance-components table
    output$bootstrap_table_ui <- renderUI({
      boot_result <- bootstrap_results()
      req(boot_result)

      point_est <- boot_result$point_estimates
      conf_int <- boot_result$conf_intervals

      boot_table <- data.table::data.table(
        "Component" = c(
          "\\(\\mathrm{CV}_{\\mathrm{A}} (\\%)\\)",
          "\\(\\mathrm{CV}_{\\mathrm{I}} (\\%)\\)",
          "\\(\\mathrm{CV}_{\\mathrm{G}} (\\%)\\)"
        ),
        `Estimate (%)` = format_bv_value(
          c(point_est$sigma_A, point_est$sigma_I, point_est$sigma_G)
        ),
        `Lower PBCI 95%` = format_bv_value(
          c(
            conf_int$sigma_A_CI[1],
            conf_int$sigma_I_CI[1],
            conf_int$sigma_G_CI[1]
          )
        ),
        `Upper PBCI 95%` = format_bv_value(
          c(
            conf_int$sigma_A_CI[2],
            conf_int$sigma_I_CI[2],
            conf_int$sigma_G_CI[2]
          )
        )
      )
      renderGlassTable(
        data = boot_table,
        sortable = FALSE
      )
    })

    # Comparison table: ANOVA (Burdick-Graybill) vs Bootstrap CIs
    output$bootstrap_comparison_table_ui <- renderUI({
      boot_result <- bootstrap_results()
      anova <- anova_results()
      req(boot_result, anova)

      point_est <- boot_result$point_estimates
      conf_int <- boot_result$conf_intervals

      comparison_table <- data.table::data.table(
        "Component" = c(
          "\\(\\mathrm{CV}_{\\mathrm{A}} (\\%)\\)",
          "\\(\\mathrm{CV}_{\\mathrm{I}} (\\%)\\)",
          "\\(\\mathrm{CV}_{\\mathrm{G}} (\\%)\\)"
        ),
        `ANOVA Est. (%)` = format_bv_value(
          c(anova$CV_A, anova$CV_I, anova$CV_G)
        ),
        `ANOVA Lower CI (%)` = format_bv_value(
          c(anova$CV_A_lower, anova$CV_I_lower, anova$CV_G_lower)
        ),
        `ANOVA Upper CI (%)` = format_bv_value(
          c(anova$CV_A_upper, anova$CV_I_upper, anova$CV_G_upper)
        ),
        `Boot. Est. (%)` = format_bv_value(
          c(point_est$sigma_A, point_est$sigma_I, point_est$sigma_G)
        ),
        `Boot. Lower PBCI 95%` = format_bv_value(
          c(
            conf_int$sigma_A_CI[1],
            conf_int$sigma_I_CI[1],
            conf_int$sigma_G_CI[1]
          )
        ),
        `Boot. Upper PBCI 95%` = format_bv_value(
          c(
            conf_int$sigma_A_CI[2],
            conf_int$sigma_I_CI[2],
            conf_int$sigma_G_CI[2]
          )
        )
      )

      renderGlassTable(
        data = comparison_table,
        sortable = FALSE
      )
    })

    # Keep outputs rendered even when their tab is not active
    not_suspend_when_hidden <- c(
      "overview_table_ui",
      "main_table_ui",
      "exclusion_counter_ui",
      "subject_filter_ui",
      "table_subject_filter_ui",
      "view_mode_ui",
      "anova_table_ui",
      "anova_detail_table_ui",
      "grand_summary_table_ui",
      "subject_summary_table_ui",
      "bootstrap_table_ui",
      "bootstrap_comparison_table_ui"
    )

    sapply(
      X = not_suspend_when_hidden,
      function(id) {
        shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
      }
    )
  })
}

#' Data Exploration Flyout Items
#'
#' Generates flyout menu items for the Data Exploration sidebar navigation.
#' Each [glassFlyoutItem()] targets a specific tab in the headless
#' [glassTabsetPanel()] created by [mod_data_exploration_ui()], allowing the
#' sidebar to drive tab switching via JavaScript.
#'
#' The returned items correspond to the five exploration tabs:
#' \enumerate{
#'   \item Data Overview
#'   \item Exclude / Include
#'   \item Prior Distributions
#'   \item ANOVA & Descriptive
#'   \item Bootstrap Estimates
#' }
#'
#' @param id Character string. Namespace ID matching the `id` used in
#'   [mod_data_exploration_ui()] and [mod_data_exploration_server()].
#'
#' @return A list of five [glassFlyoutItem()] tag objects, each bound to the
#'   `"exploring"` navigation group and wired to a tab value in the
#'   exploration tabset.
#'
#' @seealso [mod_data_exploration_ui()], [glassFlyoutItem()]
#'
#' @export
mod_data_exploration_flyout_items <- function(id) {
  ns <- NS(id)
  tabset_id <- ns("explore_tabs")
  list(
    glassFlyoutItem(
      "exploring", icon("table"), "Data Overview",
      tabset_id = tabset_id, tab_value = "overview"
    ),
    glassFlyoutItem(
      "exploring", icon("filter-circle-xmark"), "Exclude / Include",
      tabset_id = tabset_id, tab_value = "exclusion"
    ),
    glassFlyoutItem(
      "exploring", icon("chart-area"), "Prior Distributions",
      tabset_id = tabset_id, tab_value = "priors"
    ),
    glassFlyoutItem(
      "exploring", icon("square-poll-vertical"), "ANOVA & Descriptive",
      tabset_id = tabset_id, tab_value = "anova"
    ),
    glassFlyoutItem(
      "exploring", icon("shuffle"), "Bootstrap Estimates",
      tabset_id = tabset_id, tab_value = "bootstrap"
    )
  )
}

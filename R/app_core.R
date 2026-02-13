#' Application UI (internal)
#'
#' Builds the full Shiny UI tree for the biological variation application.
#' Called from \code{inst/app/app.R}.
#'
#' @return A Shiny UI definition.
#'
#' @importFrom shiny addResourcePath icon div tags tagList withMathJax
#'   reactiveValues observe observeEvent isolate
#' @importFrom data.table data.table
#' @keywords internal
#' @noRd
app_ui <- function() {
  addResourcePath(
    "assets",
    system.file("assets", package = "EstimatingBiologicalVariation")
  )

  glassPage(
    title = "Biological Variation Analysis",
    branding = div(
      style = "display: flex; align-items: center; gap: 10px;",
      icon("dna", style = "font-size: 24px;")
    ),
    sidebar = glassSidebar(
      inputId = "sidebar_nav",
      glassNavItem(
        tabName = "setup",
        icon = icon("cogs"),
        title = "Analysis Setup",
        active = TRUE,
        flyout_items = mod_setup_flyout_items("setup"),
        card_id = "setup-setup_card"
      ),
      glassNavItem(
        tabName = "exploring",
        icon = icon("heart-circle-plus"),
        title = "Data Exploration",
        flyout_items = mod_data_exploration_flyout_items("exploration"),
        card_id = "exploration-explore_card"
      ),
      glassNavItem(
        tabName = "model1_results",
        icon = icon("chart-bar"),
        title = "Bayesian Models",
        flyout_items = list(
          glassFlyoutItem("model1_results", icon("chart-simple"), "NTT"),
          glassFlyoutItem("model2_results", icon("chart-area"), "NTTDFGAM")
        )
      ),
      glassNavItem(tabName = "documentation", icon = icon("book"), title = "Documentation"),
      glassNavItem(tabName = "debugging", icon = icon("bug"), title = "Debug Monitor")
    ),
    header_items = tagList(
      tags$div(
        class = "glass-header-group",
        glassHeaderIndicator("hdr_data_uploaded",
          type = "diagnostics",
          icon_name = "database"
        ),
        glassHeaderIndicator("hdr_mapping_done",
          type = "diagnostics",
          icon_name = "table-columns"
        )
      ),
      tags$div(
        class = "glass-header-group",
        glassHeaderIndicator("hdr_analyte",
          type = "diagnostics",
          icon_name = "flask-vial"
        ),
        glassHeaderIndicator("hdr_material",
          type = "diagnostics",
          icon_name = "layer-group"
        ),
        glassHeaderIndicator("hdr_excluded",
          type = "diagnostics",
          icon_name = "filter-circle-xmark"
        )
      ),
      tags$div(
        class = "glass-header-group",
        glassHeaderIndicator("hdr_ntt",
          type = "diagnostics",
          icon_name = "chart-simple"
        ),
        glassHeaderIndicator("hdr_nttdfgam",
          type = "diagnostics",
          icon_name = "chart-line"
        )
      )
    ),
    withMathJax(),
    useGlassToast(),
    useGlassD3Plot(),
    useGlassAnalysisProgress(),
    tags$head(
      tags$script(src = "assets/glass_selectize.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "assets/glass_selectize.css"),
      tags$script(src = "assets/glass_table.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "assets/glass_table.css")
    ),

    # Module UI Routes
    glassRoute(title = "setup", mod_setup_ui("setup")),
    glassRoute(title = "model1_results", mod_model_ntt_ui("model1")),
    glassRoute(title = "model2_results", mod_model_nttdfgam_ui("model2")),
    glassRoute(title = "exploring", mod_data_exploration_ui("exploration")),
    glassRoute(title = "documentation", mod_documentation_ui("docs")),
    glassRoute(title = "debugging", mod_debug_ui("debug"))
  )
}


#' Application Server (internal)
#'
#' Contains all server logic for the biological variation application.
#' Called from \code{inst/app/app.R}.
#'
#' @param input,output,session Standard Shiny server arguments.
#' @keywords internal
#' @noRd
app_server <- function(input, output, session) {
  # ---------------------------------------------------------------------------
  # Central Application State
  # ---------------------------------------------------------------------------
  app_state <- reactiveValues(
    # Data state
    uploaded_data_raw = NULL,
    uploaded_data_selected_sheet = NULL,
    filtered_data = NULL,
    analysis_data = NULL,
    mapped_data = NULL,
    excluded_rows = data.table(SubjectID = character(0), SampleID = character(0), ReplicateID = character(0)),

    # Column mappings
    measurement_col = NULL,
    subject_id_col = NULL,
    sample_id_col = NULL,
    replicate_id_col = NULL,
    analyte_col = NULL,
    material_col = NULL,
    sex_col = NULL,
    group_1_col = NULL,
    group_2_col = NULL,
    group_1_display_name = NULL,
    group_2_display_name = NULL,

    # Filter selections
    selected_analyte = NULL,
    selected_material = NULL,
    selected_sex = NULL,
    selected_group_1 = NULL,
    selected_group_2 = NULL,

    # Naming
    analyte_name = NULL,
    material_name = NULL,
    sex_name = NULL,
    group_name = NULL,

    # Hyperparameters
    hyper_beta = NULL,
    hyper_cvi = NULL,
    hyper_cva = NULL,
    hyper_cvg = NULL,
    hyper_dfi = NULL,
    hyper_dfa = NULL,
    hyper_hbhr = NULL,
    hyper_beta_weakness = NULL,
    hyper_cvi_weakness = NULL,
    hyper_cva_weakness = NULL,
    hyper_cvg_weakness = NULL,
    hyper_dfi_weakness = NULL,
    hyper_dfa_weakness = NULL,
    hyper_hbhr_weakness = NULL,

    # Stan/MCMC options
    log_transformed = FALSE,
    num_iterations = 2500,
    burn_in_fraction = 0.5,
    num_chains = 4,
    adapt_delta = 0.9,
    max_treedepth = 12,
    num_cores = 4,

    # Stan models (loaded at startup)
    compiled_model_ntt = NULL,
    compiled_model_nttdfgam = NULL,

    # Analysis results
    model_ntt_results = NULL,
    model_nttdfgam_results = NULL,
    model_ntt_stale = FALSE,
    model_nttdfgam_stale = FALSE,

    # Model status
    model_ntt_status = "not_run",
    model_nttdfgam_status = "not_run",
    model_ntt_error = NULL,
    model_nttdfgam_error = NULL,

    # UI state
    current_wizard_step = "step1_upload",
    mandatory_columns_selected = FALSE
  )

  # ---------------------------------------------------------------------------
  # Load Pre-compiled Stan Models
  # ---------------------------------------------------------------------------
  glassWithProgress(title = "Loading Stan Models...", {
    .t <- bv_timer_start("app::load_stan_models")
    stan_dir <- system.file("stan", package = "EstimatingBiologicalVariation")
    model_file_1 <- file.path(stan_dir, "stan_model_1_compiled.rds")
    model_file_2 <- file.path(stan_dir, "stan_model_2_compiled.rds")

    glassIncProgress(0.1, detail = "Checking for compiled model files...")
    if (!file.exists(model_file_1) || !file.exists(model_file_2)) {
      showGlassToast(
        message = paste0(
          "One or both pre-compiled Stan model files (.rds) not found. ",
          "Please run the model compilation step first."
        ),
        title = "Missing Stan Models",
        type = "error",
        duration = NULL
      )
      stop("Missing compiled models")
    }

    glassIncProgress(0.5, detail = "Loading Model 1 (NTT)...")
    app_state$compiled_model_ntt <- readRDS(model_file_1)

    glassIncProgress(0.9, detail = "Loading Model 2 (NTTDFGAM)...")
    app_state$compiled_model_nttdfgam <- readRDS(model_file_2)

    glassIncProgress(1, detail = "Models loaded successfully!")
    bv_timer_end(.t)
  })

  # ---------------------------------------------------------------------------
  # Module Servers
  # ---------------------------------------------------------------------------
  mod_setup_server("setup", app_state)
  mod_model_ntt_server("model1", app_state)
  mod_model_nttdfgam_server("model2", app_state)
  mod_data_exploration_server("exploration", app_state)
  mod_documentation_server("docs")
  mod_debug_server("debug", app_state)

  # ---------------------------------------------------------------------------
  # Header Status Indicators
  # ---------------------------------------------------------------------------

  shiny::observe({
    has_data <- !is.null(app_state$uploaded_data_selected_sheet)
    if (has_data) {
      nr <- nrow(app_state$uploaded_data_selected_sheet)
      nc <- ncol(app_state$uploaded_data_selected_sheet)
      tip <- paste0("Data loaded: ", nr, " rows, ", nc, " columns")
    } else {
      tip <- ""
    }
    updateGlassHeaderIndicator(session, "hdr_data_uploaded",
      visible = has_data, tooltip_text = tip, status = "success"
    )
  })

  shiny::observe({
    done <- isTRUE(app_state$mandatory_columns_selected)
    updateGlassHeaderIndicator(session, "hdr_mapping_done",
      visible = done,
      tooltip_text = if (done) "All mandatory columns mapped" else "",
      status = "success"
    )
  })

  shiny::observe({
    sel <- app_state$selected_analyte
    has_sel <- !is.null(sel) && nzchar(sel)
    updateGlassHeaderIndicator(session, "hdr_analyte",
      visible = has_sel,
      tooltip_text = if (has_sel) paste0("Analyte: ", sel) else "",
      status = "info"
    )
  })

  shiny::observe({
    sel <- app_state$selected_material
    has_sel <- !is.null(sel) && length(sel) > 0 && any(nzchar(sel))
    updateGlassHeaderIndicator(session, "hdr_material",
      visible = has_sel,
      tooltip_text = if (has_sel) {
        paste0("Material: ", paste(sel, collapse = ", "))
      } else {
        ""
      },
      status = "info"
    )
  })

  shiny::observe({
    excluded <- app_state$excluded_rows
    n_excluded <- if (!is.null(excluded)) nrow(excluded) else 0L
    has_excluded <- n_excluded > 0
    updateGlassHeaderIndicator(session, "hdr_excluded",
      visible = has_excluded,
      tooltip_text = if (has_excluded) {
        paste0(n_excluded, " data point(s) excluded")
      } else {
        ""
      },
      status = "warning"
    )
  })

  shiny::observeEvent(app_state$analysis_data,
    {
      if (!is.null(app_state$model_ntt_results)) {
        app_state$model_ntt_stale <- TRUE
        app_state$model_ntt_status <- "stale"
      }
      if (!is.null(app_state$model_nttdfgam_results)) {
        app_state$model_nttdfgam_stale <- TRUE
        app_state$model_nttdfgam_status <- "stale"
      }
    },
    ignoreInit = TRUE
  )

  shiny::observe({
    list(
      app_state$hyper_beta, app_state$hyper_cvi, app_state$hyper_cva,
      app_state$hyper_cvg, app_state$hyper_dfi, app_state$hyper_dfa,
      app_state$hyper_hbhr,
      app_state$hyper_beta_weakness, app_state$hyper_cvi_weakness,
      app_state$hyper_cva_weakness, app_state$hyper_cvg_weakness,
      app_state$hyper_dfi_weakness, app_state$hyper_dfa_weakness,
      app_state$hyper_hbhr_weakness,
      app_state$log_transformed, app_state$num_iterations,
      app_state$burn_in_fraction, app_state$num_chains,
      app_state$adapt_delta, app_state$max_treedepth, app_state$num_cores
    )

    isolate({
      if (!is.null(app_state$model_ntt_results)) {
        app_state$model_ntt_stale <- TRUE
        app_state$model_ntt_status <- "stale"
      }
      if (!is.null(app_state$model_nttdfgam_results)) {
        app_state$model_nttdfgam_stale <- TRUE
        app_state$model_nttdfgam_status <- "stale"
      }
    })
  })

  shiny::observeEvent(app_state$model_ntt_results, {
    if (!is.null(app_state$model_ntt_results)) {
      app_state$model_ntt_stale <- FALSE
    }
  })

  shiny::observeEvent(app_state$model_nttdfgam_results, {
    if (!is.null(app_state$model_nttdfgam_results)) {
      app_state$model_nttdfgam_stale <- FALSE
    }
  })

  shiny::observe({
    has_results <- !is.null(app_state$model_ntt_results)
    is_stale <- isTRUE(app_state$model_ntt_stale)
    if (has_results && is_stale) {
      tip <- "NTT results may be outdated \u2014 data has changed"
      st <- "warning"
    } else if (has_results) {
      tip <- "NTT model results ready"
      st <- "success"
    } else {
      tip <- ""
      st <- "success"
    }
    updateGlassHeaderIndicator(session, "hdr_ntt",
      visible = has_results, tooltip_text = tip, status = st
    )
  })

  shiny::observe({
    has_results <- !is.null(app_state$model_nttdfgam_results)
    is_stale <- isTRUE(app_state$model_nttdfgam_stale)
    if (has_results && is_stale) {
      tip <- "NTTDFGAM results may be outdated \u2014 data has changed"
      st <- "warning"
    } else if (has_results) {
      tip <- "NTTDFGAM model results ready"
      st <- "success"
    } else {
      tip <- ""
      st <- "success"
    }
    updateGlassHeaderIndicator(session, "hdr_nttdfgam",
      visible = has_results, tooltip_text = tip, status = st
    )
  })
}

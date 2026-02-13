# Debug Module
# ------------------------------------------------------------------------------
#
# Debug monitor for inspecting application state during development.
# Displays ALL relevant app_state fields organised into collapsible sections.
#
# ------------------------------------------------------------------------------

#' Debug Module UI
#'
#' @param id Namespace ID for the module
#' @return Shiny UI elements for the debug monitor page
#' @importFrom shiny NS icon verbatimTextOutput uiOutput tags tagList
#' @noRd
mod_debug_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # -- UI State --
    glassCard(
      inputId = ns("debug_ui_state"),
      title = "UI State",
      icon = icon("display"),
      collapsible = TRUE,
      collapsed = FALSE,
      glassRow(
        glassCol(12, verbatimTextOutput(ns("section_ui_state")))
      )
    ),

    # -- Data Pipeline --
    glassCard(
      inputId = ns("debug_data_pipeline"),
      title = "Data Pipeline",
      icon = icon("database"),
      collapsible = TRUE,
      collapsed = FALSE,
      glassRow(
        glassCol(12, verbatimTextOutput(ns("section_data_pipeline")))
      )
    ),

    # -- Column Mappings --
    glassCard(
      inputId = ns("debug_col_mappings"),
      title = "Column Mappings",
      icon = icon("table-columns"),
      collapsible = TRUE,
      collapsed = FALSE,
      glassRow(
        glassCol(12, verbatimTextOutput(ns("section_col_mappings")))
      )
    ),

    # -- Filter Selections --
    glassCard(
      inputId = ns("debug_filter_selections"),
      title = "Filter Selections",
      icon = icon("filter"),
      collapsible = TRUE,
      collapsed = FALSE,
      glassRow(
        glassCol(12, verbatimTextOutput(ns("section_filter_selections")))
      )
    ),

    # -- Naming --
    glassCard(
      inputId = ns("debug_naming"),
      title = "Display Names",
      icon = icon("tag"),
      collapsible = TRUE,
      collapsed = TRUE,
      glassRow(
        glassCol(12, verbatimTextOutput(ns("section_naming")))
      )
    ),

    # -- Hyperparameters --
    glassCard(
      inputId = ns("debug_hyperparams"),
      title = "Hyperparameters",
      icon = icon("sliders"),
      collapsible = TRUE,
      collapsed = TRUE,
      glassRow(
        glassCol(12, verbatimTextOutput(ns("section_hyperparams")))
      )
    ),

    # -- Hyperparameter Weakness --
    glassCard(
      inputId = ns("debug_weakness"),
      title = "Hyperparameter Weakness",
      icon = icon("weight-hanging"),
      collapsible = TRUE,
      collapsed = TRUE,
      glassRow(
        glassCol(12, verbatimTextOutput(ns("section_weakness")))
      )
    ),

    # -- Stan / MCMC Options --
    glassCard(
      inputId = ns("debug_mcmc"),
      title = "Stan / MCMC Options",
      icon = icon("gears"),
      collapsible = TRUE,
      collapsed = TRUE,
      glassRow(
        glassCol(12, verbatimTextOutput(ns("section_mcmc")))
      )
    ),

    # -- Model Status --
    glassCard(
      inputId = ns("debug_model_status"),
      title = "Model Status & Results",
      icon = icon("chart-bar"),
      collapsible = TRUE,
      collapsed = FALSE,
      glassRow(
        glassCol(12, verbatimTextOutput(ns("section_model_status")))
      )
    ),

    # -- Compiled Models --
    glassCard(
      inputId = ns("debug_compiled_models"),
      title = "Compiled Stan Models",
      icon = icon("microchip"),
      collapsible = TRUE,
      collapsed = TRUE,
      glassRow(
        glassCol(12, verbatimTextOutput(ns("section_compiled_models")))
      )
    ),

    # -- Excluded Rows Preview --
    glassCard(
      inputId = ns("debug_excluded_rows"),
      title = "Excluded Rows",
      icon = icon("ban"),
      collapsible = TRUE,
      collapsed = TRUE,
      glassRow(
        glassCol(12, verbatimTextOutput(ns("section_excluded_rows")))
      )
    ),

    # -- Raw App State Dump --
    glassCard(
      inputId = ns("debug_raw_dump"),
      title = "Raw App State (all names)",
      icon = icon("code"),
      collapsible = TRUE,
      collapsed = TRUE,
      glassRow(
        glassCol(12, verbatimTextOutput(ns("section_raw_dump")))
      )
    )
  )
}

#' Debug Module Server
#'
#' Renders all sections of the debug monitor.
#' Every output uses \code{suspendWhenHidden = FALSE} so values are always
#' current when the user navigates to the Debug tab.
#'
#' @param id Namespace ID for the module
#' @param app_state reactiveValues object containing shared application state
#' @importFrom shiny moduleServer renderPrint outputOptions reactiveValuesToList
#' @importFrom utils head str
#' @noRd
mod_debug_server <- function(id, app_state) {

  moduleServer(id, function(input, output, session) {

    # Helper: safe dimension string for a data object
    safe_dim <- function(x) {
      if (is.null(x)) return("NULL")
      if (is.data.frame(x) || is.matrix(x)) {
        paste0(nrow(x), " rows x ", ncol(x), " cols")
      } else {
        paste0("length ", length(x))
      }
    }

    # Helper: safe column names
    safe_names <- function(x) {
      if (is.null(x)) return("NULL")
      paste(names(x), collapse = ", ")
    }

    # -- UI State ---------------------------------------------------------------
    output$section_ui_state <- renderPrint({
      cat("current_wizard_step       :", as.character(app_state$current_wizard_step), "\n")
      cat("mandatory_columns_selected:", as.character(app_state$mandatory_columns_selected), "\n")
    })

    # -- Data Pipeline ----------------------------------------------------------
    output$section_data_pipeline <- renderPrint({
      cat("uploaded_data_raw          :", safe_dim(app_state$uploaded_data_raw), "\n")
      cat("uploaded_data_selected_sheet:", safe_dim(app_state$uploaded_data_selected_sheet), "\n")
      cat("mapped_data                :", safe_dim(app_state$mapped_data), "\n")
      cat("filtered_data              :", safe_dim(app_state$filtered_data), "\n")
      cat("analysis_data              :", safe_dim(app_state$analysis_data), "\n")
      cat("excluded_rows              :", safe_dim(app_state$excluded_rows), "\n")
      cat("\n")
      # Column names of key frames
      if (!is.null(app_state$uploaded_data_selected_sheet)) {
        cat("  uploaded_data_selected_sheet cols:\n    ",
            safe_names(app_state$uploaded_data_selected_sheet), "\n")
      }
      if (!is.null(app_state$mapped_data)) {
        cat("  mapped_data cols:\n    ", safe_names(app_state$mapped_data), "\n")
      }
      if (!is.null(app_state$analysis_data)) {
        cat("  analysis_data cols:\n    ", safe_names(app_state$analysis_data), "\n")
      }
    })

    # -- Column Mappings --------------------------------------------------------
    output$section_col_mappings <- renderPrint({
      cat("measurement_col :", as.character(app_state$measurement_col %||% "NULL"), "\n")
      cat("subject_id_col  :", as.character(app_state$subject_id_col  %||% "NULL"), "\n")
      cat("sample_id_col   :", as.character(app_state$sample_id_col   %||% "NULL"), "\n")
      cat("replicate_id_col:", as.character(app_state$replicate_id_col %||% "NULL"), "\n")
      cat("analyte_col     :", as.character(app_state$analyte_col     %||% "NULL"), "\n")
      cat("material_col    :", as.character(app_state$material_col    %||% "NULL"), "\n")
      cat("sex_col         :", as.character(app_state$sex_col         %||% "NULL"), "\n")
      cat("group_1_col     :", as.character(app_state$group_1_col     %||% "NULL"), "\n")
      cat("group_2_col     :", as.character(app_state$group_2_col     %||% "NULL"), "\n")
      cat("group_1_display :", as.character(app_state$group_1_display_name %||% "NULL"), "\n")
      cat("group_2_display :", as.character(app_state$group_2_display_name %||% "NULL"), "\n")
    })

    # -- Filter Selections ------------------------------------------------------
    output$section_filter_selections <- renderPrint({
      cat("selected_analyte :", as.character(app_state$selected_analyte  %||% "NULL"), "\n")
      cat("selected_material:", paste(as.character(app_state$selected_material %||% "NULL"), collapse = ", "), "\n")
      cat("selected_sex     :", paste(as.character(app_state$selected_sex      %||% "NULL"), collapse = ", "), "\n")
      cat("selected_group_1 :", paste(as.character(app_state$selected_group_1  %||% "NULL"), collapse = ", "), "\n")
      cat("selected_group_2 :", paste(as.character(app_state$selected_group_2  %||% "NULL"), collapse = ", "), "\n")
    })

    # -- Naming -----------------------------------------------------------------
    output$section_naming <- renderPrint({
      cat("analyte_name :", as.character(app_state$analyte_name  %||% "NULL"), "\n")
      cat("material_name:", as.character(app_state$material_name %||% "NULL"), "\n")
      cat("sex_name     :", as.character(app_state$sex_name      %||% "NULL"), "\n")
      cat("group_name   :", as.character(app_state$group_name    %||% "NULL"), "\n")
    })

    # -- Hyperparameters --------------------------------------------------------
    output$section_hyperparams <- renderPrint({
      cat("hyper_beta :", as.character(app_state$hyper_beta %||% "NULL"), "\n")
      cat("hyper_cvi  :", as.character(app_state$hyper_cvi  %||% "NULL"), "\n")
      cat("hyper_cva  :", as.character(app_state$hyper_cva  %||% "NULL"), "\n")
      cat("hyper_cvg  :", as.character(app_state$hyper_cvg  %||% "NULL"), "\n")
      cat("hyper_dfi  :", as.character(app_state$hyper_dfi  %||% "NULL"), "\n")
      cat("hyper_dfa  :", as.character(app_state$hyper_dfa  %||% "NULL"), "\n")
      cat("hyper_hbhr :", as.character(app_state$hyper_hbhr %||% "NULL"), "\n")
    })

    # -- Hyperparameter Weakness ------------------------------------------------
    output$section_weakness <- renderPrint({
      cat("hyper_beta_weakness:", as.character(app_state$hyper_beta_weakness %||% "NULL"), "\n")
      cat("hyper_cvi_weakness :", as.character(app_state$hyper_cvi_weakness  %||% "NULL"), "\n")
      cat("hyper_cva_weakness :", as.character(app_state$hyper_cva_weakness  %||% "NULL"), "\n")
      cat("hyper_cvg_weakness :", as.character(app_state$hyper_cvg_weakness  %||% "NULL"), "\n")
      cat("hyper_dfi_weakness :", as.character(app_state$hyper_dfi_weakness  %||% "NULL"), "\n")
      cat("hyper_dfa_weakness :", as.character(app_state$hyper_dfa_weakness  %||% "NULL"), "\n")
      cat("hyper_hbhr_weakness:", as.character(app_state$hyper_hbhr_weakness %||% "NULL"), "\n")
    })

    # -- Stan / MCMC Options ----------------------------------------------------
    output$section_mcmc <- renderPrint({
      cat("log_transformed :", as.character(app_state$log_transformed), "\n")
      cat("num_iterations  :", as.character(app_state$num_iterations), "\n")
      cat("burn_in_fraction:", as.character(app_state$burn_in_fraction), "\n")
      cat("num_chains      :", as.character(app_state$num_chains), "\n")
      cat("adapt_delta     :", as.character(app_state$adapt_delta), "\n")
      cat("max_treedepth   :", as.character(app_state$max_treedepth), "\n")
      cat("num_cores       :", as.character(app_state$num_cores), "\n")
    })

    # -- Model Status & Results -------------------------------------------------
    output$section_model_status <- renderPrint({
      cat("--- NTT Model ---\n")
      cat("  status        :", as.character(app_state$model_ntt_status), "\n")
      cat("  has_results   :", !is.null(app_state$model_ntt_results), "\n")
      cat("  stale         :", isTRUE(app_state$model_ntt_stale), "\n")
      cat("  error         :", as.character(app_state$model_ntt_error %||% "none"), "\n")
      if (!is.null(app_state$model_ntt_results)) {
        cat("  result names  :", paste(names(app_state$model_ntt_results), collapse = ", "), "\n")
      }
      cat("\n--- NTTDFGAM Model ---\n")
      cat("  status        :", as.character(app_state$model_nttdfgam_status), "\n")
      cat("  has_results   :", !is.null(app_state$model_nttdfgam_results), "\n")
      cat("  stale         :", isTRUE(app_state$model_nttdfgam_stale), "\n")
      cat("  error         :", as.character(app_state$model_nttdfgam_error %||% "none"), "\n")
      if (!is.null(app_state$model_nttdfgam_results)) {
        cat("  result names  :", paste(names(app_state$model_nttdfgam_results), collapse = ", "), "\n")
      }
    })

    # -- Compiled Models --------------------------------------------------------
    output$section_compiled_models <- renderPrint({
      cat("compiled_model_ntt      :", class(app_state$compiled_model_ntt)[1]  %||% "NULL", "\n")
      cat("compiled_model_nttdfgam :", class(app_state$compiled_model_nttdfgam)[1] %||% "NULL", "\n")
    })

    # -- Excluded Rows ----------------------------------------------------------
    output$section_excluded_rows <- renderPrint({
      ex <- app_state$excluded_rows
      if (is.null(ex) || nrow(ex) == 0) {
        cat("No excluded rows.\n")
      } else {
        cat("Total excluded:", nrow(ex), "\n\n")
        print(utils::head(ex, 20))
        if (nrow(ex) > 20) cat("\n... and", nrow(ex) - 20, "more rows\n")
      }
    })

    # -- Raw App State Dump -----------------------------------------------------
    output$section_raw_dump <- renderPrint({
      all_names <- sort(names(reactiveValuesToList(app_state)))
      cat("app_state contains", length(all_names), "fields:\n\n")
      for (nm in all_names) {
        val <- app_state[[nm]]
        if (is.null(val)) {
          cat("  ", nm, ": NULL\n")
        } else if (is.data.frame(val) || is.matrix(val)) {
          cat("  ", nm, ": [", nrow(val), " x ", ncol(val), "]\n")
        } else if (is.list(val)) {
          cat("  ", nm, ": list(", length(val), ")\n")
        } else if (isS4(val)) {
          cat("  ", nm, ": <S4:", class(val), ">\n")
        } else if (length(val) == 1) {
          cat("  ", nm, ":", as.character(val), "\n")
        } else {
          cat("  ", nm, ": [", length(val), "] ", paste(utils::head(val, 3), collapse = ", "), "...\n")
        }
      }
    })

    # -------------------------------------------------------------------------
    # Prevent suspension so outputs are always fresh when navigating here
    # -------------------------------------------------------------------------
    output_ids <- c(
      "section_ui_state",
      "section_data_pipeline",
      "section_col_mappings",
      "section_filter_selections",
      "section_naming",
      "section_hyperparams",
      "section_weakness",
      "section_mcmc",
      "section_model_status",
      "section_compiled_models",
      "section_excluded_rows",
      "section_raw_dump"
    )
    for (oid in output_ids) {
      outputOptions(output, oid, suspendWhenHidden = FALSE)
    }
  })
}

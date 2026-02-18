# Setup Module - Analysis Setup Wizard
# ------------------------------------------------------------------------------
#
# Combines the 4-step setup wizard:
#   Step 1: Data Upload (mod_upload)
#   Step 2: Column Mapping (mod_column_map)
#   Step 3: Filter & Name (mod_filter)
#   Step 4: Hyperparameters (mod_hyperparameters)
#
# Also handles wizard navigation and the data processing pipeline
# (mapping raw data to standard columns and applying filters).
#
# ------------------------------------------------------------------------------

#' Setup Module UI
#'
#' @param id Namespace ID for the module
#' @return Shiny UI elements for the setup wizard
mod_setup_ui <- function(id) {
    ns <- NS(id)
    glassCard(
        inputId = ns("setup_card"),
        title = "Analysis Setup",
        icon = icon("cogs"),
        collapsible = FALSE,
        collapsed = FALSE,
        attached = FALSE,
        nested = FALSE,
        width = "100%",
        glassTabsetPanel(
            inputId = ns("setup_wizard"),
            selected = "step1_upload",
            color = "purple",
            boxed = TRUE,
            headless = TRUE,
            glassTabPanel(
                title = "Step 1: Upload Data",
                value = "step1_upload",
                mod_upload_ui(ns("upload_module")),
                glassRow(
                    glassCol(
                        width = 12,
                        create_wizard_nav_buttons(next_id = ns("goto_step2"))
                    )
                )
            ),
            glassTabPanel(
                title = "Step 2: Map Columns",
                value = "step2_map",
                mod_column_map_ui(ns("column_map_module")),
                glassRow(
                    glassCol(
                        width = 12,
                        create_wizard_nav_buttons(
                            prev_id = ns("back_to_step1"),
                            next_id = ns("goto_step3")
                        )
                    )
                )
            ),
            glassTabPanel(
                title = "Step 3: Filter & Name",
                value = "step3_filter",
                mod_filter_ui(ns("filter_module")),
                glassRow(
                    glassCol(
                        width = 12,
                        create_wizard_nav_buttons(
                            prev_id = ns("back_to_step2"),
                            next_id = ns("goto_step4")
                        )
                    )
                )
            ),
            glassTabPanel(
                title = "Step 4: Set Hyperparameters",
                value = "step4_params",
                mod_hyperparameters_ui(ns("hyperparameters_module")),
                glassRow(
                    glassCol(
                        width = 12,
                        create_wizard_nav_buttons(prev_id = ns("back_to_step3"))
                    )
                )
            )
        )
    )
}

#' Setup Module Server
#'
#' @param id Namespace ID for the module
#' @param app_state reactiveValues object containing shared application state
mod_setup_server <- function(id, app_state) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # =========================================================================
        # Sub-Module Servers
        # =========================================================================

        # Upload module
        uploaded_results <- mod_upload_server("upload_module")

        observe({
            app_state$uploaded_data_selected_sheet <- uploaded_results$data()
        })

        # Column mapping module
        col_selections <- mod_column_map_server(
            id = "column_map_module",
            uploaded_data = uploaded_results$data
        )

        observe({
            .t <- bv_timer_start("setup::sync_col_selections")
            req(col_selections$measurement_col())

            # Snapshot all column selections into a plain list.
            # This observe fires for EVERY individual dropdown change,
            # but we only propagate to app_state when the snapshot
            # actually differs from what is already stored.
            snapshot <- list(
                measurement  = col_selections$measurement_col(),
                subject_id   = col_selections$subject_id_col(),
                sample_id    = col_selections$sample_id_col(),
                replicate_id = col_selections$replicate_id_col(),
                analyte      = col_selections$analyte_col(),
                material     = col_selections$material_col(),
                sex          = col_selections$sex_col(),
                group_1      = col_selections$group_1_col(),
                group_2      = col_selections$group_2_col(),
                mandatory    = col_selections$mandatory_selected()
            )

            prev <- isolate(list(
                measurement  = app_state$measurement_col,
                subject_id   = app_state$subject_id_col,
                sample_id    = app_state$sample_id_col,
                replicate_id = app_state$replicate_id_col,
                analyte      = app_state$analyte_col,
                material     = app_state$material_col,
                sex          = app_state$sex_col,
                group_1      = app_state$group_1_col,
                group_2      = app_state$group_2_col,
                mandatory    = app_state$mandatory_columns_selected
            ))

            if (identical(snapshot, prev)) {
                bv_timer_end(.t)
                return()
            }

            # Only write fields that actually changed to avoid
            # spurious invalidation of downstream observers
            field_map <- list(
                measurement  = "measurement_col",
                subject_id   = "subject_id_col",
                sample_id    = "sample_id_col",
                replicate_id = "replicate_id_col",
                analyte      = "analyte_col",
                material     = "material_col",
                sex          = "sex_col",
                group_1      = "group_1_col",
                group_2      = "group_2_col",
                mandatory    = "mandatory_columns_selected"
            )
            for (nm in names(field_map)) {
                if (!identical(snapshot[[nm]], prev[[nm]])) {
                    app_state[[field_map[[nm]]]] <- snapshot[[nm]]
                }
            }
            bv_timer_end(.t)
        })

        # Filter & name module
        filter_values <- mod_filter_server(
            id = "filter_module",
            uploaded_data = uploaded_results$data,
            col_selections = col_selections
        )

        observe({
            .t <- bv_timer_start("setup::sync_filter_values")

            snapshot <- list(
                analyte   = filter_values$filter_analyte(),
                material  = filter_values$filter_material(),
                sex       = filter_values$filter_sex(),
                group_1   = filter_values$filter_group_1(),
                group_2   = filter_values$filter_group_2(),
                a_name    = filter_values$analyte_name(),
                m_name    = filter_values$analyte_material(),
                s_name    = filter_values$sex_name(),
                g_name    = filter_values$group_name()
            )

            prev <- isolate(list(
                analyte   = app_state$selected_analyte,
                material  = app_state$selected_material,
                sex       = app_state$selected_sex,
                group_1   = app_state$selected_group_1,
                group_2   = app_state$selected_group_2,
                a_name    = app_state$analyte_name,
                m_name    = app_state$material_name,
                s_name    = app_state$sex_name,
                g_name    = app_state$group_name
            ))

            if (identical(snapshot, prev)) {
                bv_timer_end(.t)
                return()
            }

            # Only write fields that actually changed to avoid
            # spurious invalidation of downstream observers
            field_map <- list(
                analyte  = "selected_analyte",
                material = "selected_material",
                sex      = "selected_sex",
                group_1  = "selected_group_1",
                group_2  = "selected_group_2",
                a_name   = "analyte_name",
                m_name   = "material_name",
                s_name   = "sex_name",
                g_name   = "group_name"
            )
            for (nm in names(field_map)) {
                if (!identical(snapshot[[nm]], prev[[nm]])) {
                    app_state[[field_map[[nm]]]] <- snapshot[[nm]]
                }
            }
            bv_timer_end(.t)
        })

        # Hyperparameters module
        hyperparams <- mod_hyperparameters_server(
            id = "hyperparameters_module",
            filter_values = filter_values
        )

        observe({
            .t <- bv_timer_start("setup::sync_hyperparams")
            # Guard: skip update if any hyperparameter value is NULL/NA
            # (e.g., user is mid-edit clearing an input field)
            req(
                hyperparams$beta_expected(),
                hyperparams$cvi_expected(),
                hyperparams$cva_expected(),
                hyperparams$cvg_expected(),
                hyperparams$dfi_expected(),
                hyperparams$dfa_expected(),
                hyperparams$hbhr_expected(),
                hyperparams$beta_weakness(),
                hyperparams$cvi_weakness(),
                hyperparams$cva_weakness(),
                hyperparams$cvg_weakness(),
                hyperparams$dfi_weakness(),
                hyperparams$dfa_weakness(),
                hyperparams$hbhr_weakness()
            )

            snapshot <- list(
                hyperparams$beta_expected(),
                hyperparams$cvi_expected(),
                hyperparams$cva_expected(),
                hyperparams$cvg_expected(),
                hyperparams$dfi_expected(),
                hyperparams$dfa_expected(),
                hyperparams$hbhr_expected(),
                hyperparams$beta_weakness(),
                hyperparams$cvi_weakness(),
                hyperparams$cva_weakness(),
                hyperparams$cvg_weakness(),
                hyperparams$dfi_weakness(),
                hyperparams$dfa_weakness(),
                hyperparams$hbhr_weakness(),
                hyperparams$log_transformed(),
                hyperparams$iter(),
                hyperparams$burn(),
                hyperparams$nchains(),
                hyperparams$adapt_delta(),
                hyperparams$max_treedepth(),
                hyperparams$number_of_cores()
            )

            prev <- isolate(list(
                app_state$hyper_beta,
                app_state$hyper_cvi,
                app_state$hyper_cva,
                app_state$hyper_cvg,
                app_state$hyper_dfi,
                app_state$hyper_dfa,
                app_state$hyper_hbhr,
                app_state$hyper_beta_weakness,
                app_state$hyper_cvi_weakness,
                app_state$hyper_cva_weakness,
                app_state$hyper_cvg_weakness,
                app_state$hyper_dfi_weakness,
                app_state$hyper_dfa_weakness,
                app_state$hyper_hbhr_weakness,
                app_state$log_transformed,
                app_state$num_iterations,
                app_state$burn_in_fraction * 100,
                app_state$num_chains,
                app_state$adapt_delta * 100,
                app_state$max_treedepth,
                app_state$num_cores
            ))

            if (identical(snapshot, prev)) {
                bv_timer_end(.t)
                return()
            }

            app_state$hyper_beta <- hyperparams$beta_expected()
            app_state$hyper_cvi <- hyperparams$cvi_expected()
            app_state$hyper_cva <- hyperparams$cva_expected()
            app_state$hyper_cvg <- hyperparams$cvg_expected()
            app_state$hyper_dfi <- hyperparams$dfi_expected()
            app_state$hyper_dfa <- hyperparams$dfa_expected()
            app_state$hyper_hbhr <- hyperparams$hbhr_expected()

            app_state$hyper_beta_weakness <- hyperparams$beta_weakness()
            app_state$hyper_cvi_weakness <- hyperparams$cvi_weakness()
            app_state$hyper_cva_weakness <- hyperparams$cva_weakness()
            app_state$hyper_cvg_weakness <- hyperparams$cvg_weakness()
            app_state$hyper_dfi_weakness <- hyperparams$dfi_weakness()
            app_state$hyper_dfa_weakness <- hyperparams$dfa_weakness()
            app_state$hyper_hbhr_weakness <- hyperparams$hbhr_weakness()

            app_state$log_transformed <- hyperparams$log_transformed()
            app_state$num_iterations <- hyperparams$iter()
            app_state$burn_in_fraction <- hyperparams$burn() / 100
            app_state$num_chains <- hyperparams$nchains()
            app_state$adapt_delta <- hyperparams$adapt_delta() / 100
            app_state$max_treedepth <- hyperparams$max_treedepth()
            app_state$num_cores <- hyperparams$number_of_cores()
            app_state$seed <- hyperparams$seed()
            bv_timer_end(.t)
        })

        # =========================================================================
        # Data Mapping & Filtering Pipeline
        # =========================================================================

        # Map uploaded data to standard column names
        observe({
            .t <- bv_timer_start("setup::map_data")
            req(
                app_state$uploaded_data_selected_sheet,
                app_state$mandatory_columns_selected
            )

            raw_df <- as.data.frame(app_state$uploaded_data_selected_sheet)

            new_df <- data.table(
                y = raw_df[[app_state$measurement_col]],
                SubjectID = as.character(raw_df[[app_state$subject_id_col]]),
                SampleID = as.character(raw_df[[app_state$sample_id_col]]),
                ReplicateID = as.character(raw_df[[app_state$replicate_id_col]])
            )

            # Add optional columns if selected
            if (!is.null(app_state$analyte_col) && app_state$analyte_col != "") {
                new_df$Analyte <- raw_df[[app_state$analyte_col]]
            }
            if (!is.null(app_state$material_col) && app_state$material_col != "") {
                new_df$Material <- raw_df[[app_state$material_col]]
            }
            if (!is.null(app_state$sex_col) && app_state$sex_col != "") {
                new_df$Sex <- raw_df[[app_state$sex_col]]
            }
            if (!is.null(app_state$group_1_col) && app_state$group_1_col != "") {
                group1_name <- app_state$group_1_col
                new_df[[group1_name]] <- raw_df[[group1_name]]
                app_state$group_1_display_name <- group1_name
            } else {
                app_state$group_1_display_name <- NULL
            }
            if (!is.null(app_state$group_2_col) && app_state$group_2_col != "") {
                group2_name <- app_state$group_2_col
                new_df[[group2_name]] <- raw_df[[group2_name]]
                app_state$group_2_display_name <- group2_name
            } else {
                app_state$group_2_display_name <- NULL
            }

            app_state$mapped_data <- new_df
            bv_timer_end(.t)
        })

        # Apply filters to create filtered_data.
        # Uses debounce to coalesce rapid changes from two convergent paths:
        #   Path A: column mapping -> map_data -> mapped_data change
        #   Path B: column mapping -> filter choices update -> selected_* change
        # Without debounce, filter_data (and the entire downstream explore chain)
        # would run once per path per column change.
        filter_data_reactive <- reactive({
            req(app_state$mapped_data)

            # Touch all reactive dependencies so invalidation is tracked
            list(
                app_state$mapped_data,
                app_state$selected_analyte,
                app_state$selected_material,
                app_state$selected_sex,
                app_state$selected_group_1,
                app_state$selected_group_2,
                app_state$group_1_display_name,
                app_state$group_2_display_name
            )

            data <- copy(app_state$mapped_data)

            # Helper: filter by column value
            apply_filter <- function(data, col_name, filter_values) {
                if (!is.null(filter_values) && length(filter_values) > 0 && col_name %in% names(data)) {
                    data <- data[data[[col_name]] %in% filter_values, ]
                }
                return(data)
            }

            # Remove NA values in measurement column
            data <- data[!is.na(y), ]

            # Apply filters
            data <- apply_filter(data, "Analyte", app_state$selected_analyte)
            data <- apply_filter(data, "Material", app_state$selected_material)
            data <- apply_filter(data, "Sex", app_state$selected_sex)
            if (!is.null(app_state$group_1_display_name)) {
                data <- apply_filter(data, app_state$group_1_display_name, app_state$selected_group_1)
            }
            if (!is.null(app_state$group_2_display_name)) {
                data <- apply_filter(data, app_state$group_2_display_name, app_state$selected_group_2)
            }

            data
        }) |> debounce(300)

        observe({
            .t <- bv_timer_start("setup::filter_data")
            app_state$filtered_data <- filter_data_reactive()
            bv_timer_end(.t)
        })

        # =========================================================================
        # Wizard Navigation
        # =========================================================================

        # Step 1 -> 2
        observeEvent(input$goto_step2,
            {
                if (!uploaded_results$file_uploaded()) {
                    showGlassToast(
                        message = "Please upload a data file to proceed.",
                        title = "No Data Uploaded",
                        type = "error",
                        duration = 5000
                    )
                } else {
                    updateGlassTabsetPanel(session, ns("setup_wizard"), selected = "step2_map")
                    app_state$current_wizard_step <- "step2_map"
                }
            },
            ignoreInit = TRUE
        )

        # Step 2 -> 1
        observeEvent(input$back_to_step1,
            {
                updateGlassTabsetPanel(session, ns("setup_wizard"), selected = "step1_upload")
                app_state$current_wizard_step <- "step1_upload"
            },
            ignoreInit = TRUE
        )

        # Step 2 -> 3
        observeEvent(input$goto_step3,
            {
                if (!app_state$mandatory_columns_selected) {
                    showGlassToast(
                        message = "Please map all mandatory columns to proceed.",
                        title = "Incomplete Column Mapping",
                        type = "error",
                        duration = 5000
                    )
                } else {
                    updateGlassTabsetPanel(session, ns("setup_wizard"), selected = "step3_filter")
                    app_state$current_wizard_step <- "step3_filter"
                }
            },
            ignoreInit = TRUE
        )

        # Step 3 -> 2
        observeEvent(input$back_to_step2,
            {
                updateGlassTabsetPanel(session, ns("setup_wizard"), selected = "step2_map")
                app_state$current_wizard_step <- "step2_map"
            },
            ignoreInit = TRUE
        )

        # Step 3 -> 4
        observeEvent(input$goto_step4,
            {
                updateGlassTabsetPanel(session, ns("setup_wizard"), selected = "step4_params")
                app_state$current_wizard_step <- "step4_params"
            },
            ignoreInit = TRUE
        )

        # Step 4 -> 3
        observeEvent(input$back_to_step3,
            {
                updateGlassTabsetPanel(session, ns("setup_wizard"), selected = "step3_filter")
                app_state$current_wizard_step <- "step3_filter"
            },
            ignoreInit = TRUE
        )

        # =========================================================================
        # Sync Flyout State with Wizard Navigation
        # =========================================================================
        # When wizard step changes (via buttons), update the sidebar flyout
        # indicator and card title to match.

        # Lookup table for step -> flyout label + icon
        step_meta <- list(
            step1_upload = list(label = "Upload Data", icon = "upload"),
            step2_map    = list(label = "Map Columns", icon = "columns"),
            step3_filter = list(label = "Filter & Name", icon = "filter"),
            step4_params = list(label = "Hyperparameters", icon = "sliders")
        )

        observeEvent(app_state$current_wizard_step, {
            meta <- step_meta[[app_state$current_wizard_step]]
            if (!is.null(meta)) {
                syncGlassFlyout(
                    session = session,
                    navTarget = "setup",
                    flyoutLabel = meta$label,
                    flyoutIcon = meta$icon
                )
            }
        })

        # When the setup wizard tab is switched via the sidebar flyout,
        # the JS switchGlassTab sets the Shiny input. Keep app_state in sync.
        observeEvent(input$setup_wizard,
            {
                app_state$current_wizard_step <- input$setup_wizard
            },
            ignoreInit = TRUE
        )
    })
}

# ==============================================================================
# Setup Flyout Items (for sidebar flyout menu)
# ==============================================================================

#' Create flyout items for the Setup sidebar navigation
#'
#' Maps each wizard step to a flyout menu entry that switches
#' the headless tabsetPanel inside the setup card.
#'
#' @param id The module namespace ID
#' @return A list of glassFlyoutItem elements
#' @export
mod_setup_flyout_items <- function(id) {
    ns <- NS(id)
    tabset_id <- ns("setup_wizard")
    list(
        glassFlyoutItem(
            "setup", icon("upload"), "Upload Data",
            tabset_id = tabset_id, tab_value = "step1_upload"
        ),
        glassFlyoutItem(
            "setup", icon("columns"), "Map Columns",
            tabset_id = tabset_id, tab_value = "step2_map"
        ),
        glassFlyoutItem(
            "setup", icon("filter"), "Filter & Name",
            tabset_id = tabset_id, tab_value = "step3_filter"
        ),
        glassFlyoutItem(
            "setup", icon("sliders"), "Hyperparameters",
            tabset_id = tabset_id, tab_value = "step4_params"
        )
    )
}

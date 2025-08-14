#
# This is the user-interface definition of a Shiny web application.
# It uses the shinydashboard package with a sidebar menu for navigation.
#

# LIBRARIES
# ------------------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(DT)
library(shinyBS)

# User functions
# ------------------------------------------------------------------------------
create_parameter_row <- function(id_stem, label_latex,
                                 val_expected, min_expected, max_expected, step_expected,
                                 val_weakness, min_weakness, max_weakness, step_weakness) {
  div(
    fluidRow(
      column(
        width = 3,
        h5(
          label_latex#,
          #style = "color: #495057; font-weight: 600; font-size: 16px;"
        )
      ),
      column(
        width = 4,
        numericInput(
          inputId = paste0("hyper_", id_stem),
          label = NULL,
          value = val_expected,
          min = min_expected,
          max = max_expected,
          step = step_expected
        )
      ),
      column(
        width = 4,
        numericInput(
          inputId = paste0("hyper_", id_stem, "_weakness"),
          label = NULL,
          value = val_weakness,
          min = min_weakness,
          max = max_weakness,
          step = step_weakness
        )
      )
    )
  )
}


# USER INTERFACE
# ------------------------------------------------------------------------------
ui <- dashboardPage(
  
  # 1. HEADER
  # --------------------------------------------------------------------------
  dashboardHeader(title = "Biological Variation"),
  
  # 2. SIDEBAR
  # --------------------------------------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_tabs",
      menuItem("Setup & Data Upload", tabName = "setup", icon = icon("cogs")),
      menuItem("Model 1 (NTT)", tabName = "model1_results", icon = icon("chart-simple")),
      menuItem("Model 2 (NTTDFGAM)", tabName = "model2_results", icon = icon("chart-area"))
    )
  ),
  
  # 3. BODY
  # --------------------------------------------------------------------------
  dashboardBody(
    withMathJax(),
    includeCSS("www/styles.css"),
    tabItems(
      # -- Tab 1: Setup and Inputs --
      tabItem(tabName = "setup",
              fluidRow(
                column(
                  width = 4,
                  div(
                    class = "dashboard-card",
                    div(
                      class = "card-header",
                      icon("upload", class = "header-icon"),
                      h3("Upload Data")
                    ),
                    div(
                      class = "card-body",
                      div(
                        class = "parameter-section",
                        h5("Upload Biological Variation Data"),
                        fileInput(  
                          inputId = "file_upload",
                          label = NULL,
                          multiple = FALSE,
                          accept = c(".csv", ".xlsx")
                        ),
                        h5("Select Sheet (only for .xlsx)"),
                        radioGroupButtons(
                          inputId = "sheet_name",
                          label = NULL,
                          choices = "No sheets before upload ...",
                          selected = NULL,
                          status = "primary",
                          size = "sm",
                          justified = TRUE,
                          disabled = TRUE
                        )
                      )
                    )
                  ),
                  div(
                    class = "dashboard-card-drop-down",
                    div(
                      class = "card-header",
                      icon("tasks", class = "header-icon"), 
                      h3("Select Columns"),
                      div(
                        style="margin-left: auto;", # Pushes buttons to the right
                        actionBttn(
                          inputId = "guess_cols_btn",
                          label = "Guess",
                          icon = icon("wand-magic-sparkles"),
                          style = "gradient",
                          color = "royal",
                          size = "sm"
                        ),
                        actionBttn(
                          inputId = "reset_cols_btn",
                          label = "Reset",
                          icon = icon("undo"),
                          style = "gradient",
                          color = "royal",
                          size = "sm"
                        )
                      )
                    ),
                    div(
                      class = "card-sub-header",
                      icon("arrow-pointer", class = "sub-header-icon"),
                      h4("Mandatory Selections")
                    ),
                    div(
                      class = "card-body",
                      div(
                        class = "parameter-section",
                        h5("Measurement Column (y)"),
                        selectInput("measurement_col", NULL, choices = c("None" = ""), selected = ""),
                        h5("Subject ID Column"),
                        selectInput("subject_id_col", NULL, choices = c("None" = ""), selected = ""),
                        h5("Sample ID Column"),
                        selectInput("sample_id_col", NULL, choices = c("None" = ""), selected = ""),
                        h5("Replicate ID Column"),
                        selectInput("replicate_id_col", NULL, choices = c("None" = ""), selected = ""),
                      )
                    ),
                    div(
                      class = "card-sub-header",
                      icon("people-group", class = "sub-header-icon"),
                      h4("Group Selections", id = "group_selections_help")
                    ),
                    bsTooltip(
                      id = "group_selections_help",
                      title = paste0(
                        "Although group selections is optional, it is ",
                        "recommended as not stratifying may cause issues."
                      ),
                      placement = "top",
                      trigger = "hover"
                    ),
                    div(
                      class = "card-body",
                      div(
                        class = "parameter-section",
                        h5("Analyte Column"),
                        selectInput("analyte_col", NULL, choices = c("None" = ""), selected = ""),
                        h5("Material Column"),
                        selectInput("material_col", NULL, choices = c("None" = ""), selected = ""),
                        h5("Sex Column"),
                        selectInput("sex_col", NULL, choices = c("None" = ""), selected = ""),
                        h5("Group 1 Column"),
                        selectInput("group_1_col", NULL, choices = c("None" = ""), selected = ""),
                        h5("Group 2 Column"),
                        selectInput("group_2_col", NULL, choices = c("None" = ""), selected = "")
                      )
                    )
                  )
                ),
                column(
                  width = 4,
                  div(
                    class = "dashboard-card-drop-down",
                    div(
                      class = "card-header",
                      icon("filter", class = "header-icon"), 
                      h3("Filter Groups")
                    ),
                    div(
                      class = "card-body",
                      uiOutput("analyte_filter_ui"),
                      uiOutput("material_filter_ui"),
                      uiOutput("sex_filter_ui"),
                      uiOutput("group_1_filter_ui"),
                      uiOutput("group_2_filter_ui")
                    )
                  ),
                  # --- Hyperparameter Setup Card ---
                  div(
                    class = "dashboard-card",
                    div(
                      class = "card-header",
                      icon("sliders-h", class = "header-icon"),
                      h3("Set Hyperparameters")
                    ),
                    div(
                      fluidRow(
                        column(
                          width = 3,
                          h4(
                            "Parameter",
                          )
                        ),
                        column(
                          width = 4,
                          h4(
                            "Mean",
                            icon(
                              name = "question-circle",
                              class = "tool-tip-icon"
                            ),
                            id = "prior_mean_help"
                          )
                        ),
                        column(
                          width = 4,
                          h4(
                            "SD Factor",
                            icon(
                              name = "question-circle",
                              class = "tool-tip-icon"
                            ),
                            id = "prior_weakness_help"
                          )
                        )
                      ),
                      bsTooltip(
                        id = "prior_mean_help",
                        title = paste0(
                          "The expected value or the population mean ",
                          "of the prior distribution of the paramter θ: E[θ]. ",
                          "Should be based on prior knowledge of θ, ",
                          "and not from the same data you use here to model θ!"
                        ),
                        placement = "bottom",
                        trigger = "hover"
                      ),
                      bsTooltip(
                        id = "prior_weakness_help",
                        title = paste0(
                          "Controls the Standard Deviation (SD) of the prior ",
                          "distribution of the paramter θ: SD[θ] = Factor * E[θ]. ",
                          "A higher value means a weaker, ",
                          "less informative prior, allowing the data to ",
                          "have more influence."
                        ),
                        placement = "bottom",
                        trigger = "hover"
                      )
                    ),
                    div(
                      class = "card-body",
                      div(
                        class = "parameter-section",
                        # --- Parameter Rows (created by the helper function) ---
                        create_parameter_row(
                          "beta", "\\(\\beta\\)",
                          val_expected = 7, min_expected = -1000, max_expected = 1e6, step_expected = 0.01,
                          val_weakness = 0.5, min_weakness = 0.1, max_weakness = 4, step_weakness = 0.1
                        ),
                        
                        create_parameter_row(
                          "cvi", "\\(\\mathrm{CV}_{I}(\\%)\\)",
                          val_expected = 9.4, min_expected = 0, max_expected = 80, step_expected = 0.1,
                          val_weakness = 2, min_weakness = 1e-2, max_weakness = 4, step_weakness = 0.1
                        ),
                        
                        create_parameter_row(
                          "cva", "\\(\\mathrm{CV}_{A}(\\%)\\)",
                          val_expected = 2.5, min_expected = 0, max_expected = 20, step_expected = 0.1,
                          val_weakness = 2, min_weakness = 1e-2, max_weakness = 12, step_weakness = 0.2
                        ),
                        
                        create_parameter_row(
                          "cvg", "\\(\\mathrm{CV}_{G}(\\%)\\)",
                          val_expected = 8.0, min_expected = 0, max_expected = 200, step_expected = 0.1,
                          val_weakness = 2, min_weakness = 1e-2, max_weakness = 4, step_weakness = 0.1
                        ),
                        
                        create_parameter_row(
                          "dfi", "\\(\\mathrm{df}_{I}\\)",
                          val_expected = 20, min_expected = 2, max_expected = 1000, step_expected = 1,
                          val_weakness = round(1/sqrt(2), 1L), min_weakness = 1e-2, max_weakness = 12, step_weakness = 0.01
                        ),
                        
                        create_parameter_row(
                          "dfa", "\\(\\mathrm{df}_{A}\\)",
                          val_expected = 20, min_expected = 2, max_expected = 1000, step_expected = 1,
                          val_weakness = round(1/sqrt(2), 1L), min_weakness = 1e-2, max_weakness = 12, step_weakness = 0.01
                        )
                      )
                    )
                  ),
                  div(
                    class = "dashboard-card",
                    div(
                      class = "card-header",
                      icon("tags", class = "header-icon"),
                      h3("Define Names")
                    ),
                    div(
                      class = "card-body",
                      textInput("analyte_name", "Analyte Name", placeholder = "e.g., Glucose"),
                      textInput("analyte_material", "Material Name", placeholder = "e.g., Plasma"),
                      textInput("sex_name", "Sex Name", placeholder = "e.g., Females"),
                      textInput("group_name", "Group Name", placeholder = "e.g., Healthy")
                    )
                  )
                ),
                column(
                  width = 4,
                  div(
                    class = "dashboard-card",
                    div(
                      class = "card-header",
                      icon("microchip", class = "header-icon"),
                      h3("Advanced Modelling Options")
                    ),
                    div(
                      class = "card-body",
                      div(
                        class = "parameter-section",
                        h5("Apply Log Transformation"),
                        radioGroupButtons(
                          inputId = "log_transformed",
                          label = NULL,
                          choiceNames = c("Yes", "No"),
                          choiceValues = c("Yes", "No"),
                          justified = TRUE,
                          status = "primary",
                          selected = "No",
                          disabled = TRUE
                        )
                      ),
                      div(
                        class = "parameter-section",
                        h5("Select Number of Iterations"),
                        sliderTextInput(
                          inputId = "iter",
                          label = NULL,
                          choices = c(
                            seq(100, 2500, by = 100),
                            seq(3000, 7500, by = 500),
                            seq(10000, 25000, by = 2500)
                          ),
                          selected = 1000,
                          grid = FALSE,
                          from_min = 100,
                          to_max = 1e5,
                          post = " Iterations"
                        ),
                        h5("Select Burnin Fraction"),
                        sliderTextInput(
                          inputId = "burn",
                          label = NULL,
                          choices = seq(from = 0, to = 100),
                          selected = 50,
                          grid = FALSE,
                          from_min = 0,
                          to_max = 100,
                          post = " %"
                        ),
                        h5("Select Number of Chains"),
                        sliderTextInput(
                          inputId = "nchains",
                          label = NULL,
                          choices = seq(from = 1, to = 8),
                          selected = 4,
                          grid = FALSE,
                          from_min = 1,
                          to_max = 8,
                          post = " Chains"
                        ),
                        h5("Select Acceptance Probability"),
                        sliderTextInput(
                          inputId = "adapt_delta",
                          label = NULL,
                          choices = c(
                            seq(from = 60, to = 99),
                            99.5, 99.9, 99.99, 99.999
                          ),
                          selected = 90,
                          grid = FALSE,
                          from_min = 60,
                          to_max = 99.999,
                          post = " %"
                        ),
                        h5("Select Maximum Treedepth"),
                        sliderTextInput(
                          inputId = "max_treedepth",
                          label = NULL,
                          choices = seq(5, 20),
                          selected = 10,
                          grid = FALSE,
                          from_min = 5,
                          to_max = 20
                        )
                      )
                    )
                  )
                )
              )
      ),
      
      # -- Model 1 Results Tab --
      tabItem(
        tabName = "model1_results",
        div(
          class = "page-header",
          h1(
            class = "main-title",
            icon("chart-simple"),
            "Sample from the NTT Model"
          ),
          actionBttn(
            inputId = "run_analysis_model1_btn",
            label = "Run NTT Model",
            icon = icon("play"),
            style = "gradient",
            color = "royal",
            size = "lg"
          ),
          downloadBttn(
            outputId = "download_results_model1_btn",
            label = "Download",
            icon = icon("download"),
            style = "gradient",
            color = "royal",
            size = "lg"
          )
        ),
        # --- NTT tabsetPanel ---
        tabsetPanel(
          id = "model_ntt_tabs",
          type = "pills",
          # --- Summary Statistics ---
          tabPanel(
            title = "Summary Statistics",
            value = "summary_stats_ntt",
            icon = icon("calculator"),
            div(
              class = "dashboard-card",
              div(
                class = "card-header",
                icon("calculator", class = "header-icon"),
                h3("High Level Summary Statistics From Posterior and Predictive Distributions")
              ),
              div(
                class = "card-body",
                withSpinner(
                  DT::DTOutput("results_table_model1")
                )
              )
            )
          ),
          # --- Subject-Specific CV_i Plot ---
          tabPanel(
            title = "Subject-Specific CV Plot",
            value = "subject_cvi_plot_ntt",
            icon = icon("person-dress"),
            div(
              class = "dashboard-card",
              div(
                class = "card-header",
                icon("calculator", class = "header-icon"),
                h3("Subject-Wise \\(\\mathrm{CV}_{I}(\\%)\\) + Credible Intervals Plot")
              ),
              div(
                class = "card-body",
                div(
                  class = "plot-container",
                  style = "max-width: 100%; overflow-x: auto;",
                  withSpinner(
                    plotOutput("subject_plot_model1", height = "600px")
                  )
                )
              )
            )
          ),
          # --- Peek at Filtered Data ---
          tabPanel(
            title = "Filtered Data",
            value = "filtered_data_ntt",
            icon = icon("filter"),
            div(
              class = "dashboard-card",
              div(
                class = "card-header",
                icon("filter", class = "header-icon"),
                h3("Filtered Data Sent to the NTT Model")
              ),
              div(
                class = "card-body",
                withSpinner(
                  DT::DTOutput("filtered_data_table_model1")
                )
              )
            )
          )
        )
      ),
      
      # --- Model 2 Results Tab ---
      tabItem(
        tabName = "model2_results",
        
        div(
          class = "page-header",
          h1(
            class = "main-title",
            icon("chart-area"),
            "Sample from the NTTDFGAM Model"
          ),
          actionBttn(
            inputId = "run_analysis_model2_btn",
            label = "Run NTTDFGAM Model",
            icon = icon("play"),
            style = "gradient",
            color = "royal",
            size = "lg"
          ),
          downloadBttn(
            outputId = "download_results_model2_btn",
            label = "Download",
            icon = icon("download"),
            style = "gradient",
            color = "royal",
            size = "lg"
          )
        ),
        # --- NTTDFGAM tabsetPanel ---
        tabsetPanel(
          id = "model_nttdfgam_tabs",
          type = "pills",
          # --- Summary Statistics ---
          tabPanel(
            title = "Summary Statistics",
            value = "summary_stats_nttdfgam",
            icon = icon("calculator"),
            div(
              class = "dashboard-card",
              div(
                class = "card-header",
                icon("calculator", class = "header-icon"),
                h3("High Level Summary Statistics From Posterior and Predictive Distributions")
              ),
              div(
                class = "card-body",
                withSpinner(
                  DT::DTOutput("results_table_model2")
                )
              )
            )
          ),
          # --- Subject-Specific CV_i Plot ---
          tabPanel(
            title = "Subject-Specific CV Plot",
            value = "subject_cvi_plot_nttdfgam",
            icon = icon("person"),
            div(
              class = "dashboard-card",
              div(
                class = "card-header",
                icon("calculator", class = "header-icon"),
                h3("Subject-Wise \\(\\mathrm{CV}_{I}(\\%)\\) + Credible Intervals Plot")
              ),
              div(
                class = "card-body",
                div(
                  class = "plot-container",
                  style = "max-width: 100%; overflow-x: auto;",
                  withSpinner(
                    plotOutput("subject_plot_model2", height = "600px")
                  )
                )
              )
            )
          ),
          # --- Peek at Filtered Data ---
          tabPanel(
            title = "Filtered Data",
            value = "filtered_data_nttdfgam",
            icon = icon("filter"),
            div(
              class = "dashboard-card",
              div(
                class = "card-header",
                icon("filter", class = "header-icon"),
                h3("Filtered Data Sent to the NTTDFGAM Model")
              ),
              div(
                class = "card-body",
                withSpinner(
                  DT::DTOutput("filtered_data_table_model2")
                )
              )
            )
          )
        )
      )
    )
  )
)

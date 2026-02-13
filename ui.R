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
library(shinybusy)

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

# Creates a navigation button row for the wizard
create_wizard_nav_buttons <- function(prev_id = NULL, next_id = NULL) {
  div(class = "button-container",
      style = "display: flex; justify-content: space-between; margin-top: 20px;",
      if (!is.null(prev_id)) {
        actionBttn(
          inputId = prev_id,
          label = "Previous",
          icon = icon("arrow-left"),
          style = "gradient",
          color = "default",
          size = "md"
        )
      } else {
        div() # Empty div to maintain spacing
      },
      if (!is.null(next_id)) {
        actionBttn(
          inputId = next_id,
          label = "Next",
          icon = icon("arrow-right"),
          style = "gradient",
          color = "royal",
          size = "md"
        )
      }
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
      menuItem("Model 2 (NTTDFGAM)", tabName = "model2_results", icon = icon("chart-area")),
      menuItem("Data Exploration", tabName = "exploring", icon = icon("heart-circle-plus")),
      menuItem("Documentation", tabName = "documentation", icon = icon("book")),
      menuItem("Debug Monitor", tabName = "debugging", icon = icon("bug"))
    )
  ),
  
  # 3. BODY
  # --------------------------------------------------------------------------
  dashboardBody(
    withMathJax(),# --- Allow LaTex style mathematics ---
    includeCSS("www/styles.css"),# --- Load CSS ---
    tabItems(
      # --- Tab 1: Setup and Data Upload ---
      tabItem(
        tabName = "setup",
        div(
          class = "page-header",
          h1(
            class = "main-title",
            icon("cogs"),
            "Analysis Setup"
          )
        ),
        
        # --- Wizard Setup (Four Steps) ---
        shiny::tabsetPanel(
          id = "setup_wizard",
          type = "pills",# --- Wizard Setup (1 / 4) - Upload Data ---
          selected = "step1_upload",
          
          shiny::tabPanel(
            title = "1",
            value = "step1_upload",
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
                  ),
                  create_wizard_nav_buttons(next_id = "goto_step2", prev_id = NULL)
                )
              )
            )
          ),# --- Wizard Setup (2 / 4) - Mapping Columns ---
          shiny::tabPanel(
            title = "2",
            value = "step2_map",
            div(
              class = "dashboard-card-drop-down",
              div(
                class = "card-header",
                icon("tasks", class = "header-icon"), 
                h3("Select Columns"),
                div(
                  style="margin-left: auto;",
                  actionBttn(
                    inputId = "guess_cols_btn",
                    label = "Guess",
                    icon = icon("wand-magic-sparkles"),
                    style = "gradient",
                    color = "royal",
                    size = "md"
                  ),
                  actionBttn(
                    inputId = "reset_cols_btn",
                    label = "Reset",
                    icon = icon("undo"),
                    style = "gradient",
                    color = "royal",
                    size = "md"
                  )
                )
              ),
              fluidRow(
                column(
                  width = 6,
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
                      selectInput("replicate_id_col", NULL, choices = c("None" = ""), selected = "")
                    )
                  )
                ),
                column(
                  width = 6,
                  div(
                    class = "card-sub-header",
                    icon("people-group", class = "sub-header-icon"),
                    h4("Group & Population Selections", id = "group_selections_help")
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
                      h5("Material / Matrix Column"),
                      selectInput("material_col", NULL, choices = c("None" = ""), selected = ""),
                      h5("Sex Column"),
                      selectInput("sex_col", NULL, choices = c("None" = ""), selected = ""),
                      h5("Deseases Column"),
                      selectInput("deseases_col", NULL, choices = c("None" = ""), selected = "")
                    )
                  )
                )
              ),
              div(
                class = "card-sub-header",
                icon("people-group", class = "sub-header-icon"),
                h4("Custom Group Selections", id = "custom_group_selections_help")
              ),
              bsTooltip(
                id = "custom_group_selections_help",
                title = paste0(
                  "Custom groups are groups other than analyte, material ",
                  "(i.e., sample matrix), sex and deseases ",
                  "(or state of wellbeing). May for example be age, sampling ",
                  "interval or region. Only two custom groups are allowed."
                ),
                placement = "top",
                trigger = "hover"
              ),
              div(
                class = "card-body",
                div(
                  class = "parameter-section",
                  fluidRow(
                    column(
                      width = 6,
                      h5("Custom Group 1 Column"),
                      selectInput("group_1_col", NULL, choices = c("None" = ""), selected = "")
                    ),
                    column(
                      width = 6,
                      h5("Custom Group 2 Column"),
                      selectInput("group_2_col", NULL, choices = c("None" = ""), selected = "")
                    )
                  )
                ),
                create_wizard_nav_buttons(prev_id = "back_to_step1", next_id = "goto_step3")
              )
            )
          ),# --- Wizard Setup (3 / 4) - Group Stratification & Naming ---
          shiny::tabPanel(
            title = "3",
            value = "step3_filter",
            div(
              class = "dashboard-card-drop-down",
              div(
                class = "card-header",
                icon("filter", class = "header-icon"), 
                h3("Filter Data & Define Names")
              ),
              fluidRow(
                column(
                  width = 6,
                  div(
                    class = "card-sub-header",
                    icon("filter", class = "sub-header-icon"),
                    h4("Filter by Groups")
                  ),
                  div(
                    class = "card-body",
                    div(
                      class = "parameter-section",
                      uiOutput("analyte_filter_ui"),
                      uiOutput("material_filter_ui"),
                      uiOutput("sex_filter_ui"),
                      uiOutput("group_1_filter_ui"),
                      uiOutput("group_2_filter_ui")
                    )  
                  )
                ),
                column(
                  width = 6,
                  div(
                    class = "card-sub-header",
                    icon("pen", class = "sub-header-icon"),
                    h4("Set Names to Groups")
                  ),
                  div(
                    class = "card-body",
                    div(
                      class = "parameter-section",
                      textInput("analyte_name", "Analyte Name", placeholder = "e.g., Glucose"),
                      textInput("analyte_material", "Material Name", placeholder = "e.g., Plasma"),
                      textInput("sex_name", "Sex Name", placeholder = "e.g., Females"),
                      textInput("group_name", "Group Name", placeholder = "e.g., Healthy")
                    )
                  )
                )
              ),
              div(
                class = "card-body",
                create_wizard_nav_buttons(prev_id = "back_to_step2", next_id = "goto_step4")
              )
            )
          ),# --- Wizard Setup (4 / 4) - Hyperparameters & Advanced Settings ---
          shiny::tabPanel(
            title = "4",
            value = "step4_params",
            fluidRow(
              column(
                width = 6,
                div(
                  class = "dashboard-card",
                  div(
                    class = "card-header",
                    icon("sliders-h", class = "header-icon"),
                    h3("Set Hyperparameters")
                  ),
                  div(
                    class = "card-body",
                    fluidRow(
                      column(
                        width = 3,
                        h4("Parameter")
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
                  
                  # --- Hyperparameter Selections ---
                  div(
                    class = "card-body",
                    div(
                      class = "parameter-section",
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
                        val_weakness = round(1/sqrt(2), 2L), min_weakness = 1e-2, max_weakness = 12, step_weakness = 0.01
                      ),
                      create_parameter_row(
                        "dfa", "\\(\\mathrm{df}_{A}\\)",
                        val_expected = 20, min_expected = 2, max_expected = 1000, step_expected = 1,
                        val_weakness = round(1/sqrt(2), 2L), min_weakness = 1e-2, max_weakness = 12, step_weakness = 0.01
                      ),
                      create_parameter_row(
                        "hbhr", "\\(\\mathrm{HBHR}(\\%)\\)",
                        val_expected = 50, min_expected = 2, max_expected = 200, step_expected = 1,
                        val_weakness = round(2/3, 2L), min_weakness = 1e-2, max_weakness = 12, step_weakness = 0.01
                      )
                    ),
                    create_wizard_nav_buttons(prev_id = "back_to_step3")
                  )
                )
              ),
              # --- Advanced Modelling Options (for Bayesian Models) ---
              column(
                width = 6,
                box(
                  title = tagList(
                    icon(
                      name = "microchip"
                    ),
                    "Advanced Modelling Options"
                  ),
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  width = 12,
                  status = "primary",
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
                      disabled = FALSE
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
                      selected = 2500,
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
                      choices = seq(from = 2, to = parallel::detectCores()),
                      selected = ifelse(parallel::detectCores() >= 12, 8, 4),
                      grid = FALSE,
                      from_min = 2,
                      to_max = parallel::detectCores(),
                      post = " Chains"
                    ),
                    h5("Select Acceptance Probability"),
                    sliderTextInput(
                      inputId = "adapt_delta",
                      label = NULL,
                      choices = c(
                        seq(from = 75, to = 99),
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
                      selected = 12,
                      grid = FALSE,
                      from_min = 8,
                      to_max = 20
                    ),
                    h5("Select Number of Cores"),
                    sliderTextInput(
                      inputId = "number_of_cores",
                      label = NULL,
                      choices = seq(1, parallel::detectCores()),
                      selected = ifelse(parallel::detectCores() >= 12, 8, 4),
                      grid = FALSE
                    )
                  )
                )
              )
            )
          )
        )
      ),
      # --- Model 1 Module - NTT Bayesian Model - Tables and Plots ---
      tabItem(
        tabName = "model1_results",
        div(
          class = "dashboard-card",
          div(
            class = "card-header",
            h1(
              icon(
                name = "chart-simple"
              ),
              "Sample from the NTT Model"
            ),
            div(
              style = "margin-left: auto;",
              actionBttn(
                inputId = "run_analysis_model1_btn",
                label = "Run NTT Model",
                icon = icon("play"),
                style = "gradient",
                color = "royal",
                size = "md"
              ),
              downloadBttn(
                outputId = "download_results_model1_btn",
                label = "Download",
                icon = icon("download"),
                style = "gradient",
                color = "royal",
                size = "md"
              )
            )
          )
        ),
        # --- Construct Tabset Panel for the NTT Model Module ---
        tabsetPanel(
          id = "model_ntt_tabs",
          type = "pills",
          # --- Panel 1 / 5 - NTT - High-Level Summary Statistics ---
          tabPanel(
            title = "",
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
                  ui_element = DT::DTOutput("results_table_model1"),
                  image = "https://media0.giphy.com/media/v1.Y2lkPTc5MGI3NjExY3JkYjF0OXJjdjUyd2JwNWJhOXBjeHVjZXB4aWxlMWdxNm56Z2J2eSZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/fefXhtGG2NeynEUHtU/giphy.gif",
                  image.width = 300
                )
              )
            )
          ),
          # --- Panel 2 / 5 - NTT - Subject-Wise CV_p(i) + CrIs Plot ---
          tabPanel(
            title = "",
            value = "subject_cvi_plot_ntt",
            icon = icon("person-dress"),
            div(
              class = "dashboard-card",
              div(
                class = "card-header",
                icon("person-dress", class = "header-icon"),
                h3("Subject-Wise \\(\\mathrm{CV}_{I}(\\%)\\) + Credible Intervals Plot"),
                downloadBttn(
                  outputId = "raw_data_download_2_ntt",
                  label = "Raw Data",
                  icon = icon("download"),
                  style = "gradient",
                  color = "royal",
                  size = "xs"
                )
              ),
              div(
                class = "card-body",
                withSpinner(
                  ui_element = plotOutput(
                    outputId = "subject_plot_model1",
                    width = "100%",
                    height = "400px"
                  ),
                  image = "https://media2.giphy.com/media/v1.Y2lkPTc5MGI3NjExZmx0bnF2YjB5eWN1NmJodG9vOGoyOXlud2x3ZDZxcmdma3ZrM2IxNSZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/VegxrmsHVdYS4E0QKH/giphy.gif",
                  image.width = 300
                )
              )
            )
          ),
          # --- Panel 3 / 5 - NTT - Subject-Wise Concentration Versus CV_p(i) ---
          tabPanel(
            title = "",
            value = "cv_vs_conc_ntt",
            icon = icon("vial"),
            div(
              class = "dashboard-card",
              div(
                class = "card-header",
                icon("vial", class = "header-icon"),
                h3("Subject-Wise Concentration Against \\(\\mathrm{CV}_{I}(\\%)\\)")
              ),
              div(
                class = "card-body",
                withSpinner(
                  ui_element = plotOutput(
                    outputId = "filtered_data_table_model1",
                    width = "100%",
                    height = "400px"
                  ),
                  image = "https://media.giphy.com/media/v1.Y2lkPWVjZjA1ZTQ3dWYzMXhxdGEya3FyZjBuaXc2Y2V1OTdtZGp1MXI0YXQ5ODd3d2g3ZSZlcD12MV9naWZzX3NlYXJjaCZjdD1n/jUhkz8bRXfFqpwP34k/giphy.gif",
                  image.width = 300
                )
              )
            )
          ),
          # --- Panel 4 / 5 - NTT - Subject-Wise CV_p(i) versus RCV (%) ---
          tabPanel(
            title = "",
            value = "rcv_ntt",
            icon = icon("arrow-right-arrow-left"),
            div(
              class = "dashboard-card",
              div(
                class = "card-header",
                icon("arrow-right-arrow-left", class = "header-icon"),
                h3("Subject-Wise \\(\\mathrm{CV}_{I}(\\%)\\) Against Reference Change Values")
              ),
              div(
                class = "card-body",
                withSpinner(
                  ui_element = plotOutput(
                    outputId = "rcv_plot_ntt",
                    width = "100%",
                    height = "400px"
                  ),
                  image = "https://media.giphy.com/media/v1.Y2lkPWVjZjA1ZTQ3dWYzMXhxdGEya3FyZjBuaXc2Y2V1OTdtZGp1MXI0YXQ5ODd3d2g3ZSZlcD12MV9naWZzX3NlYXJjaCZjdD1n/jUhkz8bRXfFqpwP34k/giphy.gif",
                  image.width = 300
                )
              )
            )
          ),
          # --- Panel 5 / 5 - NTT - More Plots & Sampling Diagnostics ---
          tabPanel(
            title = "Advanced",
            value = "diagnostics_and_more_ntt",
            icon = icon("brain"), 
            div(
              class = "dashboard-card",
              div(
                class = "card-header",
                icon("chart-area", class = "header-icon"),
                h3("Posterior Density Plots")
              ),
              div(
                class = "card-body",
                fluidRow(
                  column(
                    width = 4,
                    div(
                      class = "parameter-section",
                      h5("Select Parameters to Plot"),
                      radioGroupButtons(
                        inputId = "selected_posterior_parameters_ntt",
                        choiceNames = c(
                          "Univariate",
                          "\\(\\mathrm{CV}_{p(i)}\\)",
                          "\\(\\mu_{p(i)}\\)"
                        ),
                        choiceValues = c(
                          "univariate",
                          "subject_cvs",
                          "h_set_points"
                        ),
                        status = "primary",
                        justified = TRUE
                      )
                    )
                  ),
                  column(
                    width = 4,
                    div(
                      class = "parameter-section",
                      h5("Include Histogram"),
                      radioGroupButtons(
                        inputId = "included_histogram_to_posterior_plots_ntt",
                        choiceNames = c(
                          "Yes",
                          "No"
                        ),
                        choiceValues = c(
                          "Yes",
                          "No"
                        ),
                        status = "primary",
                        justified = TRUE
                      )
                    )  
                  )
                ),
                withSpinner(
                  plotOutput(
                    outputId = "posterior_density_plots_ntt",
                    width = "100%",
                    height = "800px"
                  ),
                  image = "https://media1.giphy.com/media/v1.Y2lkPTc5MGI3NjExY210b2VmdGxyeTNiZnBiNzJkYTRzYWVkdzZqdXZzamFpNnR4NXZ3cSZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/l378c04F2fjeZ7vH2/giphy.gif",
                  image.width = 300
                )
              )
            ),
            div(
              class = "dashboard-card",
              div(
                class = "card-header",
                icon("chart-line", class = "header-icon"),
                h3("Trace Plots")
              ),
              div(
                class = "card-body",
                div(
                  class = "parameter-section",
                  h5("Select Parameters to Plot"),
                  radioGroupButtons(
                    inputId = "selected_trace_plot_parameters_ntt",
                    choiceNames = c(
                      "Univariate",
                      "\\(\\mathrm{CV}_{p(i)}\\)",
                      "\\(\\mu_{p(i)}\\)"
                    ),
                    choiceValues = c(
                      "univariate",
                      "subject_cvs",
                      "h_set_points"
                    ),
                    status = "primary",
                    justified = TRUE
                  )
                ),
                withSpinner(
                  plotOutput(
                    outputId = "trace_plots_ntt",
                    width = "100%",
                    height = "800px"
                  ),
                  image = "https://media0.giphy.com/media/v1.Y2lkPTc5MGI3NjExbTFkYnYxMng0emFqbHNtdDh1dTFmcTZncG5oYTNpam9zczU0YW1nNiZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/33PMXr72xOqBdOUzTO/giphy.gif",
                  image.width = 300
                )
              )
            ),
            div(
              class = "dashboard-card",
              div(
                class = "card-header",
                icon("stethoscope", class = "header-icon"),
                h3("MCMC Sampling Diagnostics")
              ),
              div(
                class = "card-body",
                div(
                  class = "parameter-section",
                  h5("Effective Sample Sizes as Percentages"),
                  radioGroupButtons(
                    inputId = "ess_as_percentages_ntt",
                    choiceNames = c(
                      "Yes",
                      "No"
                    ),
                    choiceValues = c(
                      "Yes",
                      "No"
                    ),
                    status = "primary",
                    justified = TRUE
                  )
                ),
                withSpinner(
                  ui_element = DT::DTOutput(
                    outputId = "mcmc_parameter_diagnostics_ntt"
                  ),
                  image = "https://media0.giphy.com/media/v1.Y2lkPTc5MGI3NjExa2hzYW9xY2R3NW5qdXlmbW82YjhkeTM5ejFraXZ1aDkzeXAyOGZ3aiZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/hvXcXEyDpdV1uZJ0nJ/giphy.gif",
                  image.width = 300
                )
              )
            )
          )
        )
      ),
      # --- Model 2 Module - NTTDFGAM Bayesian Model - Tables and Plots ---
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
        # --- Construct Tabset Panel for the NTTDFGAM Model Module ---
        shiny::tabsetPanel(
          id = "model_nttdfgam_tabs",
          type = "pills",
          # --- Panel 1 / 3 - NTTDFGAM - High-Level Summary Statistics ---
          shiny::tabPanel(
            title = "",
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
                  ui_element = DT::DTOutput(
                    outputId = "results_table_model2"
                  ),
                  color = "#605ca8",
                  type = 4
                )
              )
            )
          ),
          # --- Panel 2 / 3 - NTTDFGAM - Subject-Wise CV_p(i) + CrIs Plot ---
          tabPanel(
            title = "",
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
                  withSpinner(
                    ui_element = plotOutput(
                      outputId = "subject_plot_model2",
                      width = "100%",
                      height = "400px"
                    ),
                    color = "#605ca8",
                    type = 4
                  )
                )
              )
            )
          ),
          # --- Panel 3 / 3 - NTTDFGAM - Concentration Versus CV_p(i) ---
          tabPanel(
            title = "",
            value = "cv_vs_conc_nttdfgam",
            icon = icon("vial"),
            div(
              class = "dashboard-card",
              div(
                class = "card-header",
                icon("poll", class = "header-icon"),
                h3("Subject-Wise Concentration Against \\(\\mathrm{CV}_{I}(\\%)\\)")
              ),
              div(
                class = "card-body",
                div(
                  class = "plot-container",
                  withSpinner(
                    ui_element = plotOutput(
                      outputId = "filtered_data_table_model2",
                      width = "100%",
                      height = "400px"
                    ),
                    color = "#605ca8",
                    type = 4
                  )
                )
              )
            )
          )
        )
      ),
      # --- Data Exploration & Data Removal Module ---
      tabItem(
        tabName = "exploring",
        div(
          class = "page-header",
          h1(
            class = "main-title",
            icon("chart-pie"),
            "Data Exploration & Exclusion"
          )
        ),
        tabsetPanel(
          id = "exploring_tabs",
          type = "pills",
          # --- Panel 1 / 4 - Data Exploration & Exclusion - Exlude Data ---
          tabPanel(
            title = "Exlcude Data",
            value = "exclude_data_tab",
            icon = icon("indent"),
            fluidRow(
              column(
                width = 6,
                div(
                  class = "dashboard-card",
                  div(
                    class = "card-header",
                    icon("table-list", class = "header-icon"),
                    h3("Active Data Points")
                  ),
                  div(
                    class = "card-body",
                    p("Select a row in this table and click 'Exclude' to remove it from the analysis."),
                    actionBttn(
                      inputId = "exclude_btn",
                      label = "Exclude Selected Point",
                      icon = icon("minus-circle"),
                      style = "gradient",
                      color = "danger",
                      size = "sm",
                      block = TRUE
                    ),
                    withSpinner(
                      ui_element = DT::DTOutput(
                        outputId = "main_table"
                      )
                    )
                  )
                )
              ),
              column(
                width = 6,
                div(
                  class = "dashboard-card",
                  div(
                    class = "card-header",
                    icon("indent", class = "header-icon"),
                    h3("Excluded Data Points")
                  ),
                  div(
                    class = "card-body",
                    p("Select a row in this table and click 'Restore' to add it back to the analysis."),
                    actionBttn(
                      inputId = "restore_btn",
                      label = "Restore Selected Point",
                      icon = icon("plus-circle"),
                      style = "gradient",
                      color = "success",
                      size = "sm",
                      block = TRUE
                    ),
                    hr(),
                    withSpinner(
                      ui_element = DTOutput(
                        outputId = "excluded_table"
                      )
                    )
                  )
                )
              )
            )
          ),
          # --- Panel 2 / 4 - Data Exploration & Exclusion - Histogram ---
          tabPanel(
            title = "Histogram",
            value = "histogram_tab",
            icon = icon("chart-simple"),
            div(
              class = "dashboard-card",
              div(
                class = "card-header",
                icon("chart-simple", class = "header-icon"),
                h3("Histogram for Your Data")
              ),
              div(
                class = "card-body",
                div(
                  class = "parameter-section",
                  fluidRow(
                    column(
                      width = 6,
                      h5("Apply Logarithmic Transformation on Data"),
                      radioGroupButtons(
                        inputId = "log_hist",
                        label = NULL,
                        choiceNames = c("Yes", "No"),
                        choiceValues = c("Yes", "No"),
                        selected = "No",
                        status = "primary",
                        justified = TRUE
                      )
                    ),
                    column(
                      width = 6,
                      h5("Apply Bootstrap for Confidence Intervals"),
                      radioGroupButtons(
                        inputId = "bootstrap",
                        label = NULL,
                        choiceNames = c("Yes", "No"),
                        choiceValues = c("Yes", "No"),
                        selected = "No",
                        status = "primary",
                        justified = TRUE
                      )
                    )
                  )
                ),
                plotOutput(
                  outputId = "hist",
                  width = "100%",
                  height = "400px"
                )
              )
            )
          ),
          # --- Panel 3 / 4 - Data Exploration & Exclusion - Data Plots ---
          tabPanel(
            title = "Data Plots",
            value = "data_plots",
            icon = icon("chart-bar"),
            div(
              class = "dashboard-card",
              div(
                class = "card-header",
                icon("chart-bar", class = "header-icon"),
                h3("Data Plots")
              ),
              div(
                class = "card-body",
                div(
                  class = "parameter-section",
                  h5("Select Type of Plot"),
                  radioGroupButtons(
                    inputId = "selected_data_plot",
                    label = NULL,
                    choiceNames = c(
                      "Subject Range",
                      "Subject Over Time",
                      "Sample Variation",
                      "Replication Variation"
                    ),
                    choiceValues = c(
                      "sr",
                      "sot",
                      "sv",
                      "rv"
                    ),
                    selected = "sot",
                    status = "primary",
                    justified = TRUE
                  ),
                  conditionalPanel(
                    condition = "input.selected_data_plot == 'sr'",
                    h5("Normalize Results"),
                    radioGroupButtons(
                      inputId = "sr_normalize",
                      label = NULL,
                      choices = c("No", "Yes"),
                      selected = "No",
                      status = "primary",
                      size = "sm",
                      justified = TRUE
                    )
                  ),
                  conditionalPanel(
                    condition = "input.selected_data_plot == 'sot'",
                    h5("Normalize Results"),
                    radioGroupButtons(
                      inputId = "sot_normalize",
                      label = NULL,
                      choices = c("No", "Yes"),
                      selected = "No",
                      status = "primary",
                      size = "sm",
                      justified = TRUE
                    )
                  ),
                  conditionalPanel(
                    condition = "input.selected_data_plot == 'sv'",
                    h5("Select Dispersion Measure"),
                    radioGroupButtons(
                      inputId = "sv_dispersion",
                      label = NULL,
                      choiceNames = c("SD", "CV", "MAD", "Ratio", "Range"),
                      choiceValues = c("sd", "cv", "mad", "ratio", "range"),
                      selected = "sd",
                      status = "primary",
                      size = "sm",
                      justified = TRUE
                    )
                  )
                ),
                withSpinner(
                  ui_element = plotOutput(
                    outputId = "output_data_plot",
                    width = "100%",
                    height = "800px"
                  )
                )
              )
            )
          ),
          # --- Panel 4 / 4 - Data Exploration & Exclusion - Prior Plots ---
          tabPanel(
            title = "Prior Density Plots",
            value = "prior_plots_tab",
            icon = icon("chart-area"),
            div(
              class = "dashboard-card",
              div(
                class = "card-header",
                icon("chart-area", class = "header-icon"),
                h3("Prior Density Plots")
              ),
              div(
                class = "card-body",
                div(
                  class = "parameter-section",
                  h5("Apply Logarithmic Transformation on Data"),
                  radioGroupButtons(
                    inputId = "log_priors",
                    label = NULL,
                    choiceNames = c("Yes", "No"),
                    choiceValues = c("Yes", "No"),
                    selected = "No",
                    status = "primary",
                    justified = TRUE
                  ),
                  h5("Apply the NTTDFGAM Model"),
                  radioGroupButtons(
                    inputId = "nttdfgam_priors",
                    label = NULL,
                    choiceNames = c("Yes", "No"),
                    choiceValues = c("Yes", "No"),
                    selected = "No",
                    status = "primary",
                    justified = TRUE
                  )
                ),
                plotOutput(
                  outputId = "prior_densities",
                  width = "100%",
                  height = "800px"
                )
              )
            )
          )
        )
      ),
      # --- Application Documentation Module ---
      tabItem(
        tabName = "documentation",
        fluidRow(
          box(
            title = tagList(
              icon("book-open"),
              "Application Documentation"
            ),
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = FALSE,
            HTML('
              <div class="documentation-container" style="padding: 10px; font-size: 16px; line-height: 1.6;">
                
                <h2>1. Introduction</h2>
                <hr>
                <p>This document provides a comprehensive guide to the <strong>Biological Variation application</strong>, a tool designed for performing sophisticated Bayesian analysis on biological variation data. The application allows users to upload their data, configure model parameters, and run one of two advanced Bayesian hierarchical models to estimate components of biological variation.</p>
                <p>The application is built with a user-friendly "wizard" interface for setup and provides detailed, interactive outputs including summary tables and plots.</p>
                
                <h3>Key Features:</h3>
                <ul>
                  <li><strong>Intuitive Wizard:</strong> A four-step process guides the user from data upload to model configuration.</li>
                  <li><strong>Flexible Data Input:</strong> Supports both <code>.csv</code> and <code>.xlsx</code> file formats, with automatic sheet detection for Excel files.</li>
                  <li><strong>AI Column Mapping:</strong> Includes a "Guess" feature that intelligently identifies data columns based on common naming conventions (in English and Norwegian).</li>
                  <li><strong>Two Bayesian Models:</strong>
                      <ol>
                          <li><strong>Model 1 (NTT):</strong> A Normal-Transformed t-distribution model.</li>
                          <li><strong>Model 2 (NTTDFGAM):</strong> An extension of the NTT model that incorporates a Gamma distribution for the degrees of freedom parameters.</li>
                      </ol>
                  </li>
                  <li><strong>Customizable Analysis:</strong> Users can set hyperparameters for the prior distributions and configure MCMC sampling parameters.</li>
                  <li><strong>Interactive Outputs:</strong> Results are presented in sortable, filterable tables and high-quality plots.</li>
                  <li><strong>Easy Export:</strong> All results, including tables (<code>.xlsx</code>) and plots (<code>.tif</code>), can be easily downloaded.</li>
                </ul>
                <br>
                
                <h2>2. Prerequisites</h2>
                <hr>
                <p>Before launching the application, ensure the following files are present in the application\'s root directory:</p>
                <ol>
                  <li><code>stan_model_1_compiled.rds</code>: The pre-compiled Stan model file for the NTT model.</li>
                  <li><code>stan_model_2_compiled.rds</code>: The pre-compiled Stan model file for the NTTDFGAM model.</li>
                </ol>
                <p>The application will check for these files upon startup and will not run without them. If they are missing, you may need to run a pre-compilation script.</p>
                <p>Before uploading a dataset, ensure it is formatted in a way that is acceptable: </p>
                <ol>
                  <li> <strong> Long format </strong>: Each row in the uploaded dataset should correspond to exactly one measurement. This measurement should correspond with a particular grouping. </li>
                  <li> <strong> Mandatory Identifier Columns </strong>: The uploaded dataset <strong> must include </strong> subject, sample and replicate identifier columns. So the minimal requirement for number of rows in an uploaded dataset is four (mandatory identifier columns and the column that holds the measurement results). </li>
                  <li> <strong> Unique Subject Identifiers Column </strong>: The subject identifiers should represent a particular subject. Never use the same subject identifier for two or more unique subjects. For example, if you have two groups in your dataset, you cannot assign equal subject identifiers in both groups. In other words, you cannot assign the subject identifier <strong> 9 </strong> to both Jack Smith and Jane Russels, even if Jane have diabetes and Jack is healthy </li>
                </ol>
                <br>
                
                <h2>3. User Guide: Step-by-Step Workflow</h2>
                <hr>
                <p>The application is organized into sections accessible from the sidebar. The main workflow is managed in the <strong>Setup & Data Upload</strong> tab.</p>
                
                <h3>3.1. Analysis Setup (The Wizard)</h3>
                
                <h4>Step 1: Upload Data</h4>
                <ul>
                  <li><strong>Upload Biological Variation Data:</strong> Click "Browse..." to select your data file (<code>.csv</code> or <code>.xlsx</code>).</li>
                  <li><strong>Select Sheet:</strong> If you upload an <code>.xlsx</code> file with multiple sheets, buttons will appear allowing you to select the correct sheet for analysis.</li>
                </ul>
                
                <h4>Step 2: Select Columns</h4>
                <p>This step is crucial for telling the model what each column in your data represents.</p>
                <ul>
                  <li><strong>Mandatory Selections:</strong> You <strong>must</strong> map the Measurement, Subject ID, Sample ID, and Replicate ID columns.</li>
                  <li><strong>Group Selections:</strong> These are optional but highly recommended for stratifying and filtering your data (e.g., by Analyte, Material, Sex, etc.).</li>
                  <li><strong>Helper Buttons:</strong> Use <strong>Guess</strong> to auto-map columns or <strong>Reset</strong> to clear selections.</li>
                </ul>
                
                <h4>Step 3: Filter Data & Define Names</h4>
                <p>Here, you select the specific subset of your data to analyze and give it descriptive names for the output.</p>
                <ul>
                  <li><strong>Filter Data (Left Panel):</strong> Use the dropdowns to select the specific analyte, material, sex, or group you wish to include in the analysis.</li>
                  <li><strong>Define Names (Right Panel):</strong> Enter names to label the results (e.g., Analyte Name: "Glucose"). These are often auto-populated.</li>
                </ul>
                
                <h4>Step 4: Set Hyperparameters & Advanced Options</h4>
                <p>This final step configures the Bayesian model\'s parameters.</p>
                <ul>
                  <li><strong>Set Hyperparameters:</strong> These settings define the "prior" distributions for the model\'s main parameters (e.g., \\(\\beta\\), \\(\\mathrm{CV}_{I}(\\%)\\)).
                    <ul>
                      <li><strong>Mean:</strong> The expected value of the prior distribution, based on existing knowledge.</li>
                      <li><strong>SD Factor:</strong> A multiplier that controls the standard deviation (and thus the "weakness") of the prior. A higher factor means the data will have more influence.</li>
                    </ul>
                  </li>
                  <li><strong>Advanced Modelling Options:</strong> This collapsible box contains MCMC settings for the Stan sampler, such as log transformation, number of iterations, burn-in, chains, and CPU cores.</li>
                </ul>
                <br>
                
                <h2>4. Running an Analysis and Viewing Results</h2>
                <hr>
                <p>Navigate to either the <strong>Model 1 (NTT)</strong> or <strong>Model 2 (NTTDFGAM)</strong> tab in the sidebar.</p>
                <ol>
                  <li><strong>Run Analysis:</strong> Click the large <strong>Run ... Model</strong> button.
                  A progress bar will appear with an estimated time to completion.
                  Note that this completion time estimate can be quite unstable, so do not trust it blindly.
                  It is there only to give a rough time estimate. </li>
                  <li><strong>View Results:</strong> Once the analysis is complete, the results will appear in five sub-tabs:
                    <ul>
                      <li><strong>Summary Statistics:</strong> An interactive table providing high-level posterior summaries of the most 
                      important model parameters. Summaries are given by posterior means and \\(95\\%\\) credible
                      intervals for \\(\\beta\\), \\(\\mathrm{E}[\\mathrm{CV}_i]\\) (\\(\\mathrm{CV}_{\\mathrm{I}}\\)), 
                      \\(\\mathrm{CV}_G\\), and \\(\\mathrm{CV}_A\\). Summaries are also given by the posterior median 
                      and a \\(60\\%\\) prediction interval for \\(\\mathrm{CV}_i\\). The remaining parts of
                      the summary statistics include three different estimations of the Harris-Brown Heterogeneity ratio (HBHR). </li>
                      <li><strong>Subject-Specific CV Plot:</strong> A plot displaying the estimated within-subject Coefficient
                      of Variation (\\(\\mathrm{CV}_{p(i)}\\)) for each subject. The results are ordered by the
                      posterior medians of \\(\\mathrm{CV}_{p(i)}\\). </li>
                      <li><strong>CV Versus Concentration Plot:</strong> A diagnostic plot showing each subject\'s average
                      concentration (homeostatic set points, \\(\\mu_{p(i)}\\)) plotted against their \\(\\mathrm{CV}_{p(i)}\\).</li>
                      <li><strong>CV Versus Reference Change Value Plot:</strong> A plot demonstrating the Reference Change
                      Value(s) (RCV, \\(\\mathrm{RCV}(\\%)\\)) as a function of \\(\\mathrm{CV}_{p(i)}\\).
                      Two purple horizontal lines display the RCVs for \\(\\mathrm{E}[\\mathrm{CV}_i]\\).
                      Additionally, the credible intervals of \\(\\mathrm{CV}_{p(i)}\\) are incorporated into the
                      pointwise RCV (shown as green ribbons). </li>
                      <li><strong>Advanced:</strong> This tab includes more plots and tools for sampling diagnostics.
                      Here you can find posterior distribution plots, trace plots, and a table summarizing mixing
                      measures and effective sample sizes for most model parameters. </li>
                    </ul>
                  </li>
                  <li><strong>Download Results:</strong> Click the <strong>Download</strong> button.
                  The file will be an <code>.xlsx</code> for tables or a <code>.tif</code> for plots.
                  Note that posterior distribution plots and trace plots cannot be downloaded in the
                  current version of the application. </li>
                </ol>
                <br>
                
                <h2>5. Statistical and Mathematical Details</h2>
                <hr>
                <p>This section provides an overview of the statistical models used in the application, which
                differ depending on whether a log transformation is applied.</p>
                
                <h3>The Hierarchical Model Structure</h3>
                <p>The application uses a Bayesian hierarchical model (3-level nested model) to partition variance
                into its biological and analytical components.</p>
                
                <h4>Case 1: Additive Model (Original Scale)</h4>
                <p>When "Apply Log Transformation" is set to "No", the model assumes an additive relationship:</p>
                $$ y_{isr} = \\beta + G_i + I_{is} + A_{isr} $$
                <ul>
                  <li><code>&beta;</code> is the overall population mean (fixed effect).</li>
                  <li><code>G<sub>i</sub></code> \\(\\sim \\mathrm{N}(0, \\sigma_G^2)\\) represents the <strong>between-subject</strong> biological variation.</li>
                  <li><code>I<sub>is</sub></code> \\(\\sim \\mathrm{lst}(0, \\sigma_i^2, \\mathrm{df}_I)\\) represents the <strong>within-subject</strong> biological variation.</li>
                  <li><code>A<sub>isr</sub></code> \\(\\sim \\mathrm{lst}(0, \\sigma_A^2, \\mathrm{df}_A)\\) represents the <strong>analytical</strong> variation.</li>
                </ul>
                <p>We set the following priors for \\(\\beta, \\sigma_G, \\sigma_i, \\sigma_A, \\mathrm{df}_{I} \\text{ and } \\mathrm{df}_{A}\\):</p>
                <ul>
                  <li><code>&beta;</code> \\(\\sim \\mathrm{N}(\\color{purple}{\\mathrm{E}[\\beta]}, \\mathrm{Var}[\\beta])\\), with \\(\\mathrm{Var}[\\beta] = (\\color{purple}{F_{\\beta}} \\cdot \\color{purple}{\\mathrm{E}[\\beta]})^2 \\)
                  <li><code>&sigma;<sub>G</sub></code> \\(\\sim \\mathrm{N}(\\color{purple}{\\mathrm{E}[\\sigma_G]}, \\mathrm{Var}[\\sigma_G])\\), with \\(\\mathrm{Var}[\\sigma_G] = (\\color{purple}{F_G} \\cdot \\color{purple}{\\mathrm{E}[\\sigma_G]})^2 \\).</li>
                  <li><code>&sigma;<sub>i</sub></code> \\(\\sim \\mathrm{N}(\\mathrm{E}[\\sigma_i], \\mathrm{Var}[\\sigma_i])\\) for \\(i = 1, \\ldots, n\\) where:
                  <ul>
                    <li><code>E[&sigma;<sub>i</sub>]</code> \\(\\sim \\mathrm{N}(\\color{purple}{\\color{purple}{\\sigma_I}}, \\mathrm{Var}[\\mathrm{E}[\\sigma_i]])\\), with \\(\\mathrm{Var}[\\mathrm{E}[\\sigma_i]] = (\\color{purple}{F_I} \\cdot \\color{purple}{\\sigma_I})^2 \\).</li>
                    <li><code>SD[&sigma;<sub>i</sub>]</code> \\(\\sim \\mathrm{N}(\\mathrm{E}[\\mathrm{SD}[\\sigma_i]], \\mathrm{Var}[\\mathrm{SD}[\\sigma_i]])\\), with \\(\\mathrm{E}[\\mathrm{SD}[\\sigma_i]] = \\mathrm{HBHR} \\cdot \\color{purple}{\\sigma_I} \\) and \\(\\mathrm{Var}[\\mathrm{SD}[\\sigma_i]] = (F_{\\mathrm{HBHR}} \\cdot \\color{purple}{\\sigma_I})^2 \\). </li>
                  </ul>
                  <li><code>&sigma;<sub>A</sub></code> \\(\\sim \\mathrm{N}(\\color{purple}{\\mathrm{E}[\\sigma_A]}, \\mathrm{Var}[\\sigma_A])\\), with \\(\\mathrm{Var}[\\sigma_A] = (\\color{purple}{F_A} \\cdot \\color{purple}{\\mathrm{E}[\\sigma_A]})^2 \\).</li>
                  <li><code>df<sub>I</sub></code> \\(\\sim \\mathrm{N}(\\color{purple}{\\mathrm{E}[\\mathrm{df}_I]}, \\mathrm{Var}[\\mathrm{df}_I])\\), with \\(\\mathrm{Var}[\\mathrm{df}_I] = (\\color{purple}{F_{\\mathrm{df}_I}} \\cdot \\color{purple}{\\mathrm{E}[\\mathrm{df}_I]})^2 \\).</li>
                  <li><code>df<sub>A</sub></code> \\(\\sim \\mathrm{N}(\\color{purple}{\\mathrm{E}[\\mathrm{df}_A]}, \\mathrm{Var}[\\mathrm{df}_A])\\), with \\(\\mathrm{Var}[\\mathrm{df}_A] = (\\color{purple}{F_{\\mathrm{df}_A}} \\cdot \\color{purple}{\\mathrm{E}[\\mathrm{df}_A]})^2 \\).</li>
                </ul>
                <p> Here, \\(\\color{purple}{\\sigma_I} = \\mathrm{E}[\\mathrm{E}[\\sigma_i]]\\). For "Model 2 (NTTDFGAM)", the priors for \\(\\mathrm{df}_I\\) and \\(\\mathrm{df}_A\\) are instead: </p>
                <ul>
                  <li><code>df<sub>I</sub></code> \\(\\sim 2 + \\mathrm{Gamma}(\\alpha_{\\mathrm{df}_I}, \\lambda_{\\mathrm{df}_I})\\).</li>
                  <li><code>df<sub>A</sub></code> \\(\\sim 2 + \\mathrm{Gamma}(\\alpha_{\\mathrm{df}_A}, \\lambda_{\\mathrm{df}_A})\\).</li>  
                </ul>
                <p> The parameters of the prior gamma distributions are calculated using \\(\\mathrm{E}[\\mathrm{df}_{\\circ}], \\mathrm{Var}[\\mathrm{df}_{\\circ}] = (F_{\\mathrm{df}_{\\circ}} \\cdot \\mathrm{E}[\\mathrm{df}_{\\circ}])^2\\):</p>
                $$ \\alpha_{\\mathrm{df}_{\\circ}} = \\frac{\\mathrm{E}[\\mathrm{df}_{\\circ}]^2}{\\mathrm{Var}[\\mathrm{df}_{\\circ}]} = \\frac{1}{F_{\\mathrm{df}_{\\circ}}^2} \\quad \\lambda_{\\mathrm{df}_{\\circ}} = \\frac{\\mathrm{E}[\\mathrm{df}_{\\circ}]}{\\mathrm{Var}[\\mathrm{df}_{\\circ}]} = \\frac{1}{F_{\\mathrm{df}_{\\circ}}^2 \\cdot \\mathrm{E}[\\mathrm{df}_{\\circ}]} $$
                <h4>Case 2: Multiplicative Model (Log Scale)</h4>
                <p>When "Apply Log Transformation" is set to "Yes", the model assumes a multiplicative relationship, which becomes additive on the log scale:</p>
                $$ y_{isr} = \\beta \\cdot G_i \\cdot I_{is} \\cdot A_{isr} $$
                $$ \\log(y_{isr}) = \\log(\\beta) + \\log(G_i) + \\log(I_{is}) + \\log(A_{isr}) $$
                <p>The model structure remains the same, but it now operates on the logarithms of the measurements. The parameters \\(\\sigma_G, \\sigma_i, \\sigma_A\\) now represent the standard deviations of the variation components on the log scale.</p>
                <br>
                
                <h3>From User Priors to Effective Priors</h3>
                <p>The Bayesian models are parameterized with standard deviations (\\(\\sigma\\)). However, you
                (the user) are required to parameterize the models using CVs. Before the MCMC sampling starts, 
                the user priors are converted to the priors actually used by the model. We call the 
                priors actually used by the model Effective Priors. How the user priors are transformed to 
                effective priors depend on whether the additive or multiplicative model is assumed.<p/>
                
                <h4> Case 1: Additive Model (Identity Scale) </h4>
                
                $$\\mathrm{E}[\\sigma_i] = \\frac{\\mathrm{E}[\\mathrm{CV}_i(\\%)] \\cdot \\beta}{100\\%} \\cdot \\sqrt{\\frac{\\mathrm{df}_I - 2}{\\mathrm{df}_I}} \\quad
                \\mathrm{SD}[\\sigma_i] = \\frac{\\mathrm{SD}[\\mathrm{CV}_i(\\%)] \\cdot \\beta}{100\\%} \\cdot \\sqrt{\\frac{\\mathrm{df}_I - 2}{\\mathrm{df}_I}}$$
                
                $$\\sigma_A = \\frac{\\mathrm{CV}_A (\\%) \\cdot \\beta}{100\\%} \\cdot \\sqrt{\\frac{\\mathrm{df}_A - 2}{\\mathrm{df}_A}} \\quad
                \\sigma_G = \\frac{\\mathrm{CV}_G (\\%) \\cdot \\beta}{100\\%} $$
                
                <h4> Case 2: Multiplicative Model (Log Scale) </h4>
                
                $$\\mathrm{E}[\\sigma_i] = \\sqrt{\\log\\Big(\\Big[\\frac{\\mathrm{E}[\\mathrm{CV}_i(\\%)]}{100\\%}\\Big]^2 + 1\\Big)\\cdot\\Big(\\frac{\\mathrm{df}_I - 2}{\\mathrm{df}_I}\\Big)}$$
                $$\\mathrm{SD}[\\sigma_i] = \\sqrt{\\log\\Big(\\Big[\\frac{\\mathrm{SD}[\\mathrm{CV}_i(\\%)]}{100\\%}\\Big]^2 + 1\\Big)\\cdot\\Big(\\frac{\\mathrm{df}_I - 2}{\\mathrm{df}_I}\\Big)}$$
                $$\\sigma_A = \\sqrt{\\log\\Big(\\Big[\\frac{\\mathrm{CV}_A(\\%)}{100\\%}\\Big]^2 + 1\\Big)\\cdot\\Big(\\frac{\\mathrm{df}_A - 2}{\\mathrm{df}_A}\\Big)}$$
                $$\\sigma_G = \\sqrt{\\log\\Big(\\Big[\\frac{\\mathrm{CV}_A(\\%)}{100\\%}\\Big]^2 + 1\\Big)} \\quad
                \\beta \\to \\log(\\beta)$$
                
                <h3>From Effective Posteriors to User Posteriors</h3>
                <p> Since the Bayesian models are parameterized with standard deviations, the posterior and 
                predictive distributions are also parameterized in this way. The user is typically not interested 
                in viewing the results in this way, so the parameters parameterized with standard deviations 
                must be back-transformed into user posteriors. How the effective posteriors are transformed back
                to the user scale depends on whether the additive or multiplicative model was assumed. </p>
                
                <h4> Case 1: Additive Model (Identity Scale) </h4>
                
                $$\\mathrm{E}[\\mathrm{CV}_{i}(\\%)] = \\frac{\\mathrm{E}[\\sigma_i]}{\\beta} \\cdot \\sqrt{\\frac{\\mathrm{df}_I}{\\mathrm{df}_I - 2}} \\cdot 100\\% \\quad
                \\mathrm{SD}[\\mathrm{CV}_{i}(\\%)] = \\frac{\\mathrm{SD}[\\sigma_i]}{\\beta} \\cdot \\sqrt{\\frac{\\mathrm{df}_I}{\\mathrm{df}_I - 2}} \\cdot 100\\%$$
                
                $$\\mathrm{CV}_{p(i)}(\\%) = \\frac{\\sigma_{p(i)}}{\\mu_{p(i)}} \\cdot \\sqrt{\\frac{\\mathrm{df}_I}{\\mathrm{df}_I - 2}} \\cdot 100\\%\\quad 
                \\mathrm{CV}_{i, \\mathrm{pred}}(\\%) = \\frac{\\sigma_{i, \\mathrm{pred}}}{G_{i, \\mathrm{pred}} + \\beta_{i, \\mathrm{pred}}} \\cdot 100\\%$$
                
                $$\\mathrm{CV}_{A}(\\%) = \\frac{\\sigma_A}{\\beta} \\cdot \\sqrt{\\frac{\\mathrm{df}_A}{\\mathrm{df}_A - 2}} \\cdot 100\\% \\quad
                \\mathrm{CV}_{G}(\\%) = \\frac{\\sigma_G}{\\beta} \\cdot 100\\%$$
                
                <h4> Case 2: Multiplicative Model (Log Scale) </h4>
                
                
                $$\\mathrm{E}[\\mathrm{CV}_{i}(\\%)] = \\sqrt{\\exp\\Big[\\mathrm{E}[\\sigma_i]^2 \\cdot \\frac{\\mathrm{df}_I}{\\mathrm{df}_I - 2} \\Big] - 1} \\cdot 100\\% \\quad
                \\mathrm{SD}[\\mathrm{CV}_{i}(\\%)] = \\sqrt{\\exp\\Big[\\mathrm{SD}[\\sigma_i]^2 \\cdot \\frac{\\mathrm{df}_I}{\\mathrm{df}_I - 2} \\Big] - 1} \\cdot 100\\%$$
                
                $$\\mathrm{CV}_{p(i)}(\\%) = \\sqrt{\\exp\\Big[\\sigma_{p(i)}^2 \\cdot \\frac{\\mathrm{df}_I}{\\mathrm{df}_I - 2} \\Big] - 1} \\cdot 100\\% \\quad 
                \\mathrm{CV}_{i, \\mathrm{pred}}(\\%) = \\sqrt{\\exp\\Big[\\sigma_{i, \\mathrm{pred}}^2 \\Big] - 1} \\cdot 100\\%$$
                
                $$\\mathrm{CV}_{A}(\\%) = \\sqrt{\\exp\\Big[\\sigma_{A}^2 \\cdot \\frac{\\mathrm{df}_A}{\\mathrm{df}_A - 2} \\Big] - 1} \\cdot 100\\% \\quad
                \\mathrm{CV}_{G}(\\%) = \\sqrt{\\exp\\Big[\\sigma_{G}^2\\Big] - 1} \\cdot 100\\% \\quad
                \\beta \\to \\exp(\\beta)$$
                
              </div>
            ')
          )
        )
      ),
      # --- Debugging Module ---
      tabItem(
        tabName = "debugging",
        h2("Live Input Monitor"),
        verbatimTextOutput("debug_console")
      )
    )
  )
)


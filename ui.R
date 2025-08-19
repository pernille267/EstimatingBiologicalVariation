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
          size = "sm"
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
          size = "sm"
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
      menuItem("Documentation", tabName = "documentation", icon = icon("book"))
    )
  ),
  
  # 3. BODY
  # --------------------------------------------------------------------------
  dashboardBody(
    withMathJax(),
    includeCSS("www/styles.css"),
    tabItems(
      # -- Tab 1: Setup and Inputs --
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
        tabsetPanel(
          id = "setup_wizard",
          type = "hidden",
          
          # Upload Data
          tabPanelBody(
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
                  create_wizard_nav_buttons(next_id = "goto_step2")
                )
              )
            )
          ),
          
          # Map columns
          tabPanelBody(
            value = "step2_map",
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
                ),
                create_wizard_nav_buttons(prev_id = "back_to_step1", next_id = "goto_step3")
              )
            )
          ),
          tabPanelBody(
            value = "step3_filter",
            div(
              class = "dashboard-card-drop-down",
              div(
                class = "card-header",
                icon("filter", class = "header-icon"), 
                h3("Filter Data & Define Names")
              ),
              div(
                class = "card-body",
                fluidRow(
                  column(
                    width = 6,
                    div(
                      class = "parameter-section",
                      uiOutput("analyte_filter_ui"),
                      uiOutput("material_filter_ui"),
                      uiOutput("sex_filter_ui"),
                      uiOutput("group_1_filter_ui"),
                      uiOutput("group_2_filter_ui")
                    )
                  ),
                  column(
                    width = 6,
                    div(
                      class = "parameter-section",
                      textInput("analyte_name", "Analyte Name", placeholder = "e.g., Glucose"),
                      textInput("analyte_material", "Material Name", placeholder = "e.g., Plasma"),
                      textInput("sex_name", "Sex Name", placeholder = "e.g., Females"),
                      textInput("group_name", "Group Name", placeholder = "e.g., Healthy")
                    )
                  )
                ),
                create_wizard_nav_buttons(prev_id = "back_to_step2", next_id = "goto_step4")
              )
            )
          ),
          tabPanelBody(
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
                    ),
                    create_wizard_nav_buttons(prev_id = "back_to_step3")
                  )
                )
              ),
              column(
                width = 6,
                box(
                  title = tagList(icon("microchip"), "Advanced Modelling Options"),
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
                      choices = seq(from = 2, to = parallel::detectCores()),
                      selected = 4,
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
                      selected = 4,
                      grid = FALSE
                    )
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
            title = "CV Versus Concentration Plot",
            value = "cv_vs_conc_ntt",
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
                withSpinner(
                  plotOutput("filtered_data_table_model1")
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
            title = "CV Versus Concentration Plot",
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
                withSpinner(
                  plotOutput("filtered_data_table_model2", height = "600px")
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "documentation",
        fluidRow(
          box(
            title = tagList(icon("book-open"), "Application Documentation"),
            solidHeader = TRUE,
            status = "primary",
            width = 12,
            collapsible = FALSE,
            HTML('
              <div class="documentation-container" style="padding: 10px; font-size: 16px; line-height: 1.6;">
                
                <h2>1. Introduction</h2>
                <hr>
                <p>This document provides a comprehensive guide to the <strong>Biological Variation Shiny application</strong>, a tool designed for performing sophisticated Bayesian analysis on biological data. The application allows users to upload their data, configure model parameters, and run one of two advanced Bayesian hierarchical models to estimate components of biological variation.</p>
                <p>The application is built with a user-friendly "wizard" interface for setup and provides detailed, interactive outputs including summary tables and plots.</p>
                
                <h3>Key Features:</h3>
                <ul>
                  <li><strong>Intuitive Wizard:</strong> A four-step process guides the user from data upload to model configuration.</li>
                  <li><strong>Flexible Data Input:</strong> Supports both <code>.csv</code> and <code>.xlsx</code> file formats, with automatic sheet detection for Excel files.</li>
                  <li><strong>Smart Column Mapping:</strong> Includes a "Guess" feature that intelligently identifies data columns based on common naming conventions (in English and Norwegian).</li>
                  <li><strong>Two Bayesian Models:</strong>
                      <ol>
                          <li><strong>Model 1 (NTT):</strong> A Normal-Transformed t-distribution model.</li>
                          <li><strong>Model 2 (NTTDFGAM):</strong> An extension of the NTT model that incorporates a Gamma distribution for the degrees of freedom parameter.</li>
                      </ol>
                  </li>
                  <li><strong>Customizable Analysis:</strong> Users can set prior distributions (hyperparameters) and configure MCMC sampling parameters.</li>
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
                  <li><strong>Run Analysis:</strong> Click the large <strong>Run ... Model</strong> button. A progress bar will appear with an estimated time to completion.</li>
                  <li><strong>View Results:</strong> Once complete, results appear in three sub-tabs:
                    <ul>
                      <li><strong>Summary Statistics:</strong> An interactive table with posterior summaries for all model parameters.</li>
                      <li><strong>Subject-Specific CV Plot:</strong> A plot displaying the estimated within-subject coefficient of variation (\\(\\mathrm{CV}_{I}\\)) for each subject.</li>
                      <li><strong>CV Versus Concentration Plot:</strong> A diagnostic plot showing each subject\'s average concentration against their estimated \\(\\mathrm{CV}_{I}\\).</li>
                    </ul>
                  </li>
                  <li><strong>Download Results:</strong> Click the <strong>Download</strong> button. The file will be an <code>.xlsx</code> for tables or a <code>.tif</code> for plots.</li>
                </ol>
                <br>
                
                <h2>5. Statistical and Mathematical Details</h2>
                <hr>
                <p>This section provides an overview of the statistical models used in the application, which differ depending on whether a log transformation is applied.</p>
                
                <h3>The Hierarchical Model Structure</h3>
                <p>The application uses a Bayesian hierarchical model to partition variance into its biological and analytical components.</p>
                
                <h4>Case 1: Additive Model (Original Scale)</h4>
                <p>When "Apply Log Transformation" is set to "No", the model assumes an additive relationship:</p>
                $$ y_{isr} = \\beta + G_i + I_{is} + A_{isr} $$
                <ul>
                  <li><code>&beta;</code> is the overall population mean.</li>
                  <li><code>G<sub>i</sub></code> \\(\\sim \\mathrm{N}(0, \\sigma_G^2)\\) represents the <strong>between-subject</strong> variation.</li>
                  <li><code>I<sub>is</sub></code> \\(\\sim \\mathrm{lst}(0, \\sigma_i^2, \\mathrm{df}_I)\\) represents the <strong>within-subject</strong> variation.</li>
                  <li><code>A<sub>isr</sub></code> \\(\\sim \\mathrm{lst}(0, \\sigma_A^2, \\mathrm{df}_A)\\) represents the <strong>analytical</strong> variation.</li>
                </ul>
                <p>We set the following priors for \\(\\beta, \\sigma_G, \\sigma_i, \\sigma_A, \\mathrm{df}_{I} \\text{ and } \\mathrm{df}_{A}\\):</p>
                <ul>
                  <li><code>&beta;</code> \\(\\sim \\mathrm{N}(\\color{purple}{\\mathrm{E}[\\beta]}, \\mathrm{Var}[\\beta])\\), with \\(\\mathrm{Var}[\\beta] = (\\color{purple}{F_{\\beta}} \\cdot \\color{purple}{\\mathrm{E}[\\beta]})^2 \\)
                  <li><code>&sigma;<sub>G</sub></code> \\(\\sim \\mathrm{N}(\\color{purple}{\\mathrm{E}[\\sigma_G]}, \\mathrm{Var}[\\sigma_G])\\), with \\(\\mathrm{Var}[\\sigma_G] = (\\color{purple}{F_G} \\cdot \\color{purple}{\\mathrm{E}[\\sigma_G]})^2 \\).</li>
                  <li><code>&sigma;<sub>i</sub></code> \\(\\sim \\mathrm{N}(\\mathrm{E}[\\sigma_i], \\mathrm{Var}[\\sigma_i])\\) for \\(i = 1, \\ldots, n\\) where:
                  <ul>
                    <li><code>E[&sigma;<sub>i</sub>]</code> \\(\\sim \\mathrm{N}(\\color{purple}{\\color{purple}{\\sigma_I}}, \\mathrm{Var}[\\mathrm{E}[\\sigma_i]])\\), with \\(\\mathrm{Var}[\\mathrm{E}[\\sigma_i]] = (\\color{purple}{F_I} \\cdot \\color{purple}{\\sigma_I})^2 \\).</li>
                    <li><code>SD[&sigma;<sub>i</sub>]</code> \\(\\sim \\mathrm{N}(\\mathrm{E}[\\mathrm{SD}[\\sigma_i]], \\mathrm{Var}[\\mathrm{SD}[\\sigma_i]])\\), with \\(\\mathrm{E}[\\mathrm{SD}[\\sigma_i]] = 0.5 \\cdot \\color{purple}{\\sigma_I} \\) and \\(\\mathrm{Var}[\\mathrm{SD}[\\sigma_i]] = 4 \\cdot \\color{purple}{\\sigma_I}^2 \\). </li>
                  </ul>
                  <li><code>&sigma;<sub>A</sub></code> \\(\\sim \\mathrm{N}(\\color{purple}{\\mathrm{E}[\\sigma_A]}, \\mathrm{Var}[\\sigma_A])\\), with \\(\\mathrm{Var}[\\sigma_A] = (\\color{purple}{F_A} \\cdot \\color{purple}{\\mathrm{E}[\\sigma_A]})^2 \\).</li>
                  <li><code>df<sub>I</sub></code> \\(\\sim \\mathrm{N}(\\mathrm{E}[\\mathrm{df}_I], \\mathrm{Var}[\\mathrm{df}_I])\\), with \\(\\mathrm{Var}[\\mathrm{df}_I] = (F_{\\mathrm{df}_I} \\cdot \\mathrm{E}[\\mathrm{df}_I])^2 \\).</li>
                  <li><code>df<sub>A</sub></code> \\(\\sim \\mathrm{N}(\\mathrm{E}[\\mathrm{df}_A], \\mathrm{Var}[\\mathrm{df}_A])\\), with \\(\\mathrm{Var}[\\mathrm{df}_A] = (F_{\\mathrm{df}_A} \\cdot \\mathrm{E}[\\mathrm{df}_A])^2 \\).</li>
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
                <p>The model structure remains the same, but it now operates on the logarithms of the measurements. The parameters $ \\sigma_G, \\sigma_i, \\sigma_A $ now represent the standard deviations of the variation components on the log scale.</p>
                <br>
                
                <h3>Coefficients of Variation (CV) Calculation</h3>
                <p>While the models are parameterized with standard deviations ($ \\sigma $), the results are presented as Coefficients of Variation (CV). The conversion formula is different for each model.</p>
                
                <h4>On the Original (Identity) Scale</h4>
                <p>The population-level CVs are calculated relative to the overall population mean (\\(\\beta \\)). For the Normally-distributed component (\\(\\mathrm{CV}_G\\)):</p>
                $$ \\text{CV}_G(\\%) = \\frac{\\sigma_G}{\\beta} \\times 100\\% $$
                <p>For the t-distributed components (\\(\\mathrm{CV}_I\\), \\(\\mathrm{CV}_A\\)), the standard deviation must first be derived from the scale parameter (\\(s\\)) and degrees of freedom \\(\\nu\\):</p>
                $$ \\sigma = s \\cdot \\sqrt{\\frac{\\nu}{\\nu - 2}} \\quad \\implies \\quad \\text{CV}(\\%) = \\frac{\\sigma}{\\beta} \\times 100\\% $$
                <p><strong>Priors:</strong> When you provide a prior for a CV on this scale, the application uses the prior mean for \\(\\beta\\) to convert it back to a prior for the standard deviation \\(\\sigma\\).</p>
                <p><strong>Subject-Specific CVs:</strong> Importantly, the within-subject CV for each individual subject ($CV_i$) is calculated relative to that subject\'s own mean (\\(\\beta + G_i\\)), not the overall population mean. This provides a personalized estimate of variation.</p>
                $$ \\text{CV}_i(\\%) = \\frac{\\sigma_i}{\\beta + G_i} \\times 100\\% $$

                <h4>On the Log-Transformed Scale</h4>
                <p>When data is log-transformed, the CV is derived from the standard deviation on the log scale ($ \\sigma_{\\log} $) and <strong>does not depend on the mean</strong>. For the log-normally distributed component ($CV_G$):</p>
                $$ \\text{CV}_G(\\%) = \\sqrt{e^{\\sigma_{\\log, G}^2} - 1} \\times 100\\% $$
                <p>For the log-t distributed components ($CV_I, CV_A$), the variance on the log scale is first calculated from the scale ($s_{\\log}$) and degrees of freedom ($ \\nu $), then used in the same formula:</p>
                $$ \\sigma_{\\log}^2 = s_{\\log}^2 \\cdot \\frac{\\nu}{\\nu - 2} \\quad \\implies \\quad \\text{CV}(\\%) = \\sqrt{e^{\\sigma_{\\log}^2} - 1} \\times 100\\% $$
                <p><strong>Priors:</strong> When you provide a prior for a CV on the log scale, the application uses the inverse formula \\(\\sigma_{\\log} = \\sqrt{\\ln((\\text{CV}/100)^2 + 1)}\\) to set the prior for the standard deviation on the log scale. This conversion does not involve \\(\\beta\\).</p>
                
              </div>
            ')
          )
        )
      )
    )
  )
)

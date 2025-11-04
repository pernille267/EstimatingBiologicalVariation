#
# This is the server logic of a Shiny web application.
# It now includes "Guess" and "Reset" functionality for column selection.
#

# LIBRARIES
# ------------------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(rstan)
library(data.table)
library(readxl)
library(biovar)
library(ggplot2)
library(DT)
library(writexl)

# --- Helper Constants ---

# define predifined hyperparameter priors for specific analytes (update and push request)
analyte_priors <- list(
  "HbA1c" = list(beta = 5, cvi = 1.2, cva = 2.5, cvg = 5.4),
  "Glucose" = list(beta = 5, cvi = 4.7, cva = 2.5, cvg = 8.0),
  "ALA" = list(beta = 2, cvi = 16.0, cva = 5.0, cvg = 27.0),
  "PBG" = list(beta = 1.8, cvi = 20.0, cva = 8.0, cvg = 30.0)
  # Add other analytes here as needed
)

# define analyte name synomym mapping (update and push request)
analyte_synonyms <- list(
  "HbA1c" = c(
    "hba1c", "a1c", "hemoglobin a1c", "glycated hemoglobin",
    "glycosylated hemoglobin", "glycohemoglobin", "langtidsblodsukker"
  ),
  "Glucose" = c(
    "glucose", "glukose", "glu", "blood sugar", 
    "blodsukker", "p-glucose", "s-glucose", "b-glucose", 
    "p-glukose", "s-glukose", "b-glukose", "fbs", "fbg"
  ),
  "ALA" = c(
    "ala", "aminolevulinic acid", "delta-aminolevulinic acid",
    "d-ala", "dala", "5-aminolevulinic acid", "aminolevulinsyre",
    "delta-aminolevulinsyre", "u-ala", "p-ala"
  ),
  c(
    "pbg", "porphobilinogen", "porfobilinogen", 
    "u-pbg"
  )
)

# SERVER LOGIC
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # 1. HELPER FUNCTIONS
  # --------------------------------------------------------------------------
  
  # Prepare stan modelling data
  pernille_prepares_for_stan_modeling <- function(data, hypers, hyper_strength, log_transformed = FALSE) {
    setDT(data)
    setorder(data, SubjectID, SampleID, ReplicateID)
    
    stan_data_prep <- process_stan_data_indexing(
      SubjectID_orig_R = data$SubjectID,
      SampleID_orig_R = data$SampleID,
      y_R = if(log_transformed) {log(data$y)} else {data$y}
    )
    
    stan_data_prep_priors <- process_stan_data_priors(
      beta = hypers$beta,
      cvi = hypers$cvi,
      cva = hypers$cva,
      cvg = hypers$cvg,
      dfi = hypers$dfi,
      dfa = hypers$dfa,
      log_transformed = log_transformed,
      strength = hyper_strength
    )
    c(stan_data_prep, stan_data_prep_priors)
  }
  
  # Function to guess column mappings with expanded patterns
  guess_columns <- function(col_names) {
    guesses <- list()
    available_cols <- col_names
    
    # Define extensive patterns for each role in English and Norwegian
    patterns <- list(
      measurement_col = c("result", "value", "y", "measure", "measurement", 
                          "concentration", "level", "reading", "output", "data", 
                          "observation", "score", "reading_value", "test_result", "response",
                          "resultat", "verdi", "måling", "konsentrasjon", "nivå", 
                          "svar", "observasjon", "poeng", "måleverdi", "testresultat"),
      subject_id_col = c("subject", "subjid", "patient", "patid", "cpr", 
                         "id", "identifier", "participant", "subjectid", "person", 
                         "individual", "person_id", "participant_id", "subject_identifier", "unique_id",
                         "person", "pasient", "deltaker", "forsøksperson", "cprnummer", 
                         "personnummer", "subjekt", "menneske", "hvem", "individ"),
      sample_id_col = c("sample", "sampid", "specimenid", "sampleid", "specimen_id", 
                        "sample_id", "visit", "time", "timepoint", "visitid", 
                        "specimen_number", "visit_number", "sample_name", "sample_code", "accession",
                        "prøve", "prøveid", "prøvenummer", "besøk", "tidspunkt", 
                        "prøve_id", "prøve_nr", "besøk_nr", "prøvenavn", "prøvekode"),
      replicate_id_col = c("replicate", "replid", "rep", "duplicate", "run", 
                           "replicateid", "replicate_id", "measurement_no", "runid", "dublet",
                           "technical_replicate", "bio_replicate", "repetition", "seq", "sequence",
                           "replikat", "duplikat", "kjøring", "replikatid", "replikat_id", 
                           "måling_nr", "teknisk_replikat", "biologisk_replikat", "gjentakelse", "sekvens"),
      analyte_col = c("analyte", "analysis", "test", "component", "parameter", 
                      "assay", "analytename", "analyte_name", "testname", "analytt", 
                      "substance", "marker", "biomarker", "test_code", "test_name",
                      "analyse", "komponent", "parameter", "analysenavn", "analyse_navn",
                      "stoff", "markør", "biomarkør", "testkode", "testnavn"),
      material_col = c("material", "matrix", "specimen", "sampletype", "sample_type", 
                       "mat", "specimentype", "specimen_type", "sample_matrix", "prøvetype", 
                       "fluid", "tissue", "source", "origin", "sample_source",
                       "materiale", "matrise", "prøvemateriale", "væske", "vev",
                       "kilde", "opprinnelse", "prøvekilde"),
      sex_col = c("sex", "gender", "female", "male", "sex_id", 
                  "gender_id", "sex_code", "gender_code", "kjonn", "sexo",
                  "M", "F", "K", "male_female", "is_male",
                  "kjønn", "kvinne", "mann", "mannlig", "kvinnelig"),
      group_1_col = c("group", "pas", "patient.group", "culture", "region", 
                      "country", "status", "healthy", "population", "sick", 
                      "disease", "condition", "treatment", "cohort", "diagnosis", 
                      "grp", "gruppe1", "ethnicity", "location", "site", 
                      "arm", "study_arm", "intervention", "control", "case",
                      "gruppe", "pasientgruppe", "etnisitet", "sykdom", "populasjon", 
                      "tilstand", "behandling", "kohort", "diagnose", "lokasjon",
                      "sted", "studiearm", "intervensjon", "kontroll"),
      group_2_col = c("group", "pas", "patient.group", "culture", "region", 
                      "country", "status", "healthy", "population", "sick", 
                      "disease", "condition", "treatment", "cohort", "diagnosis", 
                      "grp", "gruppe2", "ethnicity", "location", "site", 
                      "arm", "study_arm", "intervention", "control", "case",
                      "gruppe", "pasientgruppe", "etnisitet", "sykdom", "populasjon", 
                      "tilstand", "behandling", "kohort", "diagnose", "lokasjon",
                      "sted", "studiearm", "intervensjon", "kontroll")
    )
    
    # Function to find the first match for a set of patterns
    find_match <- function(patterns_list, cols) {
      for (p in patterns_list) {
        # Use word boundaries (\\b) to match whole words and avoid partial matches (e.g., 'rep' in 'report')
        match_idx <- grep(paste0("\\b", p, "\\b"), cols, ignore.case = TRUE)
        if (length(match_idx) > 0) {
          return(cols[match_idx[1]])
        }
      }
      # If no exact match, try partial match as a fallback
      for (p in patterns_list) {
        match_idx <- grep(p, cols, ignore.case = TRUE)
        if (length(match_idx) > 0) {
          return(cols[match_idx[1]])
        }
      }
      return(NULL)
    }
    
    # Iterate through roles and find matches
    for (role in names(patterns)) {
      match <- find_match(patterns[[role]], available_cols)
      if (!is.null(match)) {
        guesses[[role]] <- match
        available_cols <- setdiff(available_cols, match) # Remove used column
      }
    }
    
    return(guesses)
  }
  
  # Function to resolve synonyms for analyte names
  get_canonical_analyte_name <- function(input_value, synonym_map) {
    # Convert user input to lowercase for case-insensitive matching
    clean_input <- tolower(trimws(input_value))
    
    # Loop through each canonical name in our synonym map
    for (canonical_name in names(synonym_map)) {
      # Check if the user's input is in the list of accepted synonyms
      if (clean_input %in% synonym_map[[canonical_name]]) {
        return(canonical_name)
      }
    }
    
    return(NULL) # Return NULL if no match is found
  }
  
  # A helper function to parse Stan warnings and provide user-friendly advice
  parse_stan_warnings <- function(warnings, current_iter, current_adapt_delta, current_max_treedepth) {
    
    advice_list <- list()
    
    # Combine all captured warning messages into a single string for easier searching
    all_warnings_text <- paste(warnings, collapse = " \n ")
    
    # 1. Check for R-hat warnings (Convergence)
    if (grepl("R-hat", all_warnings_text, ignore.case = TRUE)) {
      advice_list$rhat <- tags$li(
        tags$strong("Convergence Issue: High R-hat values detected."),
        # Clearer explanation of the problem
        paste0(
          "The model runs multiple independent processes ('chains') to find solutions, ",
          "but they have not agreed on a consistent result. This means the analysis ",
          "failed to find a stable solution, making the results unreliable."
        ),
        tags$strong("Primary Recommendation:"),
        # Adding the "why" to the recommendation
        paste0(
          "Give the model more time to find a stable solution by increasing ",
          "the number of iterations from ",
          current_iter,
          " to at least ",
          current_iter * 2,
          "."
        ),
        tags$strong("If the problem persists:"),
        # Providing a next step
        paste0(
          "A persistent convergence issue can point to a problem with the ",
          "model itself. Consider simplifying the model or using stronger priors."
        )
      )
    }
    
    # 2. Check for Low Bulk ESS (Sample Quality)
    if (grepl("Bulk Effective Samples Size \\(ESS\\) is too low", all_warnings_text, ignore.case = TRUE)) {
      advice_list$ess_bulk <- tags$li(
        tags$strong("Poor Sample Quality: Low Bulk ESS."),
        paste0(
          "Effective Sample Size (ESS) estimates the amount of independent information in your samples. ",
          "A low value means the samples were not diverse enough to produce reliable estimates for the ",
          "center of the distribution (e.g., the mean or median)."
        ),
        tags$strong("Primary Recommendation:"),
        paste0(
          "Try to generate more independent information by increasing the number of ",
          "iterations from ",
          current_iter,
          " to at least ",
          current_iter * 2, "."
        ),
        tags$strong("If the problem persists:"),
        # Connect this warning to others
        paste0(
          "Low ESS can be a symptom of a more serious issue. ",
          "Ensure that any Divergent Transition or R-hat warnings are resolved first, ",
          "as they can be the root cause."
        )
      )
    }
    
    # 3. Check for Low Tail ESS (Sample Quality)
    if (grepl("Tail Effective Samples Size \\(ESS\\) is too low", all_warnings_text, ignore.case = TRUE)) {
      advice_list$ess_tail <- tags$li(
        tags$strong("Poor Sample Quality: Low Tail ESS."),
        # Explain what Tail ESS is and its specific consequence
        paste0(
          "Tail ESS measures sample quality for the extremes (the 'tails') of the distribution. ",
          "A low value means the model has not gathered enough information to build reliable ",
          "credible intervals or estimate extreme quantiles."
        ),
        tags$strong("Primary Recommendation:"),
        # Add the "why" to the recommendation
        paste0(
          "To better explore the tails of the distribution, increase the number of ",
          "iterations from ",
          current_iter,
          " to at least ",
          current_iter * 2, "."
        ),
        tags$strong("If the problem persists:"),
        # Connect this warning to others
        paste0(
          "This is often a symptom of a deeper model issue. Always resolve any ",
          "Divergent Transition or R-hat warnings first, as they are the likely root cause."
        )
      )
    }
    
    # 4. Check for Divergent Transitions (Model Specification / Sampling Difficulty)
    if (grepl("divergent transitions after warmup", all_warnings_text, ignore.case = TRUE)) {
      advice_list$divergences <- tags$li(
        tags$strong("Critical Issue: Divergent Transitions Detected!"),
        paste0(
          " This is a serious warning that the model is struggling to explore",
          " the full range of plausible solutions. The results are likely biased",
          " and should not be trusted. "
        ),
        tags$strong("Primary Recommendation:"),
        paste0(
          " Increase the 'Acceptance Probability' parameter from ",
          current_adapt_delta, # Assuming this is a percentage like 80 or 95
          "% to a higher value.",
          ifelse(
            as.numeric(current_adapt_delta) < 99,
            " For example: 99% ",
            ifelse(
              as.numeric(current_adapt_delta) < 99.999,
              " Use the highest possible value: 99.999% ",
              paste0(
                " But wait, you have already selected the highest value possible!",
                " Read the 'If the problem persists' text below for further guidance!"
              )
            )
          )
        ),
        tags$strong("If the problem persists:"),
        # More constructive advice than "you cannot use this model"
        paste0(
          " A high number of divergences, even with a high 'Acceptance Probability', ",
          "often points to a fundamental mismatch between the model and the data. ",
          "Consider checking your data for severe outliers or errors, ",
          "or using stronger priors which simplifies the model."
        )
      )
    }
    
    # 5. Check for Max Treedepth (Sampling Inefficiency)
    if (grepl("maximum treedepth", all_warnings_text, ignore.case = TRUE)) {
      advice_list$treedepth <- tags$li(
        tags$strong("Efficiency Warning: Exceeded Maximum Treedepth."),
        # Reassure the user first, then explain the issue
        paste0(
          "This is not an error that invalidates your results. It means the sampling ",
          "process was inefficient, requiring the maximum allowed number of steps for many ",
          "iterations, which significantly slowed down the analysis."
        ),
        tags$strong("Recommendation:"),
        # Create a clear, prioritized list of actions
        tags$ol(
          tags$li(
            paste0(
              "First, ensure all Divergent Transition warnings are resolved,",
              " as they are a more critical issue and a potential root cause."
            )
          ),
          tags$li(
            "If no divergences are present, you can improve performance by",
            " increasing the 'Maximum Treedepth' parameter. ",
            ifelse(
              as.numeric(current_max_treedepth) < 12,
              paste0(
                "For example, increase it from ",
                as.numeric(current_max_treedepth),
                " to",
                " 12. "
              ),
              ifelse(
                as.numeric(current_max_treedepth) < 14,
                paste0(
                  "For example, increase it from ",
                  as.numeric(current_max_treedepth),
                  " to",
                  " 14. "
                ),
                ifelse(
                  as.numeric(current_max_treedepth) < 16,
                  paste0(
                    "For example, increase it from ",
                    as.numeric(current_max_treedepth),
                    " to",
                    " 16. "
                  ),
                  ifelse(
                    as.numeric(current_max_treedepth) < 20,
                    paste0(
                      "For example, increase it from ",
                      as.numeric(current_max_treedepth),
                      " to",
                      "16, 17, 18, 19 or 20. "
                    ),
                    paste0(
                      "But wait, you have already selected the maximum",
                      " 'Maximum Treedepth' (20). Increasing it more than this",
                      " have little value. "
                    )
                  )
                )
              )
            )
          )
        )
      )
    }
    
    return(advice_list)
  }
  
  
  # 2. LOAD PRE-COMPILED STAN MODELS
  # --------------------------------------------------------------------------
  compiled_models <- reactiveValues()
  withProgress(message = 'Loading Stan Models...', value = 0, {
    # Correct mapping: Model 1 is NTT, Model 2 is NTTDFGAM
    model_file_1_rds <- "stan_model_1_compiled.rds" # NTT model (from stan_model_3.stan)
    model_file_2_rds <- "stan_model_2_compiled.rds" # NTTDFGAM model (from stan_model_4.stan)
    
    incProgress(0.1, detail = "Checking for compiled model files...")
    if (!file.exists(model_file_1_rds) || !file.exists(model_file_2_rds)) {
      stop(
        "One or both pre-compiled Stan model files (.rds) not found. ",
        "Please run the 'precompile_models.R' script first."
      )
    }
    
    incProgress(0.3, detail = "Loading Model 1 (NTT)...")
    compiled_models$model1 <- readRDS(model_file_1_rds)
    
    incProgress(0.7, detail = "Loading Model 2 (NTTDFGAM)...")
    compiled_models$model2 <- readRDS(model_file_2_rds)
    
    incProgress(1, detail = "Models loaded.")
  })
  
  # 3. WIZARD NAVIGATION LOGIC
  # --------------------------------------------------------------------------
  observeEvent(input$goto_step2, {
    req(input$file_upload)
    
    updateTabsetPanel(session, "setup_wizard", selected = "step2_map")
  })
  observeEvent(input$back_to_step1, {
    updateTabsetPanel(session, "setup_wizard", selected = "step1_upload")
  })
  observeEvent(input$goto_step3, {
    # Require mandatory columns to be selected before proceeding
    req(
      input$measurement_col,
      input$subject_id_col,
      input$sample_id_col,
      input$replicate_id_col
    )
    updateTabsetPanel(session, "setup_wizard", selected = "step3_filter")
  })
  observeEvent(input$back_to_step2, {
    updateTabsetPanel(session, "setup_wizard", selected = "step2_map")
  })
  observeEvent(input$goto_step4, {
    updateTabsetPanel(session, "setup_wizard", selected = "step4_params")
  })
  observeEvent(input$back_to_step3, {
    updateTabsetPanel(session, "setup_wizard", selected = "step3_filter")
  })
  
  # 4. DATA HANDLING & DYNAMIC INPUTS
  # --------------------------------------------------------------------------
  
  # Reactive to read the selected data
  uploaded_data_full <- reactive({
    req(input$file_upload)
    ext <- tools::file_ext(input$file_upload$name)
    if (ext == "xlsx") {
      sheet_names <- readxl::excel_sheets(path = input$file_upload$datapath)
      out <- sapply(
        X = sheet_names,
        FUN = function(sheet_name) {
          read_excel(
            path = input$file_upload$datapath,
            sheet = sheet_name
          )
        },
        simplify = FALSE
      )
      names(out) <- sheet_names
      return(out)
    }
    else if (ext == "csv") {
      data.table::fread(input$file_upload$datapath)
    }
    else {
      validate("Unsupported file type.")
    }
  })
  
  # Reactive to get sheet names from uploaded excel file
  names_of_excel_sheets <- reactive({
    req(input$file_upload)
    ext <- tools::file_ext(input$file_upload$name)
    if (ext == "xlsx") {
      readxl::excel_sheets(path = input$file_upload$datapath)
    } else {
      # Return a default for CSV files
      return("No sheets here ...") 
    }
  })
  
  # Observer to update the sheet selection buttons
  observe({
    req(names_of_excel_sheets())
    sheet_names <- names_of_excel_sheets()
    
    # Only enable and update if there's more than one sheet
    enable_sheet_selector <- length(sheet_names) > 1
    
    updateRadioGroupButtons(
      session = session,
      inputId = "sheet_name",
      choices = sheet_names,
      selected = sheet_names[1],
      label = NULL,
      status = "primary",
      size = "sm",
      justified = TRUE,
      disabled = !enable_sheet_selector
    )
  })
  
  uploaded_data <- reactive({
    req(
      uploaded_data_full(),
      input$sheet_name
    )
    if (inherits(uploaded_data_full(), "list")) {
      uploaded_data_full()[[input$sheet_name]]
    }
  })
  
  column_selectors <- c("measurement_col", "subject_id_col", "sample_id_col", 
                        "replicate_id_col", "analyte_col", "material_col", 
                        "sex_col", "group_1_col", "group_2_col")
  
  observe({
    req(uploaded_data())
    all_cols <- names(uploaded_data())
    selections <- sapply(column_selectors, function(id) input[[id]], simplify = FALSE)
    used_cols <- unlist(selections[selections != ""])
    
    for (selector_id in column_selectors) {
      current_val <- selections[[selector_id]]
      other_used_cols <- setdiff(used_cols, current_val)
      available_choices <- c("None" = "", setdiff(all_cols, other_used_cols))
      updateSelectInput(session, selector_id, choices = available_choices, selected = current_val)
    }
  })
  
  # Event for the "Reset" button
  observeEvent(input$reset_cols_btn, {
    for (selector_id in column_selectors) {
      updateSelectInput(session, selector_id, selected = "")
    }
  })
  
  # Event for the "Guess" button
  observeEvent(input$guess_cols_btn, {
    req(uploaded_data())
    col_names <- names(uploaded_data())
    guesses <- guess_columns(col_names)
    
    for (role in names(guesses)) {
      updateSelectInput(session, role, selected = guesses[[role]])
    }
  })
  
  # 5. DYNAMIC FILTER UI GENERATION
  # --------------------------------------------------------------------------
  output$analyte_filter_ui <- renderUI({
    req(input$analyte_col)
    unique_vals <- unique(uploaded_data()[[input$analyte_col]])
    selectizeInput("filter_analyte", "Filter by Analyte", choices = unique_vals, multiple = FALSE)
  })
  output$material_filter_ui <- renderUI({
    req(input$material_col)
    unique_vals <- unique(uploaded_data()[[input$material_col]])
    selectizeInput("filter_material", "Filter by Material", choices = unique_vals, multiple = TRUE)
  })
  output$sex_filter_ui <- renderUI({
    req(input$sex_col)
    unique_vals <- unique(uploaded_data()[[input$sex_col]])
    selectizeInput("filter_sex", "Filter by Sex", choices = unique_vals, multiple = TRUE)
  })
  output$group_1_filter_ui <- renderUI({
    req(input$group_1_col)
    label <- paste("Filter by", input$group_1_col)
    unique_vals <- unique(uploaded_data()[[input$group_1_col]])
    selectizeInput("filter_group_1", label, choices = unique_vals, multiple = TRUE)
  })
  output$group_2_filter_ui <- renderUI({
    req(input$group_2_col)
    label <- paste("Filter by", input$group_2_col)
    unique_vals <- unique(uploaded_data()[[input$group_2_col]])
    selectizeInput("filter_group_2", label, choices = unique_vals, multiple = TRUE)
  })
  
  # --- NEW ---
  # 6. OBSERVER FOR UPDATING HYPERPARAMETERS BASED ON ANALYTE
  # --------------------------------------------------------------------------
  observeEvent(input$filter_analyte, {
    
    req(input$filter_analyte)
    
    # --- UPDATED LOGIC ---
    
    # 1. Find the canonical name using our helper function and synonym map
    canonical_name <- get_canonical_analyte_name(
      input_value = input$filter_analyte,
      synonym_map = analyte_synonyms
    )
    
    # 2. Check if a valid canonical name was found
    if (!is.null(canonical_name)) {
      
      # 3. Retrieve the priors using the canonical name
      priors <- analyte_priors[[canonical_name]]
      
      # 4. Update the numeric inputs (this part remains the same)
      updateNumericInput(session, "hyper_beta", value = priors$beta)
      updateNumericInput(session, "hyper_cvi", value = priors$cvi)
      updateNumericInput(session, "hyper_cva", value = priors$cva)
      updateNumericInput(session, "hyper_cvg", value = priors$cvg)
    }
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # 6. AUTOMATIC NAME GENERATION
  # --------------------------------------------------------------------------
  # These observers automatically populate the name fields based on filter selections.
  
  observeEvent(input$filter_analyte, {
    req(input$filter_analyte)
    updateTextInput(session, "analyte_name", value = input$filter_analyte)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$filter_material, {
    req(input$filter_material)
    # Combine multiple selections into a single string
    material_name_str <- paste(input$filter_material, collapse = ", ")
    updateTextInput(session, "analyte_material", value = material_name_str)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$filter_sex, {
    req(input$filter_sex)
    # Combine multiple selections into a single string
    sex_name_str <- paste(input$filter_sex, collapse = ", ")
    updateTextInput(session, "sex_name", value = sex_name_str)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # 7. FILTERED DATA
  # --------------------------------------------------------------------------
  filtered_data <- reactive({
    data <- uploaded_data()
    req(data)
    
    apply_filter <- function(data, col_name, filter_values) {
      if (!is.null(col_name) && col_name != "" && !is.null(filter_values) && length(filter_values) > 0) {
        data <- data[data[[col_name]] %in% filter_values, ]
      }
      return(data)
    }
    
    # Filters out NA-values
    if (!is.null(input$measurement_col) && input$measurement_col != "") {
      NA_vals_rows <- which(is.na(data[[input$measurement_col]]))
      valid_vals_rows <- setdiff(1:nrow(data), NA_vals_rows)
      data <- data[valid_vals_rows, ]
    }
    
    
    data <- apply_filter(data, input$analyte_col, input$filter_analyte)
    data <- apply_filter(data, input$material_col, input$filter_material)
    data <- apply_filter(data, input$sex_col, input$filter_sex)
    data <- apply_filter(data, input$group_1_col, input$filter_group_1)
    data <- apply_filter(data, input$group_2_col, input$filter_group_2)
    return(data)
  })
  
  # 8. ANALYSIS LOGIC
  # --------------------------------------------------------------------------
  run_analysis <- function(model_to_run) {
    
    # --- Wrap All Calculations Inside withProgress() to Display Progress to User ---
    withProgress(message = 'Analysis in progress', value = 0, {
      
      req(
        model_to_run,
        filtered_data(),
        input$measurement_col,
        input$subject_id_col,
        input$sample_id_col,
        input$replicate_id_col
      )
      
      incProgress(0.1, detail = "Preparing data...")
      
      data_for_stan <- copy(filtered_data())
      setnames(
        data_for_stan,
        old = c(input$measurement_col, input$subject_id_col, input$sample_id_col, input$replicate_id_col),
        new = c("y", "SubjectID", "SampleID", "ReplicateID"),
        skip_absent = TRUE
      )
      
      hyper_strength_vec <- c(input$hyper_beta_weakness, input$hyper_cvi_weakness, input$hyper_cva_weakness, input$hyper_cvg_weakness, input$hyper_dfi_weakness, input$hyper_dfa_weakness)
      log_trans_logical <- (input$log_transformed == "Yes")
      
      prepared_stan_data <- pernille_prepares_for_stan_modeling(
        data = data_for_stan,
        hypers = list("beta" = input$hyper_beta, "cvi" = input$hyper_cvi, "cva" = input$hyper_cva, "cvg" = input$hyper_cvg, "dfi" = input$hyper_dfi, "dfa" = input$hyper_dfa),
        hyper_strength = hyper_strength_vec,
        log_transformed = log_trans_logical
      )
      
      # --- Time Estimation Logic ---
      if (as.numeric(input$iter) >= 500) { # Estimates time using 500 iterations for one chain and one core ...
        incProgress(0.2, detail = "Estimating run time...")
        
        test_iter <- 500
        start_time <- Sys.time()
        suppressWarnings({
          suppressMessages({
            sampling(
              object = model_to_run,
              data = prepared_stan_data,
              chains = 1,
              iter = test_iter,
              warmup = floor(test_iter * (as.numeric(input$burn) / 100)),
              thin = 1,
              seed = 123,
              cores = 1,
              open_progress = FALSE,
              control = list(
                adapt_delta = as.numeric(input$adapt_delta) / 100,
                max_treedepth = as.numeric(input$max_treedepth)
              )
            )
          })
        })
        end_time <- Sys.time()
        
        elapsed_seconds <- as.numeric(difftime(end_time, start_time, units = "secs")) - 1
        incProgress(0.25, detail = paste("One chain using one core takes", round(elapsed_seconds, 0L), ")", "for 500 iterations"))
        Sys.sleep(3)
        time_per_iter <- elapsed_seconds / test_iter
        estimated_seconds <- time_per_iter * as.numeric(input$iter) * as.numeric(input$nchains) / (1 / (0.05 + 0.95 / as.numeric(input$number_of_cores))) 
        
        format_time <- function(s) {
          s <- s
          mins <- floor(s / 60)
          secs <- round(s %% 60)
          if (mins > 0) paste("Will take approximately", mins, "minutes and", secs, "seconds")
          else paste("Will take approximately", secs, "seconds")
        }
        time_estimate_str <- format_time(estimated_seconds)
      }
      else {
        incProgress(0.2, detail = "Selected number of iterations not high enough to estimate run time ...")
      }
      
      
      # --- End of Time Estimation Logic ---
      
      # --- Interpret User Input Regarding Number of Iterations ---
      total_iter <- as.numeric(input$iter)
      warmup_iter <- floor(total_iter * (as.numeric(input$burn) / 100))
      
      incProgress(0.3, detail = paste("Running Bayesian model...", time_estimate_str))
      
      # Create an empty list to store warnings
      captured_warnings <- list()
      
      # Wrap the sampling call in withCallingHandlers to catch warnings
      fit <- withCallingHandlers(
        # The main expression to run:
        expr = {
          rstan::sampling(
            object = model_to_run,
            data = prepared_stan_data,
            chains = input$nchains,
            iter = as.numeric(input$iter),
            warmup = floor(as.numeric(input$iter) * (as.numeric(input$burn) / 100)),
            thin = 1,
            seed = 123,
            cores = as.numeric(input$number_of_cores),
            control = list(
              adapt_delta = as.numeric(input$adapt_delta) / 100,
              max_treedepth = as.numeric(input$max_treedepth)
            )
          )
        },
        warning = function(w) {
          captured_warnings <<- c(captured_warnings, w$message)
          # invokeRestart("muffleWarning") # Insert when captured warnings are explained as desired
        }
      )
      
      parsed_advice <- parse_stan_warnings(
        warnings = captured_warnings,
        current_iter = input$iter,
        current_adapt_delta = input$adapt_delta,
        current_max_treedepth = input$max_treedepth
      )
      
      
      incProgress(0.9, detail = "Processing posterior samples...")
      
      
      # --- Handling Posterior Samples ---
      extracted_fit <- extract(fit)
      
      # Extract all relevant summary metrics from stan output
      processed_stan_output <- process_stan_output( 
        fit = extracted_fit,
        log_transformed = log_trans_logical,
        analyte = input$analyte_name,
        material = input$analyte_material,
        sex = input$sex_name,
        group = input$group_name,
        data = data_for_stan
      )
      
      bayesian_output_table <- processed_stan_output[[1]]
      
      
      # Handling grouping aestics for plots
      color_by_input <- NULL
      shape_by_input <- NULL
      
      if (input$sex_col != "") {
        if (!is.null(input$filter_sex) && length(input$filter_sex) >= 2) {
          color_by_input <- input$sex_col  
        }
        else if (input$material_col != "") {
          if (!is.null(input$filter_material) && length(input$filter_material) >= 2) {
            color_by_input <- input$material_col
          }
        }
        else if (input$group_1_col != "") {
          if (!is.null(input$filter_group_1) && length(input$filter_group_1) >= 2) {
            color_by_input <- input$group_1_col
          }
        }
      }
      else if (input$material_col != "") {
        if (!is.null(input$filter_material) && length(input$filter_material) >= 2) {
          color_by_input <- input$material_col
        }
        else if (input$group_1_col != "") {
          if (!is.null(input$filter_group_1) && length(input$filter_group_1) >= 2) {
            color_by_input <- input$group_1_col
          }
        }
      }
      else if (input$group_1_col != "") {
        if (!is.null(input$filter_group_1) && length(input$filter_group_1) >= 2) {
          color_by_input <- input$group_1_col
        }
      }
      
      # --- Subject-Wise CV Plot --- 
      bayesian_output_plot <- plot_subject_specific_CVI(
        processed_output = processed_stan_output, 
        color_by = color_by_input,
        shape_by = shape_by_input,
        data = data_for_stan
      )
      
      # --- Subject-Wise Concentration Versus CV Plot ---
      bayesian_output_plot2 <- plot_subject_specific_CVI(
        processed_output = processed_stan_output, 
        color_by = color_by_input,
        shape_by = shape_by_input,
        data = data_for_stan,
        against_concentration = TRUE
      )
      
      incProgress(1, detail = "Done!")
      
      # --- Output List ---
      list(
        table = bayesian_output_table,
        plot = bayesian_output_plot,
        plot2 = bayesian_output_plot2,
        advice = parsed_advice # Warning messages from console captured here.
      )
    })
  }
  
  analysis_results_model1 <- eventReactive(input$run_analysis_model1_btn, {
    run_analysis(compiled_models$model1)
  })
  
  observeEvent(analysis_results_model1(), {
    
    advice <- analysis_results_model1()$advice
    
    # If the list of advice is not empty, show the modal
    if (length(advice) > 0) {
      showModal(modalDialog(
        title = tagList(icon("exclamation-triangle"), "Potential Model Issues Detected"),
        p("The analysis completed, but Stan reported the following potential issues. Please review these recommendations before trusting the results:"),
        tags$ul(
          advice # The list of <li> tags from our parsing function
        ),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
    }
  })
  
  
  analysis_results_model2 <- eventReactive(input$run_analysis_model2_btn, {
    run_analysis(compiled_models$model2)
  })
  
  observeEvent(analysis_results_model2(), {
    
    advice <- analysis_results_model2()$advice
    
    # If the list of advice is not empty, show the modal
    if (length(advice) > 0) {
      showModal(
        modalDialog(
          title = tagList(
            icon("exclamation-triangle"),
            div(
              class = "input-warning-note",
              "Potential Model Issues Detected"
            )
          ),
          p(
            paste0(
              "The analysis completed, but Stan reported the following ",
              "potential issues. Please review these recommendations before ",
              "trusting the results:"
            )
          ),
          tags$ul(
            advice
          ),
          easyClose = TRUE,
          footer = modalButton("Dismiss")
        )
      )
    }
  })
  
  # 9. RENDER OUTPUTS
  # --------------------------------------------------------------------------
  output$results_table_model1 <- renderDT({
    DT::datatable(
      data = analysis_results_model1()$table,
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(
        scolllX = TRUE,
        scrollY = "400px",
        pageLength = 5,
        dom = "Bfrtip", # B=Buttons, f=filtering, r=processing, t=table, i=info, p=pagination
        buttons = list(
          list(extend = 'copy', className = 'btn-dt'),
          list(extend = 'csv', className = 'btn-dt'),
          list(extend = 'excel', className = 'btn-dt'),
          list(extend = 'pdf', className = 'btn-dt'),
          list(extend = 'print', className = 'btn-dt')
        ),
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        ),
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().table().container()).find('.dataTables_scrollBody').on('scroll', function() {",
          "    $(this).prev('.dataTables_scrollHead').scrollLeft($(this).scrollLeft());",
          "  });",
          "}"
        )
      )
    )
  })
  
  output$subject_plot_model1 <- renderPlot({
    analysis_results_model1()$plot
  },
  res = 120)
  
  output$filtered_data_table_model1 <- renderPlot({
    analysis_results_model1()$plot2
  },
  res = 120)
  
  output$download_results_model1_btn <- downloadHandler(
    filename = function() {
      
      if (input$model_ntt_tabs == "subject_cvi_plot_ntt") {
        paste0(
          "bvem_ntt_",
          "subject_specific_cv_plot_",
          paste0(
            c(
              input$analyte_name,
              input$analyte_material,
              input$sex_name,
              input$group_name
            ),
            collapse = "_"
          ),
          "_",
          ifelse(input$log_transformed == "Yes", "log_trans_", ""),
          Sys.Date(),
          ".tif"
        )
      }
      
      else if (input$model_ntt_tabs == "cv_vs_conc_ntt") {
        paste0(
          "bvem_ntt_",
          "subject_specific_concentration_vs_cv_plot_",
          paste0(
            c(
              input$analyte_name,
              input$analyte_material,
              input$sex_name,
              input$group_name
            ),
            collapse = "_"
          ),
          "_",
          ifelse(input$log_transformed == "Yes", "log_trans_", ""),
          Sys.Date(),
          ".tif"
        )
      }
      
      else {
        paste0(
          "bvem_ntt_",
          "high_level_summary_stats_",
          paste0(
            c(
              input$analyte_name,
              input$analyte_material,
              input$sex_name,
              input$group_name
            ),
            collapse = "_"
          ),
          "_",
          ifelse(input$log_transformed == "Yes", "log_trans_", ""),
          Sys.Date(),
          ".xlsx"
        )  
      }
    },
    content = function(file) {
      
      if (input$model_ntt_tabs == "subject_cvi_plot_ntt") {
        ggsave(
          filename = file,
          plot = analysis_results_model1()$plot,
          device = "tif",
          dpi = 450,
          width = 16.8,
          height = 16.8,
          units = "cm"
        )
      }
      
      else if (input$model_ntt_tabs == "cv_vs_conc_ntt") {
        ggsave(
          filename = file,
          plot = analysis_results_model1()$plot2,
          device = "tif",
          dpi = 450,
          width = 16.8,
          height = 16.8,
          units = "cm"
        )
      }
      
      else {
        writexl::write_xlsx(
          x = analysis_results_model1()$table,
          format_headers = TRUE,
          path = file
        )  
      }
    }
  )
  output$results_table_model2 <- renderDT({
    DT::datatable(
      data = analysis_results_model2()$table,
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(
        scolllX = TRUE,
        scrollY = "400px",
        pageLength = 5,
        dom = "Bfrtip", # B=Buttons, f=filtering, r=processing, t=table, i=info, p=pagination
        buttons = list(
          list(extend = 'copy', className = 'btn-dt'),
          list(extend = 'csv', className = 'btn-dt'),
          list(extend = 'excel', className = 'btn-dt'),
          list(extend = 'pdf', className = 'btn-dt'),
          list(extend = 'print', className = 'btn-dt')
        ),
        columnDefs = list(
          list(className = 'dt-center', targets = "_all")
        ),
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().table().container()).find('.dataTables_scrollBody').on('scroll', function() {",
          "    $(this).prev('.dataTables_scrollHead').scrollLeft($(this).scrollLeft());",
          "  });",
          "}"
        )
      )
    )
  })
  
  output$subject_plot_model2 <- renderPlot({
    analysis_results_model2()$plot
  })
  
  output$filtered_data_table_model2 <- renderPlot({
    analysis_results_model2()$plot2
  },
  res = 120)
  
  output$download_results_model2_btn <- downloadHandler(
    filename = function() {
      if (input$model_ntt_tabs == "subject_cvi_plot_nttdfgam") {
        paste0(
          "bvem_nttdfgam_",
          "subject_specific_cv_plot_",
          paste0(
            c(
              input$analyte_name,
              input$analyte_material,
              input$sex_name,
              input$group_name
            ),
            collapse = "_"
          ),
          "_",
          ifelse(input$log_transformed == "Yes", "log_trans_data_", "raw_data_"),
          Sys.Date(),
          ".tif"
        )
      }
      else {
        paste0(
          "bvem_nttdfgam_",
          "high_level_summary_stats_",
          paste0(
            c(
              input$analyte_name,
              input$analyte_material,
              input$sex_name,
              input$group_name
            ),
            collapse = "_"
          ),
          "_",
          ifelse(input$log_transformed == "Yes", "log_trans_", ""),
          Sys.Date(),
          ".xlsx"
        )  
      }
    },
    content = function(file) {
      
      if (input$model_ntt_tabs == "subject_cvi_plot_nttdfgam") {
        ggsave(
          filename = file,
          plot = analysis_results_model2()$plot,
          device = "tif",
          dpi = 450,
          width = 16.8,
          height = 16.8,
          units = "cm"
        )
      }
      else {
        writexl::write_xlsx(
          x = analysis_results_model2()$table,
          format_headers = TRUE,
          path = file
        )  
      }
    }
  )
}

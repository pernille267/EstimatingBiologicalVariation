# DOCUMENTATION AND NOTES
# ------------------------------------------------------------------------------

# --- Understanding the Shiny Components ---------------------------------------

# req(...): The input can be (one or multiple) input$# or a logical value
# Requires the variable inside to be either not NULL or TRUE. Nothing after it
# will be evaluated if the variance inside is NULL or FALSE

# reactive({...}): The input must be an expression
# Executes the code (...) inside the brackets each time there is
# an particular update in the application. For example, such an update can be a change in
# `input$#` or a change of other reactive() expressions, which depend on the
# original reactive expression. It is however lazy in that it only updates when
# it is needed.
# 
# For example:
# uploaded_data_full only updates if and of these are satisfied:
# - input$file_upload is not NULL (because of req(...))
# - input$file_upload is updated in the UI

# observe({...}): The input must be an expression
# Performs one or more actions in response to any particular changes in its scope.
# Does not return a value, in contrast to reactive({...}).
# It creates a link to reactive things in the code (...). Reactive things
# are components that can change, either by the user in the UI (input$#) or
# somewhere on the server (back-end reactive components).
# observe({...}) is an observer that pays close attention to changes in input$# or
# reactive expressions defined by reactive({...}) inside the code (...).
# An observer is very eager, because it will execute the code (...) EVERYTIME
# it observes a change in the reactive things it is linked to.

# observeEvent(Event, Action): The two first arguments are expressions meant
# for two different things. The first one is the expression for the event and
# the second one is the expression for the action(s) that should be executed
# if the expression for the event results in a change from its previous state.

# Does not return a value, in contrast to reactive({...}).

# We can say that it observes one specific thing (the first argument; Event)
# It ignores all reactive things defined in the Action-argument. However, it
# will execute the Action-argument EVERYTIME it observes a change in final
# output of the Event expression.


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
library(truncnorm)

# --- Helper Constants ---

# Default names for expected columns to exist in uploaded data (may not match these names)
column_selectors <- c("measurement_col", "subject_id_col", "sample_id_col", 
                      "replicate_id_col", "analyte_col", "material_col", 
                      "sex_col", "group_1_col", "group_2_col")

# Define predifined hyperparameter priors for specific analytes (update and push request)
analyte_priors <- list(
  "HbA1c" = list(beta = 5, cvi = 1.2, cva = 2.5, cvg = 5.4),
  "Glucose" = list(beta = 5, cvi = 4.7, cva = 2.5, cvg = 8.0),
  "ALA" = list(beta = 2, cvi = 16.0, cva = 5.0, cvg = 27.0),
  "PBG" = list(beta = 1.8, cvi = 20.0, cva = 8.0, cvg = 30.0)
  # Add other analytes here as needed
)

# Define analyte name synomym mapping (update and push request)
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
  "PBG" = c(
    "pbg", "porphobilinogen", "porfobilinogen", "u-pbg"
  )
)

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
  
<<<<<<< HEAD
=======
  
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
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
    # Require that the user have uploaded a file
    req(input$file_upload)
    
    # Extract the file extension part of the path (xlsx, csv, or illegal alternatives)
    ext <- tools::file_ext(input$file_upload$name)
    
    # Check if extension is `xlsx`, and extract the sheet names if it is `xlsx`
    if (ext == "xlsx") {
      readxl::excel_sheets(path = input$file_upload$datapath)
    } 
    else {
      # Return a default message for CSV-files or illegal files
      return("No sheets here ...") 
    }
  })
  
  # Note: `names_of_excel_sheets` must be independent of `uploaded_data_full`
  
  # Observer to update the sheet selection buttons
  observe({
    # Require sheetnames to exist before selection buttons can be updated
    req(names_of_excel_sheets())
    
    # Get sheet names
    sheet_names <- names_of_excel_sheets()
    
    # Only enable and update if there are more than one sheet
    enable_sheet_selector <- length(sheet_names) > 1
    
    # Update `RadioGroupButtons`
    # - Defaults to first sheet name in the list
    # - Only allow users to click on buttons if there are more than one sheet
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
  
  # Reactive to extract the sheet of the uploaded file (if .xlsx)
  uploaded_data <- reactive({
    # Requires uploaded file (either .xlsx or .csv)
    req(
      uploaded_data_full(),
      input$sheet_name
    )
    if (inherits(uploaded_data_full(), "list")) {
      uploaded_data_full()[[input$sheet_name]]
    }
  })
  
  # Observer to update list of available columns in uploaded data
  # - The user is enabled to map the indended purpose of a column to a column name of their data
  observe({
    # Requires valid uploaded data (from file: .xlsx or .csv)
    # - At this point, a sheet of the uploaded data is already selected
    req(uploaded_data())
    
    # Extract the column names of the uploaded data
    all_cols <- names(uploaded_data())
    
    # Keep track of mappings that the user have performed at any time
    selections <- sapply(column_selectors, function(id) input[[id]], simplify = FALSE)
    
    # Keep track of which columns that cannot longer be mapped (because they are already mapped)
    used_cols <- unlist(selections[selections != ""])
    
    # Update the list of possible mappings as the user select mappings
    # - If a user select a particular mapping, this mapping will not longer
    #   be possible to select. This is to avoid "double mapping".
    # - The user can at any time press "reset" to start over (see `ObserveEvent` below)
    for (selector_id in column_selectors) {
      current_val <- selections[[selector_id]]
      other_used_cols <- setdiff(used_cols, current_val)
      available_choices <- c("None" = "", setdiff(all_cols, other_used_cols))
      updateSelectInput(session, selector_id, choices = available_choices, selected = current_val)
    }
  })
  
  # Event observer for the "Reset" button
  # - Resets any mapping selections done manually or by pressing "Guess" (see `observeEvent` below)
  observeEvent(input$reset_cols_btn, {
    # Requires valid uploaded data (from file: .xlsx or .csv)
    # - At this point, a sheet of the uploaded data is already selected
    req(uploaded_data())
    
    # Replace all selections with empty selections ("") if "Reset" button is pressed
    for (selector_id in column_selectors) {
      updateSelectInput(session, selector_id, selected = "")
    }
  })
  
  # Event observer for the "Guess" button
  # - Attempts to guesss the appropriate mappings using AI 
  observeEvent(input$guess_cols_btn, {
    # Requires valid uploaded data (from file: .xlsx or .csv)
    # - At this point, a sheet of the uploaded data is already selected
    req(uploaded_data())
    
    # Extract the column names of the uploaded data 
    col_names <- names(uploaded_data())
    
    # For each column name, make the AI guess which mapping that is appropriate
    # - Based on word-recongnition
    guesses <- guess_columns(col_names)
    
    # Automatically fill in guessed mappings
    for (role in names(guesses)) {
      updateSelectInput(session, role, selected = guesses[[role]])
    }
  })
  
  # 5. DYNAMIC FILTER UI GENERATION
<<<<<<< HEAD
  # ----------------------------------------------------------------------------
  
  # --- Turn on Dynamic Filtering where Possible -------------------------------
  
  # Turn on analyte filtering if an `analyte` column exists and is mapped
  # - Analyte can for example be `glucose`, `HBa1c`, or `CRP`
  # - Users can only select one analyte at the time
=======
  # --------------------------------------------------------------------------
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
  output$analyte_filter_ui <- renderUI({
    req(input$analyte_col)
    unique_vals <- unique(uploaded_data()[[input$analyte_col]])
    selectizeInput("filter_analyte", "Filter by Analyte", choices = unique_vals, multiple = FALSE)
  })
  
  # Turn on material filtering if an `material` column exists and is mapped
  # - material can for example be `Serum`, `Whole blood`, `Urine` or `Plasma`
  # * FIX REQUIRED: Only materials that correspond to a user-selected `analyte`
  # * should be possible to select from the list!!!
  output$material_filter_ui <- renderUI({
    req(input$material_col)
    unique_vals <- unique(uploaded_data()[[input$material_col]])
    selectizeInput("filter_material", "Filter by Material", choices = unique_vals, multiple = TRUE)
  })
  
  # Turn on sex filtering if an `sex` column exists and is mapped
  # - sex can for example be `Male` or `Female`
  output$sex_filter_ui <- renderUI({
    req(input$sex_col)
    unique_vals <- unique(uploaded_data()[[input$sex_col]])
    selectizeInput("filter_sex", "Filter by Sex", choices = unique_vals, multiple = TRUE)
  })
  
  # Turn on filtering for one of the allowable custom groups
  # - Such groups can be `Fasting`, `Desease` or `Age` (in the context age can be considered a group)
  # * POSSIBLE WISE FIX: this should possibly be replaced with `desease state` filtering
  # * or some other variable that is typical in BV experiments...
  output$group_1_filter_ui <- renderUI({
    req(input$group_1_col)
    label <- paste("Filter by", input$group_1_col)
    unique_vals <- unique(uploaded_data()[[input$group_1_col]])
    selectizeInput("filter_group_1", label, choices = unique_vals, multiple = TRUE)
  })
  
  # Turn on filtering for the second of the allowable custom groups
  # - See examples above.
  output$group_2_filter_ui <- renderUI({
    req(input$group_2_col)
    label <- paste("Filter by", input$group_2_col)
    unique_vals <- unique(uploaded_data()[[input$group_2_col]])
    selectizeInput("filter_group_2", label, choices = unique_vals, multiple = TRUE)
  })
  
<<<<<<< HEAD
  # 6. UPDATING HYPERPARAMETERS BASED ON SELECTED ANALYTE
  # --------------------------------------------------------------------------
  
  # Event observer to update the hyperparameters EACH time the user:
  # - Make a change in their filtering of the analyte variable column
  observeEvent(input$filter_analyte, {
    
    # Require that an analyte variable column exists in the data and ...
    # - A particular filter is applied to it (can be empty - no filter)
    req(input$filter_analyte)
    
    # Find the canonical analyte name using our analyte name synonym mapper (fancy AI)
=======
  # --- NEW ---
  # 6. OBSERVER FOR UPDATING HYPERPARAMETERS BASED ON ANALYTE
  # --------------------------------------------------------------------------
  observeEvent(input$filter_analyte, {
    
    req(input$filter_analyte)
    
    # --- UPDATED LOGIC ---
    
    # 1. Find the canonical name using our helper function and synonym map
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
    canonical_name <- get_canonical_analyte_name(
      input_value = input$filter_analyte,
      synonym_map = analyte_synonyms
    )
    
<<<<<<< HEAD
    # Check if a valid canonical name was found (i.e., `canonical_name` is not `NULL`)
    if (!is.null(canonical_name)) {
      
      # Retrieve the priors using the canonical name
      # Note: `analyte_priors` is defined above server(...)
      priors <- analyte_priors[[canonical_name]]
      
      # Update the default numeric inputs for `beta`, `cvi`, `cva` and `cvg`
=======
    # 2. Check if a valid canonical name was found
    if (!is.null(canonical_name)) {
      
      # 3. Retrieve the priors using the canonical name
      priors <- analyte_priors[[canonical_name]]
      
      # 4. Update the numeric inputs (this part remains the same)
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
      updateNumericInput(session, "hyper_beta", value = priors$beta)
      updateNumericInput(session, "hyper_cvi", value = priors$cvi)
      updateNumericInput(session, "hyper_cva", value = priors$cva)
      updateNumericInput(session, "hyper_cvg", value = priors$cvg)
    }
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # 6. AUTOMATIC NAME GENERATION
  # --------------------------------------------------------------------------
<<<<<<< HEAD
  
  # --- These event observers automatically populate the name fields -----------
  # - Based on changes in filtering choices made by the user
  # Note: These name fields can be manually changed after automatic updates
  # However, if the user makes a change in filtering, the defaults will be
  # reintroduced... (I do not know if this is going to be annoying or not)
  
  # Event observer to set the default analyte name field to match the selected filtering
  observeEvent(input$filter_analyte, {
    # Requires a valid mapping and filter of analyte
    req(input$filter_analyte)
    
    # Update default analyte name (can be changed manually after)
    updateTextInput(session, "analyte_name", value = input$filter_analyte)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Event observer to change analyte material name field to match the column name in uploaded data
  observeEvent(input$filter_material, {
    # Requires a valid mapping and filter of analyte material
    req(input$filter_material)
    
    # If multiple selections, join strings and seperate with `, `
    material_name_str <- paste(input$filter_material, collapse = ", ")
    
    # Update default analyte material name (can be changed manually after)
    updateTextInput(session, "analyte_material", value = material_name_str)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Event observer to change sex name field to match the column name in uploaded data
  observeEvent(input$filter_sex, {
    # Requires a valid mapping and filter of sex
    req(input$filter_sex)
    
    # Combine multiple selections into a single string
    sex_name_str <- paste(input$filter_sex, collapse = ", ")
    
    # Update default sex name (can be changed manually after)
=======
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
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
    updateTextInput(session, "sex_name", value = sex_name_str)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # 7. FILTERED DATA
  # --------------------------------------------------------------------------
  filtered_data <- reactive({
    # Requires valid uploaded data
    req(uploaded_data())
    
    # Create a local variable for the uploaded data
    data <- uploaded_data()
    
    # Helper function: Keep rows that corresponds with user-desired filters
    # Applies to one column (`col_name`) in `data`.
    # `filter_values` is allowed to be a character vector.
    apply_filter <- function(data, col_name, filter_values) {
      if (!is.null(col_name) && (col_name != "") && !is.null(filter_values) && (length(filter_values) > 0)) {
        # Extract the rows in which the relevant column (`data[[col_name]]`)
        # have values that also are in `filter_values`
        data <- data[data[[col_name]] %in% filter_values, ]
      }
      return(data)
    }
    
    # Remove rows with NA-values in the `measurement_col`
    # Check first if `measurement_col` is valid (not NULL or an empty string)
    # Then: Find NA-value row indices => Derive non NA-value row indices =>
    # Keep only the part of `data` corresponding to the non NA-value row indices.
    if (!is.null(input$measurement_col) && input$measurement_col != "") {
      NA_vals_rows <- which(is.na(data[[input$measurement_col]]))
      valid_vals_rows <- setdiff(1:nrow(data), NA_vals_rows)
      data <- data[valid_vals_rows, ]
    }
    
    # Apply filters in the correct order:
    # Analyte filter => Material filter => Sex filter => ...
    data <- apply_filter(data, input$analyte_col, input$filter_analyte)
    data <- apply_filter(data, input$material_col, input$filter_material)
    data <- apply_filter(data, input$sex_col, input$filter_sex)
    data <- apply_filter(data, input$group_1_col, input$filter_group_1)
    data <- apply_filter(data, input$group_2_col, input$filter_group_2)
    return(data)
  })
  
  # 8. ANALYSIS LOGIC
<<<<<<< HEAD
  # ----------------------------------------------------------------------------
  
  # --- The Grand Modelling function -------------------------------------------
  # Performs all necessary tasks for a complete analysis
  run_analysis <- function(model_to_run) {
    
    # Wrap All Calculations Inside withProgress() to Display Progress to User
=======
  # --------------------------------------------------------------------------
  run_analysis <- function(model_to_run) {
    
    # --- Wrap All Calculations Inside withProgress() to Display Progress to User ---
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
    withProgress(message = 'Analysis in progress', value = 0, {
      
      # Before starting the following are required:
      # 1. The model to be run must be chosen (automatic)
      # 2. The reactive expression `filtered_data` must exist and be valid
      # 3. The measurement column is mapped and valid
      # 4. The subject ID column is mapped and valid
      # 5. The sample ID column is mapped and valid
      # 6. The replicate ID column is mapped and valid
      req(
        model_to_run,
        filtered_data(),
        input$measurement_col,
        input$subject_id_col,
        input$sample_id_col,
        input$replicate_id_col
      )
      
      # 10 % done
      incProgress(0.1, detail = "Preparing data...")
      
      # Make a copy of `filtered_data` to avoid modifying it
      data_for_stan <- copy(filtered_data())
      
      # Convert user-specified names with canonical names that the `biovar`
      # package recongnizes
      setnames(
        data_for_stan,
        old = c(
          input$measurement_col,
          input$subject_id_col,
          input$sample_id_col,
          input$replicate_id_col),
        new = c("y", "SubjectID", "SampleID", "ReplicateID"),
        skip_absent = TRUE
      )
      
      # Get the `weakness` factors for each hyperparameter
      # Note: These are denoted strength in the `biovar` package, which is
      # confusing...
      hyper_strength_vec <- c(
        input$hyper_beta_weakness,
        input$hyper_cvi_weakness,
        input$hyper_cva_weakness,
        input$hyper_cvg_weakness,
        input$hyper_dfi_weakness,
        input$hyper_dfa_weakness
      )
      
      # Create a logical value from the string `input$log_transformed`
      # Because the `biovar` package only accepts logical values
      log_trans_logical <- (input$log_transformed == "Yes")
      
      # Pack data and all modelling parameters into one `list` object
      prepared_stan_data <- pernille_prepares_for_stan_modeling(
        data = data_for_stan,
        hypers = list(
          "beta" = input$hyper_beta,
          "cvi" = input$hyper_cvi,
          "cva" = input$hyper_cva,
          "cvg" = input$hyper_cvg,
          "dfi" = input$hyper_dfi,
          "dfa" = input$hyper_dfa),
        hyper_strength = hyper_strength_vec,
        log_transformed = log_trans_logical
      )
      
      # 15 % done
      incProgress(0.1, detail = "Setting starting values for the sampler...")
      
      # Create a seperate object for all hyperparameter information.
      # Because these hyperparameters are used as starting values
      hypers <- process_stan_data_priors(
        beta = input$hyper_beta,
        cvi = input$hyper_cvi,
        cva = input$hyper_cva,
        cvg = input$hyper_cvg,
        dfi = input$hyper_dfi,
        dfa = input$hyper_dfa,
        log_transformed = log_trans_logical,
        strength = hyper_strength_vec
      )
      
      # Create starting value function
      initfunc.stan <- function() hypers
      
      # --- Time Estimation Logic ---
<<<<<<< HEAD
      # Estimates run time for the actual sampling using
      # - 500 iterations
      # - 1 chain
      # - 1 core
      # Limitation: Only works if this trial run is representative of the
      # actual run. If the sampler runs unexpectedely fast in the trial run
      # the actual run time will be underestimates. If the sampler runs
      # unexpectedely slow in the trial run, the actual run time will be
      # overestimated.
      
      # Only performs a trial run if the original sampling is to be performed
      # using at least 1000 iterations. 
      if (as.numeric(input$iter) >= 1000) {
        # 20 % done
        incProgress(
          amount = 0.2,
          detail = "Estimating run time..."
        )
        
        test_iter <- 500
        
        # Record the time before the trial run starts
        start_time <- Sys.time()
        
        # Warnings and Messages are not relevant for thr trial run.
        # They are suppressed!
=======
      if (as.numeric(input$iter) >= 500) { # Estimates time using 500 iterations for one chain and one core ...
        incProgress(0.2, detail = "Estimating run time...")
        
        test_iter <- 500
        start_time <- Sys.time()
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
        suppressWarnings({
          suppressMessages({
            sampling(
              object = model_to_run,
              data = prepared_stan_data,
              chains = 1,
              iter = test_iter,
              warmup = floor(test_iter * (as.numeric(input$burn) / 100)),
<<<<<<< HEAD
              init = initfunc.stan,
=======
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
              thin = 1,
              seed = 123,
              cores = 1,
              open_progress = FALSE,
              control = list(
                adapt_delta = as.numeric(input$adapt_delta) / 100,
                max_treedepth = as.numeric(input$max_treedepth)
<<<<<<< HEAD
              ),
              refresh = 10
            )
          })
        })
        
        # Record the time after the trial run has finished
        end_time <- Sys.time()
        
        # Compute the elapsed time:
        # Time before trial run starts - Time after trial run finishes
        # Present the result in seconds here
        elapsed_seconds <- as.numeric(difftime(end_time, start_time, units = "secs")) - 1
        
        # 25 % done
        incProgress(
          amount = 0.25,
          detail = paste(
            "Based on 500 iterations using one chain and core takes",
            round(elapsed_seconds, 0L),
            "seconds!",
            ifelse(
              elapsed_seconds > 180,
              "This is going to take a VERY long time. Suspicious. Are your settings correct?",
              ifelse(
                elapsed_seconds > 120,
                "This is going to take a while. Let us hope you are patient...",
                ifelse(
                  elapsed_seconds < 15,
                  "That is fast ... Maybe too fast? Very fast is not necessarily positive...",
                  ifelse(
                    elapsed_seconds > 60,
                    "Sit back and relax. Or not. Up to you.",
                    "This is going to take but a moment. The sampler seems efficient. Good job!"
                  )
                )
              )
            )
          )
        )
        
        # Give the user some time to read how long it took to run
        # the trial sampler. Program sleeps for three seconds.
        Sys.sleep(5)
        
        # Calculate the time it takes to perform a single iteration
        time_per_iter <- elapsed_seconds / test_iter
        
        # Estimate the expected run time for the actual sampling (in seconds)
        estimated_time_all_iters <- time_per_iter * as.numeric(input$iter)
        estimated_time_all_chains <- estimated_time_all_iters * as.numeric(input$nchains)
        multiple_cores_factor <- 1 / (0.05 + 0.95 / as.numeric(input$number_of_cores)) 
        estimated_seconds <-  estimated_time_all_chains / multiple_cores_factor
        
        # Helper function: Text that describes how many minutes and seconds
        # the sampler will take (an estimate) to be finished.
        # If the number of seconds is larger than or equal to 60, the message
        # will be given with both minutes and seconds. Otherwise, the message
        # will be given in only seconds.
=======
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
        
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
        format_time <- function(s) {
          s <- s
          mins <- floor(s / 60)
          secs <- round(s %% 60)
<<<<<<< HEAD
          if (mins > 0) {
            paste(
              "Sampling will take approximately",
              mins,
              "minutes and",
              secs,
              "seconds."
            ) 
          }
          else {
            paste(
              "Sampling is expected to be fast! It will take approximately",
              secs,
              "seconds."
            ) 
          }
        }
        
        # Reformat the estimated time as a nice-looking string.
        time_estimate_str <- format_time(estimated_seconds)
      }
      else {
        # 25 % done
        incProgress(
          amount = 0.25,
          detail = "Selected number of iterations not high enough to justify estimating the run time ..."
        )
=======
          if (mins > 0) paste("Will take approximately", mins, "minutes and", secs, "seconds")
          else paste("Will take approximately", secs, "seconds")
        }
        time_estimate_str <- format_time(estimated_seconds)
      }
      else {
        incProgress(0.2, detail = "Selected number of iterations not high enough to estimate run time ...")
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
      }
      
      
      # --- End of Time Estimation Logic ---
      
<<<<<<< HEAD
      # 28 % done
      incProgress(
        amount = 0.28,
        detail = paste(
          "Interpreting sampling user inputs from `Advanced Settings`..."
        )
      )
      
      # Give the user some time to read that user inputs are retrieved
      # from the `Advanced Settings` Tab.
      Sys.sleep(2)
      
      # --- Convert User Inputs to Types Accepted by `sampling(...)` -----------
      
      # Interpret and Convert User Input Regarding Number of Iterations
      total_iter <- as.numeric(input$iter)
      warmup_iter <- floor(total_iter * (as.numeric(input$burn) / 100))
      
      # Interpret and Convert User Input Regarding Control Parameters
      interpreted_adapt_delta <- as.numeric(input$adapt_delta) / 100
      interpreted_max_treedepth <- as.numeric(input$max_treedepth)
      
      # Interpret and Convert User Input Regarding Chains and Cores
      interpreted_number_of_chains <- as.integer(input$nchains)
      interpreted_number_of_cores <- as.integer(input$number_of_cores)
      
      # 30 % done
      incProgress(
        amount = 0.3,
        detail = paste(
          "Running Bayesian model...\n",
          time_estimate_str
        )
      )
      
      # Create an empty list to store warnings
      captured_warnings <- list()
      
      # --- Start the Sampler --------------------------------------------------
      
      # Wrap the sampling call in `withCallingHandlers` to catch warnings
      fit <- withCallingHandlers(
        expr = {
          rstan::sampling(
            object = model_to_run,
            data = prepared_stan_data,
            chains = interpreted_number_of_chains,
            iter = total_iter,
            warmup = warmup_iter,
            init = initfunc.stan,
            thin = 1,
            seed = 123,
            cores = interpreted_number_of_cores,
            control = list(
              adapt_delta = interpreted_adapt_delta,
              max_treedepth = interpreted_max_treedepth
            ),
            refresh = 100
          )
        },
        warning = function(w) {
          captured_warnings <<- c(captured_warnings, w$message)
          # invokeRestart("muffleWarning") # Insert when captured warnings are explained as desired
        }
      )
      
      # 85 % done
      incProgress(
        amount = 0.85,
        detail = paste(
          "Sampling done ..."
        )
      )
      
      # 88% done
      incProgress(
        amount = 0.88,
        detail = paste(
          "Evaluating warnings (if any) ..."
        )
      )
      
      # Reformat warnings if any were catched during the sampling
      parsed_advice <- parse_stan_warnings(
        warnings = captured_warnings,
        current_iter = input$iter,
        current_adapt_delta = input$adapt_delta,
        current_max_treedepth = input$max_treedepth
      )
      
      # 90% done
      incProgress(
        amount = 0.9,
        detail = "Processing posterior and predictive distribution samples..."
      )
=======
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
      
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
      
      # --- Handling Posterior Samples ---
      extracted_fit <- extract(fit)
      
<<<<<<< HEAD
      #browser()
      out <<- extracted_fit
      
=======
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
      # Extract all relevant summary metrics from stan output
      processed_stan_output <- process_stan_output( 
        fit = extracted_fit,
        log_transformed = log_trans_logical,
        analyte = input$analyte_name,
        material = input$analyte_material,
        sex = input$sex_name,
        group = input$group_name,
<<<<<<< HEAD
        data = data_for_stan
      )
      
      # 92% done
      incProgress(
        amount = 0.92,
        detail = "Making High-level Summary Statistics Table ..."
      )
      
      # Extract high-level summary table from `process_stan_output`
      bayesian_output_table <- processed_stan_output[[1]]
      
      # Temporary: For debugging ...
      print(processed_stan_output[[3]])
      
      # 95% done
      incProgress(
        amount = 0.95,
        detail = "Constructing plots now..."
      )
      
      # Handling grouping aestics for plots
      color_by_input <- NULL
      shape_by_input <- NULL
      
      # Representing sex with colors are preferred
      if (input$sex_col != "") {
        if (!is.null(input$filter_sex) && length(input$filter_sex) >= 2) {
          color_by_input <- input$sex_col  
        }
        # If sex is not a candidate, material are preferred to be represented by color
        else if (input$material_col != "") {
          if (!is.null(input$filter_material) && length(input$filter_material) >= 2) {
            color_by_input <- input$material_col
          }
        }
        # If neither sex or analyte material are candidates, group 1
        # are preferred to be represented by color
        else if (input$group_1_col != "") {
          if (!is.null(input$filter_group_1) && length(input$filter_group_1) >= 2) {
            color_by_input <- input$group_1_col
          }
        }
      }
      # Representing analyte material with colors are preferred after sex
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
      
      # 100 % done
      incProgress(
        amount = 1,
        detail = "Everything is done!"
      )
      
=======
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
      
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
      # --- Output List ---
      list(
        table = bayesian_output_table,
        plot = bayesian_output_plot,
        plot2 = bayesian_output_plot2,
        advice = parsed_advice # Warning messages from console captured here.
      )
    })
  }
  
<<<<<<< HEAD
  # Event Reactive Expression which is rerun each time the `Run NTT Model`
  # button is pressed. In this case, the expression is:
  # - `run_analysis(...$model1)`
  # - Note: Will only run if the system is not busy when the button is pressed
=======
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
  analysis_results_model1 <- eventReactive(input$run_analysis_model1_btn, {
    run_analysis(compiled_models$model1)
  })
  
<<<<<<< HEAD
  # Event Observer which produce advice to the user (based on collected warnings)
  # each time `analysis_results_model1()` changes state. In practice it changes
  # state each time the `Run NTT Model` button is pressed while the system is
  # not busy.
  observeEvent(analysis_results_model1(), {
    
    # Extract `advice` (list element of the output of `run_analysis`)
    advice <- analysis_results_model1()$advice
    
    # If there is at least one warning, there is at least one advice to show
    # to the user. Therefore, we check whether the length of the `advice` list is
    # non-empty. If it is non-empty, a warning modal pops up, which the user
    # can read, and then cancel out if they are finished (or do not care).
    if (length(advice) > 0) {
      showModal(
        ui = modalDialog(
          title = tagList(
            icon("exclamation-triangle"),
            "Potential Model Issues Detected"
          ),
          p(
            paste0(
              "The sampling from the Bayesian model is completed. However, ",
              "Stan reported the following potential issues. ",
              "It is important to review these messages and recommendations ",
              "before you start trusting the results! An ethically inclined ",
              "person would never do that, and you should not either."
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
  
  # Event Reactive Expression which is rerun each time the `Run NTTDFGAM Model`
  # button is pressed. In this case, the expression is:
  # - `run_analysis(...$model2)`
  # - Note: Will only run if the system is not busy when the button is pressed
=======
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
  
  
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
  analysis_results_model2 <- eventReactive(input$run_analysis_model2_btn, {
    run_analysis(compiled_models$model2)
  })
  
<<<<<<< HEAD
  # Event Observer which produce advice to the user (based on collected warnings)
  # each time `analysis_results_model2()` changes state. In practice it changes
  # state each time the `Run NTTDFGAM Model` button is pressed while the system is
  # not busy.
  observeEvent(analysis_results_model2(), {
    
    # Extract `advice` (list element of the output of `run_analysis`)
    advice <- analysis_results_model2()$advice
    
    # If there is at least one warning, there is at least one advice to show
    # to the user. Therefore, we check whether the length of the `advice` list is
    # non-empty. If it is non-empty, a warning modal pops up, which the user
    # can read, and then cancel out if they are finished (or do not care).
=======
  observeEvent(analysis_results_model2(), {
    
    advice <- analysis_results_model2()$advice
    
    # If the list of advice is not empty, show the modal
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
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
<<<<<<< HEAD
              "The sampling from the NTTDFGAM Bayesian model is completed. ",
              "However, Stan reported the following potential issues. ",
              "It is important to review these messages and recommendations ",
              "before you start trusting the results! An ethically inclined ",
              "person would never do that, and you should not either."
=======
              "The analysis completed, but Stan reported the following ",
              "potential issues. Please review these recommendations before ",
              "trusting the results:"
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
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
  
<<<<<<< HEAD
  # 9. RENDER OUTPUTS (TABLES & PLOTS)
  # ----------------------------------------------------------------------------
  
  # Render the summary statistics table (NTT model)
  # - Typically only one row.
  # - Can scroll in x-direction and y-direction
  # - Export buttons included (copy, csv, excel, pdf, and print)
=======
  # 9. RENDER OUTPUTS
  # --------------------------------------------------------------------------
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
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
  
  # Render subject-specific CV(%) plot (NTT model)
  # - Subjects on the y-axis
  # - CV_p(i) (%) on x-axis (median + 95% credible interval) (error bars)
  # - Order of Subjects are matched by the order of median CV_p(i) (%) (acending)
  # - Groups are represented by colors and point shapes
  # - Web quality: 120
  output$subject_plot_model1 <- renderPlot({
    analysis_results_model1()$plot
  },
  res = 120)
  
<<<<<<< HEAD
  # Concentration versus CV(%) plot (NTT model)
  # - Concentration (`beta` + G_i) on the x-axis
  # - CV_p(i) (%) on the y-axis (median + 95% credible intervals) (error bars)
  # - Groups are represented by colors and point shapes
  # - Web quality: 120
=======
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
  output$filtered_data_table_model1 <- renderPlot({
    analysis_results_model1()$plot2
  },
  res = 120)
  
<<<<<<< HEAD
  # Construct download handler for the NTT model outputs
  # Which object that is downloaded depends on which tab that is currently open ...
  output$download_results_model1_btn <- downloadHandler(
    filename = function() {
      
      # Checks if the `Subject-Specific CV Plot` tab is selected
      if (input$model_ntt_tabs == "subject_cvi_plot_ntt") {
        # Create a generic file name for the downloaded plot (.tif)
=======
  output$download_results_model1_btn <- downloadHandler(
    filename = function() {
      
      if (input$model_ntt_tabs == "subject_cvi_plot_ntt") {
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
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
      
<<<<<<< HEAD
      # Checks if the `Concentration Versus CV Plot` tab is selected
      else if (input$model_ntt_tabs == "cv_vs_conc_ntt") {
        # Create a generic file name for the downloaded plot (.tif)
=======
      else if (input$model_ntt_tabs == "cv_vs_conc_ntt") {
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
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
      
<<<<<<< HEAD
      # Checks if the `Summary Statistics` tab is selected
      else {
        # Create a generic file name for the downloaded table (.xlsx)
=======
      else {
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
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
      
<<<<<<< HEAD
      # Checks if the `Subject-Specific CV Plot` tab is selected
      if (input$model_ntt_tabs == "subject_cvi_plot_ntt") {
        # Save as .tif to disk (16.8 x 16.8 cm) with quality 450 (dpi)
=======
      if (input$model_ntt_tabs == "subject_cvi_plot_ntt") {
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
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
      
<<<<<<< HEAD
      # Checks if the `Concentration Versus CV Plot` tab is selected
      else if (input$model_ntt_tabs == "cv_vs_conc_ntt") {
        # Save as .tif to disk (16.8 x 16.8 cm) with quality 450 (dpi)
=======
      else if (input$model_ntt_tabs == "cv_vs_conc_ntt") {
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
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
      
<<<<<<< HEAD
      # Checks if the `Summary Statistics` tab is selected
      else {
        # Save as .xlsx to disk (with centered headers in bold)
=======
      else {
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
        writexl::write_xlsx(
          x = analysis_results_model1()$table,
          format_headers = TRUE,
          path = file
        )  
      }
    }
  )
  
  # Render the summary statistics table (NTTDFGAM model)
  # - Typically only one row.
  # - Can scroll in x-direction and y-direction
  # - Export buttons included (copy, csv, excel, pdf, and print)
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
  
  # Render subject-specific CV(%) plot (NTTDFGAM model)
  # - Subjects on the y-axis
  # - CV_p(i) (%) on x-axis (median + 95% credible interval) (error bars)
  # - Order of Subjects are matched by the order of median CV_p(i) (%) (acending)
  # - Groups are represented by colors and point shapes
  # - Web quality: 120
  output$subject_plot_model2 <- renderPlot({
    analysis_results_model2()$plot
  })
  
<<<<<<< HEAD
  # Concentration versus CV(%) plot (NTTDFGAM model)
  # - Concentration (`beta` + G_i) on the x-axis
  # - CV_p(i) (%) on the y-axis (median + 95% credible intervals) (error bars)
  # - Groups are represented by colors and point shapes
  # - Web quality: 120
=======
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
  output$filtered_data_table_model2 <- renderPlot({
    analysis_results_model2()$plot2
  },
  res = 120)
  
<<<<<<< HEAD
  # Construct download handler for the NTTDFGAM model outputs
  # Which object that is downloaded depends on which tab that is currently open ...
  output$download_results_model2_btn <- downloadHandler(
    filename = function() {
      
      # Checks if the `Subject-Specific CV Plot` tab is selected
      if (input$model_ntt_tabs == "subject_cvi_plot_nttdfgam") {
        # Create a generic file name for the downloaded plot (.tif)
=======
  output$download_results_model2_btn <- downloadHandler(
    filename = function() {
      if (input$model_ntt_tabs == "subject_cvi_plot_nttdfgam") {
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
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
<<<<<<< HEAD
      
      # Checks if the `Concentration Versus CV Plot` tab is selected
      else if (input$model_ntt_tabs == "cv_vs_conc_nttdgam") {
        # Create a generic file name for the downloaded plot (.tif)
        paste0(
          "bvem_nttdgam_",
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
      
      # Checks if the `Summary Statistics` tab is selected
      else {
        # Create a generic file name for the downloaded table (.xlsx)
=======
      else {
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
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
      
<<<<<<< HEAD
      # Checks if the `Subject-Specific CV Plot` tab is selected
      if (input$model_ntt_tabs == "subject_cvi_plot_nttdfgam") {
        # Save as .tif to disk (16.8 x 16.8 cm) with quality 450 (dpi)
=======
      if (input$model_ntt_tabs == "subject_cvi_plot_nttdfgam") {
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
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
<<<<<<< HEAD
      
      # Checks if the `Concentration Versus CV Plot` tab is selected
      else if (input$model_ntt_tabs == "cv_vs_conc_ntt") {
        # Save as .tif to disk (16.8 x 16.8 cm) with quality 450 (dpi)
        ggsave(
          filename = file,
          plot = analysis_results_model2()$plot2,
          device = "tif",
          dpi = 450,
          width = 16.8,
          height = 16.8,
          units = "cm"
        )
      }
      
      # Checks if the `Summary Statistics` tab is selected
      else {
        # Save as .xlsx to disk (with centered headers in bold)
=======
      else {
>>>>>>> 6f1b2c53becf023614bb03d6bd800520c970532a
        writexl::write_xlsx(
          x = analysis_results_model2()$table,
          format_headers = TRUE,
          path = file
        )  
      }
    }
  )
  
  # 10. EASTER EGGS (TOOLS FOR DEVELOPER OR THOSE ABLE TO FIND THEM)
  # ----------------------------------------------------------------------------
  
  # Construct and Render a histogram for the filtered data
  # ANOVA-based estimates of `beta`, `CVI`, `CVA` and `CVG` are included
  # - Web quality: 120
  output$hist <- renderPlot({
    
    # Requires the following:
    # 1. The reactive expression `filtered_data` must exist and be valid
    # 2. The measurement column is mapped and valid
    # 3. The subject ID column is mapped and valid
    # 4. The sample ID column is mapped and valid
    # 5. The replicate ID column is mapped and valid
    req(
      filtered_data(),
      input$measurement_col,
      input$subject_id_col,
      input$sample_id_col,
      input$replicate_id_col,
      input$log_hist
    )
    
    # Make a copy of `filetered_data`, so that we do not accidently modify it
    data_to_plot <- copy(filtered_data())
    
    # Ensure the mappings are correct before proceding to convert names to canonical
    current_mappings <- c(
      input$measurement_col,
      input$subject_id_col,
      input$sample_id_col,
      input$replicate_id_col
    )
    
    # Check if current mappings can be linked to names in `filtered_data`
    # Return `NULL` if there is something wrong ...
    if (!all(current_mappings %in% names(data_to_plot))) {
      return(NULL)
    }
    
    # Convert mapping names (original column names) to canonical column names
    setnames(
      x = data_to_plot,
      old = c(input$subject_id_col, input$sample_id_col, input$replicate_id_col, input$measurement_col),
      new = c("SubjectID", "SampleID", "ReplicateID", "y"),
      skip_absent = TRUE
    )
    
    # Parameters for `biovar::variance_components`
    desired_output_type <- "cv_ci"
    desired_mult <- 100
    
    # Transform data and change input parameters for `biovar::variance_components`
    # if data is desired log-transformed.
    if (input$log_hist == "Yes") {
      data_to_plot$y <- log(data_to_plot$y)
      desired_output_type <- "sigma_ci"
      desired_mult <- 1
    }
    
    # Use ANOVA to calculate estimates of `beta`, `CVI`, `CVA` and `CVG`
    # The function `biovar::variance_components` does exactly this!
    bv_anova_results <- variance_components(
      data = data_to_plot,
      output_type = desired_output_type,
      mult = desired_mult,
      level = 0.95 # 95% confindence intervals
    )
    
    # Transform from log-scale to identity scale if relevant
    # Note: relevant if data is log-transformed
    if (input$log_hist == "Yes") {
      bv_anova_results$sigma_I <- sqrt(exp(bv_anova_results$sigma_I^2) - 1) * 100
      bv_anova_results$sigma_G <- sqrt(exp(bv_anova_results$sigma_G^2) - 1) * 100
      bv_anova_results$sigma_A <- sqrt(exp(bv_anova_results$sigma_A^2) - 1) * 100
    }
    
    # Formatting point estimates and confidence intervals
    # - Always use one decimal
    # - Create mathematical expressions for all estimators (greek symbols etc.)
    
    # Formatting `beta`
    beta <- sprintf(
      "hat(beta) == '%.1f'",
      mean(data_to_plot$y)
    )
    
    # Formatting `CVI`
    CV_I <- sprintf(
      "CV[I] == '%.1f' ~ `(`*'%.1f'*`-`*'%.1f'*`)`",
      bv_anova_results$sigma_I[1],
      bv_anova_results$sigma_I[2],
      bv_anova_results$sigma_I[3]
    )
    
    # Formatting `CVG`
    CV_G <- sprintf(
      "CV[G] == '%.1f' ~ `(`*'%.1f'*`-`*'%.1f'*`)`",
      bv_anova_results$sigma_G[1],
      bv_anova_results$sigma_G[2],
      bv_anova_results$sigma_G[3]
    )
    
    # Formatting `CVA`
    CV_A <- sprintf(
      "CV[A] == '%.1f' ~ `(`*'%.1f'*`-`*'%.1f'*`)`",
      bv_anova_results$sigma_A[1],
      bv_anova_results$sigma_A[2],
      bv_anova_results$sigma_A[3]
    )
    
    # Construct the plot object (freestyle)
    # - Histogram with density on the y-axis
    # - Density estimation curve overlays the histogram
    # Note: the plot is not finished here yet. Continue later
    plot_obj <- ggplot() +
      geom_histogram(
        data = data_to_plot,
        mapping = aes(x = y, y = after_stat(density)),
        bins = 15,
        fill = "#28A745",
        color = "black",
        alpha = 0.7
      ) +
      geom_density(
        data = data_to_plot,
        mapping = aes(x = y),
        color = "#605CA8",
        linewidth = 0.75
      ) 
    
    # Placement of `beta`, `CVI`, `CVA` and `CVG` (somewhat tedious task)
    
    # Get automatically derived limits for the y-axis
    plot_obj_y_lims <- ggplot_build(plot_obj)$layout$panel_scales_y
    
    # Determine relative y-axis positions of `beta`, `CVI`, `CVA` and `CVG`
    # (relative to the range of the y-axis values and upper limit)
    plot_obj_y_position <- sapply(
      plot_obj_y_lims,
      function(x) {
        lims <- x$range$range
        max_y <- lims[2]
        range_y <- diff(lims)
        return(max_y - c(0.1, 0.2, 0.3, 0.4) * range_y)
      })
    
    # Get automatically derived limits for the y-axis
    plot_obj_x_lims <- ggplot_build(plot_obj)$layout$panel_scales_x
    
    # Determine relative x-axis position of `beta`, `CVI`, `CVA` and `CVG`
    # (same for all four, making them aligned vertically)
    plot_obj_x_position <- sapply(
      plot_obj_x_lims,
      function(x) {
        lims <- x$range$range
        max_x <- lims[2]
        range_x <- diff(lims)
        return(max_x - 0.10 * range_x)
      })
    
    # Pack together ANOVA-based estimates (and their reformatting)
    # together with their absolute positions (x, y) in the histogram figure.
    descriptive_stats_data <- data.table(
      "parameter" = c("beta", "cvi", "cvg", "cva"),
      "label" = c(beta, CV_I, CV_G, CV_A),
      "y" = rep(plot_obj_x_position, 4L),
      "x" = plot_obj_y_position
    ) 
    
    #browser()
    
    # Continue construction of the plot object (freestyle). Modifications:
    # - Add formatted and nice-looking ANOVA-based estimates and confidence intervals
    # - Change number of ticks on both axes to 10
    # - Change x-axis name to `input$measurement_col` and ln(`input$measurement_col`)
    #   if data is selected to be log-transformed
    # - Change y-axis name to "Density"
    # - Use a clean plotting theme (`classic`)
    #   (no visible grids, minimalistic, sharp and clean)
    plot_obj <- plot_obj +
      geom_text(
        data = descriptive_stats_data,
        mapping = aes(
          x = y,
          y = x.V1,
          label = label
        ),
        parse = TRUE
      ) +
      scale_x_continuous(
        name = ifelse(input$log_hist == "Yes", paste0("ln(", input$measurement_col, ")"), input$measurement_col),
        n.breaks = 10,
        expand = expansion(mult = c(0, 0))
      ) +
      scale_y_continuous(
        name = "Density",
        n.breaks = 10,
        expand = expansion(mult = c(0, 0))
      ) +
      theme_classic() +
      theme(
        axis.title = element_text(face = "bold", color = "#605CA8"),
        axis.text.x = element_text(color = "#000000"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      )
    
    # Render the histogram
    plot(plot_obj)

  }, res = 120)
  
  # Construct and Render a plotting-grid that visualizes the prior probability
  # distributions. Focuses on the six most relevant priors:
  # `beta`, `CVI`, `CVA`, `CVG`, `dfI`, and `dfA`
  output$prior_densities <- renderPlot({
    
    # Makes no actual requirements
    req(
      input$hyper_beta > 0,
      input$hyper_cvi > 0,
      input$hyper_cva > 0,
      input$hyper_cvg > 0,
      input$hyper_dfi > 0,
      input$hyper_dfa > 0,
      input$hyper_beta_weakness > 0,
      input$hyper_cvi_weakness > 0,
      input$hyper_cva_weakness > 0,
      input$hyper_cvg_weakness > 0,
      input$hyper_dfi_weakness > 0,
      input$hyper_dfa_weakness > 0
    )
    
    # --- Get Effective Hyperparameters (Expected Values Only) -----------------
    # Note: We call them `effective` because they are those actually used
    # in the stan scripts.
    # Note: We transform to effective hyperparameters from direct input values
    # Transformation method changes if log-transformation is desired.
    
    # Effective E[`beta`]
    effective_beta <- ifelse(
      input$log_priors == "Yes",
      log(input$hyper_beta),
      input$hyper_beta
    )
    
    # Note: In the stan script, CVs are not used directly. We assign a prior to
    # the standard deviations, SDs.
    # Define: E[E[`CVi`]]=E[`CVI`]; E[`CVi`] = `CVI`
    
    # Effective E[`CVI`] (i.e., E[`SDI`])
    effective_mean_sdi <- ifelse(
      input$log_priors == "Yes",
      sqrt(log((input$hyper_cvi / 100)^2 + 1)) * sqrt((input$hyper_dfi - 2) / input$hyper_dfi),
      input$hyper_cvi * effective_beta / 100 * sqrt((input$hyper_dfi - 2) / input$hyper_dfi) 
    )
    
    # Effective E[SD[`CVi`]] (i.e., E[SD[`SDi`]])
    effective_sd_sdi <- ifelse(
      input$log_priors == "Yes",
      0.5 * sqrt(log((input$hyper_cvi / 100)^2 + 1)) * sqrt((input$hyper_dfi - 2) / input$hyper_dfi),
      0.5 * input$hyper_cvi * effective_beta / 100 * sqrt((input$hyper_dfi - 2) / input$hyper_dfi)
    )
    
    # Effective E[`CVA`] (i.e., E[`SDA`])
    effective_sda <- ifelse(
      input$log_priors == "Yes",
      sqrt(log((input$hyper_cva / 100)^2 + 1)) * sqrt((input$hyper_dfa - 2) / input$hyper_dfa),
      input$hyper_cva * effective_beta / 100 * sqrt((input$hyper_dfa - 2) / input$hyper_dfa)
    )
    
    # Effective E[`CVG`] (i.e., E[`SDG`])
    effective_sdg <- ifelse(
      input$log_priors == "Yes",
      sqrt(log((input$hyper_cvg / 100)^2 + 1)),
      input$hyper_cvg * effective_beta / 100 
    )
    
    # Effective first parameter for `dfI`
    effective_dfi <- ifelse(
      input$nttdfgam_priors == "Yes",
      (1/input$hyper_dfi_weakness)^2,
      input$hyper_dfi
    )
    
    # Effective first parameter for `dfA`
    effective_dfa <- ifelse(
      input$nttdfgam_priors == "Yes",
      (1/input$hyper_dfa_weakness)^2,
      input$hyper_dfa
    )
    
    
    # Prior parameters (mean and standard deviation for `beta`, `mean_sdi`, `sd_sdi` `sda`, `sdg`)
    beta_pars <- effective_beta * c(1, input$hyper_beta_weakness)
    sdi_mean_pars <- effective_mean_sdi * c(1, input$hyper_cvi_weakness)
    sdi_sd_pars <- effective_sd_sdi * c(1, 4/3)
    hbhr_pars <- c(1/2, 2/3)
    sda_pars <- effective_sda * c(1, input$hyper_cva_weakness)
    sdg_pars <- effective_sdg * c(1, input$hyper_cvg_weakness)
    
    # Prior parameters (for `dfI` and `dfA`)
    # Either mean and standard deviation or location and rate
    if(input$nttdfgam_priors == "Yes") {
      dfi_pars <- effective_dfi * c(1, 1/input$hyper_dfi)
      dfa_pars <- effective_dfa * c(1, 1/input$hyper_dfa)
    }
    else {
      dfi_pars <- effective_dfi * c(1, input$hyper_dfi_weakness)
      dfa_pars <- effective_dfa * c(1, input$hyper_dfa_weakness)
    }
    
    # Simulate from actual priors 
    # Note: TN is short for truncated normal distribution
    
    # `eff_beta` ~ TN(E[`beta`], E[`beta`] * F[`beta`], > 0)
    beta_actual_prior_samples <- rnorm(
      n = 1e5,
      mean = beta_pars[1],
      sd = beta_pars[2]
    )
    
    # `eff_mean_cvi` = `mean_sdi` ~ TN(sdI, sdI * F[`sdi`], > 0)
    cvi_mean_actual_prior_samples <- truncnorm::rtruncnorm(
      1e5,
      a = 0,
      b = Inf,
      mean = sdi_mean_pars[1],
      sd = sdi_mean_pars[2]
    )
    
    # `eff_sd_cvi` = `sd_sdi ~ TN(0.5 * sdI, 2/3 * sdI, > 0)`
    cvi_sd_actual_prior_samples <- truncnorm::rtruncnorm(
      n = 1e5,
      a = 0,
      b = Inf,
      mean = sdi_sd_pars[1],
      sd = sdi_sd_pars[2]
    )
    
    # `hbhr = sd_sdi / SDI` ~ TN(1/2, 2/3, > 0)
    hbhr_actual_prior_samples <- truncnorm::rtruncnorm(
      n = 1e5,
      a = 0,
      b = Inf,
      mean = hbhr_pars[1],
      sd = hbhr_pars[2]
    )
    
    cvi_actual_prior_samples <- truncnorm::rtruncnorm(
      n = 1e5,
      a = 0,
      b = Inf,
      mean = cvi_mean_actual_prior_samples,
      sd = cvi_sd_actual_prior_samples
    )
    
    # `eff_cva` = `sda` ~ TN(E[`sda`], E[`sda`] * F[`sda`], > 0)
    cva_actual_prior_samples <- truncnorm::rtruncnorm(
      1e5,
      a = 0,
      b = Inf,
      mean = sda_pars[1],
      sd = sda_pars[2]
    )
    
    # `eff_cvg` = `sdg` ~ TN(E[`sdg`], E[`sdg`] * F[`sdg`], > 0)
    cvg_actual_prior_samples <- truncnorm::rtruncnorm(
      1e5,
      a = 0,
      b = Inf,
      mean = sdg_pars[1],
      sd = sdg_pars[2]
    )
    
    # `eff_dfi - 2` ~ TN(E[`dfi`], E[`dfi`] * F[`dfi`], > 0)
    # or `eff_dfi - 2` ~ gamma(1 / F[`dfi`]^2, 1 / (E[`dfi`] * F[`dfi`]^2))
    if (input$nttdfgam_priors == "Yes") {
      dfi_actual_prior_samples <- rgamma(
        n = 1e5,
        shape = dfi_pars[1],
        rate = dfi_pars[2]
      )
      dfi_user_prior_samples <- dfi_actual_prior_samples + 2
    }
    else {
      dfi_actual_prior_samples <- truncnorm::rtruncnorm(
        n = 1e5,
        a = 0,
        b = Inf,
        mean = dfi_pars[1],
        sd = dfi_pars[2]
      )
      dfi_user_prior_samples <- dfi_actual_prior_samples + 2
    }
    
    # `eff_dfa - 2` ~ TN(E[`dfa`], E[`dfa`] * F[`dfa`], > 0)
    # or `eff_dfa - 2` ~ gamma(1 / F[`dfa`]^2, 1 / (E[`dfa`] * F[`dfa`]^2))
    if (input$nttdfgam_priors == "Yes") {
      dfa_actual_prior_samples <- rgamma(
        n = 1e5,
        shape = dfa_pars[1],
        rate = dfa_pars[2]
      )
      dfa_user_prior_samples <- dfa_actual_prior_samples + 2
    }
    else {
      dfa_actual_prior_samples <- truncnorm::rtruncnorm(
        n = 1e5,
        a = 0,
        b = Inf,
        mean = dfa_pars[1],
        sd = dfa_pars[2]
      )
      dfa_user_prior_samples <- dfa_actual_prior_samples + 2
    }
    
    # Transform simulated samples back to "user" priors
    if (input$log_priors == "Yes") {
      beta_user_prior_samples <- exp(beta_actual_prior_samples)
      cvi_mean_user_prior_samples <- sqrt(exp(cvi_mean_actual_prior_samples^2 * input$hyper_dfi / (input$hyper_dfi - 2)) - 1) * 100
      cvi_sd_user_prior_samples <- sqrt(exp(cvi_sd_actual_prior_samples^2  * input$hyper_dfi / (input$hyper_dfi - 2)) - 1) * 100
      cvi_user_prior_samples <- sqrt(exp(cvi_actual_prior_samples^2 * input$hyper_dfi / (input$hyper_dfi - 2)) - 1) * 100
      cva_user_prior_samples <- sqrt(exp(cva_actual_prior_samples^2 * input$hyper_dfa / (input$hyper_dfa - 2)) - 1) * 100
      cvg_user_prior_samples <- sqrt(exp(cvg_actual_prior_samples^2) - 1) * 100
    }
    else {
      beta_user_prior_samples <- beta_actual_prior_samples
      cvi_mean_user_prior_samples <- cvi_mean_actual_prior_samples / effective_beta * 100 * sqrt(input$hyper_dfi / (input$hyper_dfi - 2)) 
      cvi_sd_user_prior_samples <- cvi_sd_actual_prior_samples / effective_beta * 100 * input$hyper_dfi / (input$hyper_dfi - 2)
      cvi_user_prior_samples <- cvi_actual_prior_samples / effective_beta * 100 * input$hyper_dfi / (input$hyper_dfi - 2)
      cva_user_prior_samples <- cva_actual_prior_samples / effective_beta * 100 * input$hyper_dfa / (input$hyper_dfa - 2)
      cvg_user_prior_samples <- cvg_actual_prior_samples / effective_beta * 100
    }
    
    # Create plotting objects
    beta_data <- data.table("param" = rep("beta", 1e5),
                            "prior_sample" = beta_user_prior_samples)
    
    cvi_mean_data <- data.table("param" = rep("CVI", 1e5),
                           "prior_sample" = cvi_mean_user_prior_samples)
    
    cvi_sd_data <- data.table("param" = rep("sd_CVi", 1e5),
                              "prior_sample" = cvi_sd_user_prior_samples)
    
    cvi_data <- data.table("param" = rep("CVi", 1e5),
                           "prior_sample" = cvi_user_prior_samples)
    
    hbhr_data <- data.table("param" = rep("HBHR", 1e5),
                            "prior_sample" = hbhr_actual_prior_samples * 100)
    
    cva_data <- data.table("param" = rep("CVA", 1e5),
                           "prior_sample" = cva_user_prior_samples)
    
    cvg_data <- data.table("param" = rep("CVG", 1e5),
                           "prior_sample" = cvg_user_prior_samples)
    
    dfi_data <- data.table("param" = rep("dfI", 1e5),
                           "prior_sample" = dfi_user_prior_samples)
    
    dfa_data <- data.table("param" = rep("dfA", 1e5),
                           "prior_sample" = dfa_user_prior_samples)
    
    # Create one seperate density plot for each prior
    
    # `beta` prior density plot
    beta_plot_obj <- ggplot() +
      geom_density(
        data = beta_data,
        mapping = aes(
          x = prior_sample
        ),
        adjust = 2,
        bounds = c(-Inf, Inf),
        color = "#605CA8",
        fill = "#28A745",
        alpha = 0.7
      ) +
      geom_vline(
        xintercept = c(0, input$hyper_beta)
      ) +
      scale_x_continuous(
        name = expression(beta),
        #n.breaks = 6,
        breaks = seq(
          from = quantile(
            beta_data$prior_sample,
            probs = 0.01,
            names = FALSE
          ),
          to = quantile(
            beta_data$prior_sample,
            probs = 0.99,
            names = FALSE
          ),
          length.out = 8
        ),
        labels = function(x) {
          if (diff(range(x)) < 1) {
            format(
              x = x,
              nsmall = 2,
              digits = 2
            )
          }
          else if (diff(range(x)) < 8) {
            format(
              x = x,
              nsmall = 1,
              digits = 1
            )
          }
          else {
            round(x)
          }
        },
        limits = c(-NA, NA),
        expand = expansion(mult = c(0,0))
      ) +
      scale_y_continuous(
        name = "Density",
        breaks = NULL,
        expand = expansion(mult = c(0,0))
      ) +
      coord_cartesian(
        xlim = c(
          quantile(
            x = beta_data$prior_sample,
            probs = 0.0025,
            names = FALSE
          ),
          quantile(
            x = beta_data$prior_sample,
            probs = 0.9975,
            names = FALSE
          )
        )
      ) +
      theme_classic()
    
    # E[`CVI`] prior density plot
    cvi_mean_plot_obj <- ggplot() +
      geom_density(
        data = cvi_mean_data,
        mapping = aes(
          x = prior_sample
        ),
        adjust = 2,
        bounds = c(0, Inf),
        color = "#605CA8",
        fill = "#28A745",
        alpha = 0.7
      ) +
      geom_vline(
        xintercept = c(input$hyper_cvi)
      ) +
      scale_x_continuous(
        name = expression(E(CV[I])),
        #n.breaks = 6,
        breaks = seq(
          from = 0,
          to = quantile(
            x = cvi_mean_data$prior_sample,
            probs = 0.99,
            names = FALSE
          ),
          length.out = 8
        ),
        labels = function(labz) {
          if (diff(range(labz)) < 1) {
            paste0(
              format(
                x = labz,
                nsmall = 2,
                digits = 2
              ),
              " %"
            )
          }
          else if (diff(range(labz)) < 8) {
            paste0(
              format(
                x = labz,
                nsmall = 1,
                digits = 1
              ),
              " %"
            )
          }
          else {
            paste0(
              round(labz),
              " %"
            )
          }
        }, 
        limits = c(0, NA),
        expand = expansion(mult = c(0,0))
      ) +
      scale_y_continuous(
        name = "Density",
        breaks = NULL,
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.01))
      ) +
      coord_cartesian(
        xlim = c(
          0,
          quantile(
            x = cvi_mean_data$prior_sample,
            probs = 0.995,
            names = FALSE
          )
        )
      ) +
      theme_classic()
    
    # SD[`CVI`] prior density plot
    cvi_sd_plot_obj <- ggplot() +
      geom_density(
        data = cvi_sd_data,
        mapping = aes(
          x = prior_sample
        ),
        adjust = 2,
        bounds = c(0, Inf),
        color = "#605CA8",
        fill = "#28A745",
        alpha = 0.7
      ) +
      geom_vline(
        xintercept = c(input$hyper_cvi * 0.5)
      ) +
      scale_x_continuous(
        name = expression(SD(CV[I])),
        #n.breaks = 6,
        breaks = seq(
          from = 0,
          to = quantile(
            x = cvi_sd_data$prior_sample,
            probs = 0.99,
            names = FALSE
          ),
          length.out = 8
        ),
        labels = function(labz) {
          if (diff(range(labz)) < 1) {
            paste0(
              format(
                x = labz,
                nsmall = 2,
                digits = 2
              ),
              " %"
            )
          }
          else if (diff(range(labz)) < 8) {
            paste0(
              format(
                x = labz,
                nsmall = 1,
                digits = 1
              ),
              " %"
            )
          }
          else {
            paste0(
              round(labz),
              " %"
            )
          }
        },
        limits = c(0, NA),
        expand = expansion(mult = c(0,0))
      ) +
      scale_y_continuous(
        name = "Density",
        breaks = NULL,
        expand = expansion(mult = c(0, 0.01))
      ) +
      coord_cartesian(
        xlim = c(
          0,
          quantile(
            x = cvi_sd_data$prior_sample,
            probs = 0.995,
            names = FALSE
          )
        )
      ) +
      theme_classic()
    
    # `CVI` prior density plot
    cvi_plot_obj <- ggplot() +
      geom_density(
        data = cvi_data,
        mapping = aes(
          x = prior_sample
        ),
        adjust = 2,
        bounds = c(0, Inf),
        color = "#605CA8",
        fill = "#28A745",
        alpha = 0.7
      ) +
      geom_vline(
        xintercept = c(input$hyper_cvi)
      ) +
      scale_x_continuous(
        name = expression(CV[I]),
        #n.breaks = 6,
        breaks = seq(
          from = 0,
          to = quantile(
            x = cvi_data$prior_sample,
            probs = 0.99,
            names = FALSE
          ),
          length.out = 8
        ),
        labels = function(labz) {
          if (diff(range(labz)) < 1) {
            paste0(
              format(
                x = labz,
                nsmall = 2,
                digits = 2
              ),
              " %"
            )
          }
          else if (diff(range(labz)) < 8) {
            paste0(
              format(
                x = labz,
                nsmall = 1,
                digits = 1
              ),
              " %"
            )
          }
          else {
            paste0(
              round(labz),
              " %"
            )
          }
        },
        limits = c(0, NA),
        expand = expansion(mult = c(0,0))
      ) +
      scale_y_continuous(
        name = "Density",
        breaks = NULL,
        expand = expansion(mult = c(0, 0.01))
      ) +
      coord_cartesian(
        xlim = c(
          0,
          quantile(
            x = cvi_data$prior_sample,
            probs = 0.995,
            names = FALSE
          )
        )
      ) +
      theme_classic()
    
    # `HBHR` prior density plot
    hbhr_plot_obj <- ggplot() +
      geom_density(
        data = hbhr_data,
        mapping = aes(
          x = prior_sample
        ),
        adjust = 2,
        bounds = c(0, Inf),
        color = "#605CA8",
        fill = "#28A745",
        alpha = 0.7
      ) +
      geom_vline(
        xintercept = c(50)
      ) +
      scale_x_continuous(
        name = "HBHR",
        #n.breaks = 6,
        breaks = seq(
          from = 0,
          to = 200,
          length.out = 9
        ),
        labels = function(labz) {
          if (diff(range(labz)) < 1) {
            paste0(
              format(
                x = labz,
                nsmall = 2,
                digits = 2
              ),
              " %"
            )
            
          }
          else if (diff(range(labz)) < 8) {
            paste0(
              format(
                x = labz,
                nsmall = 1,
                digits = 1
              ),
              " %"
            )
          }
          else {
            paste0(
              round(labz),
              " %"
            )
          }
        },
        limits = c(0, NA),
        expand = expansion(mult = c(0,0))
      ) +
      scale_y_continuous(
        name = "Density",
        breaks = NULL,
        expand = expansion(mult = c(0, 0.01))
      ) +
      coord_cartesian(
        xlim = c(
          0,
          quantile(
            x = hbhr_data$prior_sample,
            probs = 0.995,
            names = FALSE
          )
        )
      ) +
      theme_classic()
    
    # `CVA` prior density plot
    cva_plot_obj <- ggplot() +
      geom_density(
        data = cva_data,
        mapping = aes(
          x = prior_sample
        ),
        adjust = 2,
        bounds = c(0, Inf),
        color = "#605CA8",
        fill = "#28A745",
        alpha = 0.7
      ) +
      geom_vline(
        xintercept = c(input$hyper_cva)
      ) +
      scale_x_continuous(
        name = expression(CV[A]),
        #n.breaks = 6,
        breaks = seq(
          from = 0,
          to = quantile(
            x = cva_data$prior_sample,
            probs = 0.99,
            names = FALSE
          ),
          length.out = 8
        ),
        labels = function(labz) {
          if (diff(range(labz)) < 1) {
            paste0(
              format(
                x = labz,
                nsmall = 2,
                digits = 2
              ),
              " %"
            )
            
          }
          else if (diff(range(labz)) < 8) {
            paste0(
              format(
                x = labz,
                nsmall = 1,
                digits = 1
              ),
              " %"
            )
          }
          else {
            paste0(
              round(labz),
              " %"
            )
          }
        },
        limits = c(0, NA),
        expand = expansion(mult = c(0,0))
      ) +
      scale_y_continuous(
        name = "Density",
        breaks = NULL,
        expand = expansion(mult = c(0, 0.01))
      ) +
      coord_cartesian(
        xlim = c(
          0,
          quantile(
            x = cva_data$prior_sample,
            probs = 0.995,
            names = FALSE
          )
        )
      ) +
      theme_classic()
    
    # `CVG` prior density plot
    cvg_plot_obj <- ggplot() +
      geom_density(
        data = cvg_data,
        mapping = aes(
          x = prior_sample
        ),
        adjust = 2,
        bounds = c(0, Inf),
        color = "#605CA8",
        fill = "#28A745",
        alpha = 0.7
      ) +
      geom_vline(
        xintercept = c(input$hyper_cvg)
      ) +
      scale_x_continuous(
        name = expression(CV[G]),
        #n.breaks = 6,
        breaks = seq(
          from = 0,
          to = quantile(
            x = cvg_data$prior_sample,
            probs = 0.99,
            names = FALSE
          ),
          length.out = 8
        ),
        labels = function(labz) {
          if (diff(range(labz)) < 1) {
            paste0(
              format(
                x = labz,
                nsmall = 2,
                digits = 2
              ),
              " %"
            )
            
          }
          else if (diff(range(labz)) < 8) {
            paste0(
              format(
                x = labz,
                nsmall = 1,
                digits = 1
              ),
              " %"
            )
          }
          else {
            paste0(
              round(labz),
              " %"
            )
          }
        },
        limits = c(0, NA),
        expand = expansion(mult = c(0,0))
      ) +
      scale_y_continuous(
        name = "Density",
        breaks = NULL,
        expand = expansion(mult = c(0, 0.01))
      ) +
      coord_cartesian(
        xlim = c(
          0,
          quantile(
            x = cvg_data$prior_sample,
            probs = 0.995,
            names = FALSE
          )
        )
      ) +
      theme_classic()
    
    # `dfI` prior density plot
    dfi_plot_obj <- ggplot() +
      geom_density(
        data = dfi_data,
        mapping = aes(
          x = prior_sample
        ),
        adjust = 2,
        bounds = c(2, Inf),
        color = "#605CA8",
        fill = "#28A745",
        alpha = 0.7
      ) +
      geom_vline(
        xintercept = c(2, input$hyper_dfi)
      ) +
      scale_x_continuous(
        name = expression(df[I]),
        #n.breaks = 6,
        breaks = seq(
          from = 2,
          to = quantile(
            x = dfi_data$prior_sample,
            probs = 0.99,
            names = FALSE
          ),
          length.out = 8
        ),
        labels = function(dfs) {
          if (diff(range(dfs)) < 1) {
            format(
              x = dfs,
              nsmall = 2,
              digits = 2
            )
          }
          else if (diff(range(dfs)) < 8) {
            format(
              x = dfs,
              nsmall = 1,
              digits = 1
            )
          }
          else {
            round(dfs)
          }
        },
        limits = c(2, NA),
        expand = expansion(mult = c(0,0))
      ) +
      scale_y_continuous(
        name = "Density",
        breaks = NULL,
        expand = expansion(mult = c(0, 0.01))
      ) +
      coord_cartesian(
        xlim = c(
          2,
          quantile(
            x = dfi_data$prior_sample,
            probs = 0.995,
            names = FALSE
          )
        )
      ) +
      theme_classic()
    
    # `dfA` prior density plot
    dfa_plot_obj <- ggplot() +
      geom_density(
        data = dfa_data,
        mapping = aes(
          x = prior_sample
        ),
        adjust = 2,
        bounds = c(2, Inf),
        color = "#605CA8",
        fill = "#28A745",
        alpha = 0.7
      ) +
      geom_vline(
        xintercept = c(2, input$hyper_dfa)
      ) +
      scale_x_continuous(
        name = expression(df[A]),
        #n.breaks = 6,
        breaks = seq(
          from = 2,
          to = quantile(
            x = dfa_data$prior_sample,
            probs = 0.99,
            names = FALSE
          ),
          length.out = 8
        ),
        labels = function(dfs) {
          if (diff(range(dfs)) < 1) {
            format(
              x = dfs,
              nsmall = 2,
              digits = 2
            )
          }
          else if (diff(range(dfs)) < 8) {
            format(
              x = dfs,
              nsmall = 1,
              digits = 1
            )
          }
          else {
            round(dfs)
          }
        },
        limits = c(2, NA),
        expand = expansion(mult = c(0,0))
      ) +
      scale_y_continuous(
        name = "Density",
        breaks = NULL,
        expand = expansion(mult = c(0, 0.01))
      ) +
      coord_cartesian(
        xlim = c(
          2,
          quantile(
            x = dfa_data$prior_sample,
            probs = 0.995,
            names = FALSE
          )
        )
      ) +
      theme_classic()
    
    gathered_prior_plots <- cowplot::plot_grid(
      plotlist = list(
        beta_plot_obj,
        dfi_plot_obj,
        dfa_plot_obj,
        cva_plot_obj,
        cvg_plot_obj,
        cvi_plot_obj,
        cvi_mean_plot_obj,
        cvi_sd_plot_obj,
        hbhr_plot_obj
      )
    )
    
    plot(gathered_prior_plots)
    
    
    
    
    
  }, res = 120)
  
}

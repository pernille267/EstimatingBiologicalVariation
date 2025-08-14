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

# SERVER LOGIC
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # 1. HELPER FUNCTIONS
  # --------------------------------------------------------------------------
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
  
  # New helper function to guess column mappings with expanded patterns
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
  
  # 2. COMPILE STAN MODELS ON STARTUP
  # --------------------------------------------------------------------------
  compiled_models <- reactiveValues()
  withProgress(message = 'Compiling Stan Models...', value = 0, {
    model_file_1 <- "stan_model_4.stan"
    model_file_2 <- "stan_model_3.stan"
    incProgress(0.1, detail = "Checking for model files...")
    if (!file.exists(model_file_1) || !file.exists(model_file_2)) {
      stop("One or both Stan model files not found.")
    }
    incProgress(0.2, detail = paste("Compiling", model_file_1))
    compiled_models$model1 <- stan_model(file = model_file_1)
    incProgress(0.5, detail = paste("Compiling", model_file_2))
    compiled_models$model2 <- stan_model(file = model_file_2)
    incProgress(1, detail = "Compilation complete.")
  })
  
  # 3. DATA HANDLING & DYNAMIC INPUTS
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
      print(uploaded_data_full()[[input$sheet_name]])
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
  
  # 4. DYNAMIC FILTER UI GENERATION
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
  
  # 5. FILTERED DATA
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
  
  output$filtered_data_table_model1 <- renderDT({
    req(filtered_data())
    DT::datatable(
      data = filtered_data(),
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(
        scolllX = TRUE,
        scrollY = "400px",
        pageLength = 50,
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
  output$filtered_data_table_model2 <- renderDT({
    req(filtered_data())
    DT::datatable(
      data = filtered_data(),
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(
        scolllX = TRUE,
        scrollY = "400px",
        pageLength = 50,
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
  
  # 6. ANALYSIS LOGIC
  # --------------------------------------------------------------------------
  run_analysis <- function(model_to_run) {
    
    # Requirements that needs to be satisfied before running is allowed
    req(
      model_to_run,
      filtered_data(),
      input$measurement_col,
      input$subject_id_col,
      input$sample_id_col,
      input$replicate_id_col
    )
    # Show notification that the sampling starts
    showNotification(
      ui = "Starting Stan analysis...",
      type = "message",
      duration = NULL,
      id = "stan_running"
    )
    # Copy data.table to avoid modifying the original data.table
    data_for_stan <- copy(filtered_data())
    # Standard names
    standard_names <- c(
      "y",
      "SubjectID",
      "SampleID",
      "ReplicateID"
    )
    # User names
    user_selected_cols <- c(
      input$measurement_col,
      input$subject_id_col,
      input$sample_id_col,
      input$replicate_id_col
    )
    # Change from user names to standard names
    setnames(
      data_for_stan,
      old = user_selected_cols,
      new = standard_names,
      skip_absent = TRUE
    )
    
    # Construct hyper_strength vector from weakness factor inputs
    hyper_strength_vec <- c(
      input$hyper_beta_weakness,
      input$hyper_cvi_weakness,
      input$hyper_cva_weakness,
      input$hyper_cvg_weakness,
      input$hyper_dfi_weakness,
      input$hyper_dfa_weakness
    )
    
    # Convert log_transformed input to logical
    log_trans_logical <- (input$log_transformed == "Yes")
    
    # Prepare the stan data
    prepared_stan_data <- pernille_prepares_for_stan_modeling(
      data = data_for_stan,
      hypers = list(
        "beta" = input$hyper_beta,
        "cvi" = input$hyper_cvi,
        "cva" = input$hyper_cva,
        "cvg" = input$hyper_cvg,
        "dfi" = input$hyper_dfi,
        "dfa" = input$hyper_dfa
      ),
      hyper_strength = hyper_strength_vec,
      log_transformed = log_trans_logical
    )
    
    # Calculate warmup iterations
    total_iter <- as.numeric(input$iter)
    warmup_iter <- floor(total_iter * (as.numeric(input$burn) / 100))
    
    # Do the sampling
    fit <- sampling(
      object = model_to_run,
      data = prepared_stan_data,
      chains = input$nchains,
      iter = total_iter,
      warmup = warmup_iter,
      thin = 1,
      seed = 123,
      control = list(
        adapt_delta = as.numeric(input$adapt_delta) / 100,
        max_treedepth = as.numeric(input$max_treedepth)
      )
    )
    
    # Remove notification that says stan is running
    removeNotification("stan_running")
    
    # Add notification that says stan sampling is complete
    showNotification(
      ui = "Analysis complete! Processing results...",
      type = "message",
      duration = 5
    )
    extracted_fit <- extract(fit)
    bayesian_output_table <- process_stan_output(
      fit = extracted_fit,
      log_transformed = log_trans_logical,
      analyte = paste0(
        input$analyte_name,
        " (",
        input$analyte_material,
        ")"
      ),
      group = input$group_name,
      data = data_for_stan
    )[[1]]
    
    bayesian_output_plot <- plot_subject_specific_CVI(
      fit = extracted_fit,
      log_transformed = log_trans_logical,
      analyte = paste0(
        input$analyte_name,
        " (",
        input$analyte_material,
        ")"
      ),
      group = input$group_name,
      data = data_for_stan
    )
    list(
      table = bayesian_output_table,
      plot = bayesian_output_plot
    )
  }
  
  analysis_results_model1 <- eventReactive(input$run_analysis_model1_btn, {
    run_analysis(compiled_models$model1)
  })
  analysis_results_model2 <- eventReactive(input$run_analysis_model2_btn, {
    run_analysis(compiled_models$model2)
  })
  
  # 7. RENDER OUTPUTS
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
  
  output$download_results_model1_btn <- downloadHandler(
    filename = function() {
      paste0(
        "biological_variation_estimation_model_ntt_",
        Sys.Date(),
        ".xlsx"
      ) 
    },
    content = function(file) {
      writexl::write_xlsx(
        x = analysis_results_model1()$table,
        path = file
      )
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
  
  output$download_results_model2_btn <- downloadHandler(
    filename = function() {
      paste0(
        "biological_variation_estimation_model_ntt_dfgamma_",
        Sys.Date(),
        ".xlsx"
      ) 
    },
    content = function(file) {
      writexl::write_xlsx(
        x = analysis_results_model2()$table,
        path = file,
        format_headers = TRUE
      )
    }
  )
}

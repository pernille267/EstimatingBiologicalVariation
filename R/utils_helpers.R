# Utility Helper Functions
# ------------------------------------------------------------------------------

# ==============================================================================
# Performance Timing Utility
# ==============================================================================
# Enable with:  options(bv.perf = TRUE)
# Disable with: options(bv.perf = FALSE)   (default)
#
# When enabled, every instrumented reactive block prints a message like:
#   [BV PERF] setup::map_data ............. 0.042s
# so you can see exactly what runs and how long it takes.
# ==============================================================================

#' Start a performance timer
#'
#' Returns a named list that \code{bv_timer_end()} uses to compute elapsed time.
#' If \code{getOption("bv.perf")} is not \code{TRUE}, returns \code{NULL}
#' (zero overhead).
#'
#' @param label A short label such as \code{"setup::map_data"}
#' @return A timer object (list) or \code{NULL}
bv_timer_start <- function(label) {
  if (!isTRUE(getOption("bv.perf"))) {
    return(invisible(NULL))
  }
  list(label = label, t0 = proc.time()[["elapsed"]])
}

#' End a performance timer and print elapsed time
#'
#' @param timer The object returned by \code{bv_timer_start()}.
#'   If \code{NULL} (perf disabled) this is a silent no-op.
#' @return Invisible elapsed seconds (or \code{NULL})
bv_timer_end <- function(timer) {
  if (is.null(timer)) {
    return(invisible(NULL))
  }
  elapsed <- proc.time()[["elapsed"]] - timer$t0
  pad <- max(1, 45 - nchar(timer$label))
  message(
    sprintf(
      "[BV PERF] %s %s %.3fs",
      timer$label, strrep(".", pad), elapsed
    )
  )
  invisible(elapsed)
}

#' Guess column mappings from data column names
#'
#' @param col_names Character vector of column names
#' @return Named list of guessed column mappings
guess_columns <- function(col_names) {
  guesses <- list()
  available_cols <- col_names

  # Define extensive patterns for each role in English and Norwegian
  patterns <- list(
    measurement_col = c(
      "result", "value", "y", "measure", "measurement",
      "concentration", "level", "reading", "output", "data",
      "observation", "score", "reading_value", "test_result", "response",
      "resultat", "verdi", "måling", "konsentrasjon", "nivå",
      "svar", "observasjon", "poeng", "måleverdi", "testresultat"
    ),
    subject_id_col = c(
      "subject", "subjid", "patient", "patid", "cpr",
      "id", "identifier", "participant", "subjectid", "person",
      "individual", "person_id", "participant_id",
      "subject_identifier", "unique_id",
      "person", "pasient", "deltaker", "forsøksperson", "cprnummer",
      "personnummer", "subjekt", "menneske", "hvem", "individ"
    ),
    sample_id_col = c(
      "sample", "sampid", "specimenid", "sampleid", "specimen_id",
      "sample_id", "visit", "time", "timepoint", "visitid",
      "specimen_number", "visit_number", "sample_name", "sample_code",
      "accession", "prøve", "prøveid", "prøvenummer", "besøk", "tidspunkt",
      "prøve_id", "prøve_nr", "besøk_nr", "prøvenavn", "prøvekode"
    ),
    replicate_id_col = c(
      "replicate", "replid", "rep", "duplicate", "run",
      "replicateid", "replicate_id", "measurement_no", "runid", "dublet",
      "technical_replicate", "bio_replicate", "repetition", "seq", "sequence",
      "replikat", "duplikat", "kjøring", "replikatid", "replikat_id",
      "måling_nr", "teknisk_replikat", "biologisk_replikat",
      "gjentakelse", "sekvens"
    ),
    analyte_col = c(
      "analyte", "analysis", "test", "component", "parameter",
      "assay", "analytename", "analyte_name", "testname", "analytt",
      "substance", "marker", "biomarker", "test_code", "test_name",
      "analyse", "komponent", "parameter", "analysenavn", "analyse_navn",
      "stoff", "markør", "biomarkør", "testkode", "testnavn"
    ),
    material_col = c(
      "material", "matrix", "specimen", "sampletype", "sample_type",
      "mat", "specimentype", "specimen_type", "sample_matrix", "prøvetype",
      "fluid", "tissue", "source", "origin", "sample_source",
      "materiale", "matrise", "prøvemateriale", "væske", "vev",
      "kilde", "opprinnelse", "prøvekilde"
    ),
    sex_col = c(
      "sex", "gender", "female", "male", "sex_id",
      "gender_id", "sex_code", "gender_code", "kjonn", "sexo",
      "M", "F", "K", "male_female", "is_male",
      "kjønn", "kvinne", "mann", "mannlig", "kvinnelig"
    ),
    group_1_col = c(
      "group", "pas", "patient.group", "culture", "region",
      "country", "status", "healthy", "population", "sick",
      "disease", "condition", "treatment", "cohort", "diagnosis",
      "grp", "gruppe1", "ethnicity", "location", "site",
      "arm", "study_arm", "intervention", "control", "case",
      "gruppe", "pasientgruppe", "etnisitet", "sykdom", "populasjon",
      "tilstand", "behandling", "kohort", "diagnose", "lokasjon",
      "sted", "studiearm", "intervensjon", "kontroll"
    ),
    group_2_col = c(
      "group", "pas", "patient.group", "culture", "region",
      "country", "status", "healthy", "population", "sick",
      "disease", "condition", "treatment", "cohort", "diagnosis",
      "grp", "gruppe2", "ethnicity", "location", "site",
      "arm", "study_arm", "intervention", "control", "case",
      "gruppe", "pasientgruppe", "etnisitet", "sykdom", "populasjon",
      "tilstand", "behandling", "kohort", "diagnose", "lokasjon",
      "sted", "studiearm", "intervensjon", "kontroll"
    )
  )

  # Function to find the first match for a set of patterns
  find_match <- function(patterns_list, cols) {
    for (p in patterns_list) {
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
    NULL
  }

  # Iterate through roles and find matches
  for (role in names(patterns)) {
    match <- find_match(patterns[[role]], available_cols)
    if (!is.null(match)) {
      guesses[[role]] <- match
      available_cols <- setdiff(available_cols, match) # Remove used column
    }
  }

  guesses
}

#' Resolve analyte name synonyms to canonical names
#'
#' @param input_value Character string with analyte name
#' @param synonym_map List mapping canonical names to synonyms
#' @return Canonical analyte name or NULL if not found
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

  NULL # Return NULL if no match is found
}

#' Parse Stan warnings and provide user-friendly advice
#'
#' @param warnings Character vector of warning messages from Stan
#' @param current_iter Current number of iterations
#' @param current_adapt_delta Current adapt_delta value
#' @param current_max_treedepth Current max_treedepth value
#' @return List of HTML tags with advice for each warning type
parse_stan_warnings <- function(warnings,
                                current_iter,
                                current_adapt_delta,
                                current_max_treedepth) {
  advice_list <- list()

  # Combine all captured warning messages into a single
  # string for easier searching
  all_warnings_text <- paste(warnings, collapse = " \n ")

  # 1. Check for R-hat warnings (Convergence)
  if (grepl("R-hat", all_warnings_text, ignore.case = TRUE)) {
    advice_list$rhat <- shiny::tags$li(
      shiny::tags$strong("Convergence Issue: High R-hat values detected."),
      paste0(
        "The model runs multiple independent processes ('chains') to find solutions, ",
        "but they have not agreed on a consistent result. This means the analysis ",
        "failed to find a stable solution, making the results unreliable."
      ),
      shiny::tags$strong("Primary Recommendation:"),
      paste0(
        "Give the model more time to find a stable solution by increasing ",
        "the number of iterations from ", current_iter, " to at least ", current_iter * 2, "."
      ),
      shiny::tags$strong("If the problem persists:"),
      paste0(
        "A persistent convergence issue can point to a problem with the ",
        "model itself. Consider simplifying the model or using stronger priors."
      )
    )
  }

  # 2. Check for Low Bulk ESS (Sample Quality)
  if (grepl("Bulk Effective Samples Size \\(ESS\\) is too low", all_warnings_text, ignore.case = TRUE)) {
    advice_list$ess_bulk <- shiny::tags$li(
      shiny::tags$strong("Poor Sample Quality: Low Bulk ESS."),
      paste0(
        "Effective Sample Size (ESS) estimates the amount of independent information in your samples. ",
        "A low value means the samples were not diverse enough to produce reliable estimates for the ",
        "center of the distribution (e.g., the mean or median)."
      ),
      shiny::tags$strong("Primary Recommendation:"),
      paste0(
        "Try to generate more independent information by increasing the number of ",
        "iterations from ", current_iter, " to at least ", current_iter * 2, "."
      ),
      shiny::tags$strong("If the problem persists:"),
      paste0(
        "Low ESS can be a symptom of a more serious issue. ",
        "Ensure that any Divergent Transition or R-hat warnings are resolved first, ",
        "as they can be the root cause."
      )
    )
  }

  # 3. Check for Low Tail ESS (Sample Quality)
  if (grepl("Tail Effective Samples Size \\(ESS\\) is too low", all_warnings_text, ignore.case = TRUE)) {
    advice_list$ess_tail <- shiny::tags$li(
      shiny::tags$strong("Poor Sample Quality: Low Tail ESS."),
      paste0(
        "Tail ESS measures sample quality for the extremes (the 'tails') of the distribution. ",
        "A low value means the model has not gathered enough information to build reliable ",
        "credible intervals or estimate extreme quantiles."
      ),
      shiny::tags$strong("Primary Recommendation:"),
      paste0(
        "To better explore the tails of the distribution, increase the number of ",
        "iterations from ", current_iter, " to at least ", current_iter * 2, "."
      ),
      shiny::tags$strong("If the problem persists:"),
      paste0(
        "This is often a symptom of a deeper model issue. Always resolve any ",
        "Divergent Transition or R-hat warnings first, as they are the likely root cause."
      )
    )
  }

  # 4. Check for Divergent Transitions (Model Specification / Sampling Difficulty)
  if (grepl("divergent transitions after warmup", all_warnings_text, ignore.case = TRUE)) {
    advice_list$divergences <- shiny::tags$li(
      shiny::tags$strong("Critical Issue: Divergent Transitions Detected!"),
      paste0(
        " This is a serious warning that the model is struggling to explore",
        " the full range of plausible solutions. The results are likely biased",
        " and should not be trusted. "
      ),
      shiny::tags$strong("Primary Recommendation:"),
      paste0(
        " Increase the 'Acceptance Probability' parameter from ", current_adapt_delta,
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
      shiny::tags$strong("If the problem persists:"),
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
    advice_list$treedepth <- shiny::tags$li(
      shiny::tags$strong("Efficiency Warning: Exceeded Maximum Treedepth."),
      paste0(
        "This is not an error that invalidates your results. It means the sampling ",
        "process was inefficient, requiring the maximum allowed number of steps for many ",
        "iterations, which significantly slowed down the analysis."
      ),
      shiny::tags$strong("Recommendation:"),
      shiny::tags$ol(
        shiny::tags$li(
          paste0(
            "First, ensure all Divergent Transition warnings are resolved,",
            " as they are a more critical issue and a potential root cause."
          )
        ),
        shiny::tags$li(
          "If no divergences are present, you can improve performance by",
          " increasing the 'Maximum Treedepth' parameter. ",
          ifelse(
            as.numeric(current_max_treedepth) < 12,
            paste0("For example, increase it from ", as.numeric(current_max_treedepth), " to 12. "),
            ifelse(
              as.numeric(current_max_treedepth) < 14,
              paste0("For example, increase it from ", as.numeric(current_max_treedepth), " to 14. "),
              ifelse(
                as.numeric(current_max_treedepth) < 16,
                paste0("For example, increase it from ", as.numeric(current_max_treedepth), " to 16. "),
                ifelse(
                  as.numeric(current_max_treedepth) < 20,
                  paste0("For example, increase it from ", as.numeric(current_max_treedepth), " to 16, 17, 18, 19 or 20. "),
                  paste0(
                    "But wait, you have already selected the maximum 'Maximum Treedepth' (20). ",
                    "Increasing it more than this have little value. "
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

# Analysis Utility Functions
# ------------------------------------------------------------------------------

#' Format a numeric value for display in BV tables
#'
#' Rounds to 2 decimal places. If the value is positive but less than 0.005
#' (i.e., would round to 0.00), it is displayed as "< 0.01" instead.
#'
#' @param x Numeric vector of values to format.
#' @return Character vector of formatted values.
#' @keywords internal
format_bv_value <- function(x) {
  vapply(x, function(val) {
    if (is.na(val)) {
      return(NA_character_)
    }
    if (val > 0 & val < 0.005) {
      return("< 0.01")
    }
    formatC(round(val, 2), format = "f", digits = 2)
  }, character(1))
}

#' Run Bayesian analysis using a compiled Stan model
#'
#' Shared utility function used by both NTT and NTTDFGAM model modules.
#' Prepares data, runs Stan MCMC sampling, and processes results.
#'
#' @param model Compiled Stan model object
#' @param model_name Character string for progress messages
#' @param app_state reactiveValues containing all analysis parameters
#' @return List containing results table, plots, diagnostics, and advice
run_bayesian_analysis <- function(model, model_name, app_state) {
  glassWithProgress(title = paste("Running", model_name, "..."), expr = {
    shiny::req(
      model,
      app_state$analysis_data,
      app_state$measurement_col,
      app_state$subject_id_col,
      app_state$sample_id_col,
      app_state$replicate_id_col
    )

    glassIncProgress(
      value = 0.1,
      detail = "Preparing data...",
      step = "Step 1/9"
    )

    # Prepare data for Stan
    data_for_stan <- data.table::copy(app_state$analysis_data)

    # Ensure ID columns are integer (Stan expects integer types)
    data_for_stan[, SubjectID := as.integer(SubjectID)]
    data_for_stan[, SampleID := as.integer(SampleID)]
    data_for_stan[, ReplicateID := as.integer(ReplicateID)]

    # Prepare hyperparameters
    hyper_strength_vec <- c(
      app_state$hyper_beta_weakness,
      app_state$hyper_cvi_weakness,
      app_state$hyper_cva_weakness,
      app_state$hyper_cvg_weakness,
      app_state$hyper_dfi_weakness,
      app_state$hyper_dfa_weakness,
      app_state$hyper_hbhr_weakness
    )

    prepared_stan_data <- pernille_prepares_for_stan_modeling(
      data = data_for_stan,
      hypers = list(
        "beta" = app_state$hyper_beta,
        "cvi" = app_state$hyper_cvi,
        "cva" = app_state$hyper_cva,
        "cvg" = app_state$hyper_cvg,
        "dfi" = app_state$hyper_dfi,
        "dfa" = app_state$hyper_dfa,
        "hbhr" = app_state$hyper_hbhr
      ),
      hyper_strength = hyper_strength_vec,
      log_transformed = app_state$log_transformed
    )

    glassIncProgress(
      value = 0.2,
      detail = "Setting up sampler...",
      step = "Step 2/9"
    )

    # Create starting values
    hypers <- process_stan_data_priors(
      beta = app_state$hyper_beta,
      cvi = app_state$hyper_cvi,
      cva = app_state$hyper_cva,
      cvg = app_state$hyper_cvg,
      dfi = app_state$hyper_dfi,
      dfa = app_state$hyper_dfa,
      hbhr = app_state$hyper_hbhr,
      log_transformed = app_state$log_transformed,
      strength = hyper_strength_vec
    )

    initfunc.stan <- function() hypers

    glassIncProgress(
      value = 0.3,
      detail = "Running MCMC sampler...",
      step = "Step 3/9"
    )

    # Calculate iteration parameters
    total_iter <- app_state$num_iterations
    warmup_iter <- floor(total_iter * app_state$burn_in_fraction)

    # Capture warnings
    captured_warnings <- list()

    # Run Stan sampling
    fit <- withCallingHandlers(
      expr = {
        rstan::sampling(
          object = model,
          data = prepared_stan_data,
          chains = app_state$num_chains,
          iter = total_iter,
          warmup = warmup_iter,
          init = initfunc.stan,
          thin = 1,
          seed = app_state$seed,
          cores = app_state$num_cores,
          control = list(
            adapt_delta = app_state$adapt_delta,
            max_treedepth = app_state$max_treedepth
          ),
          refresh = 100
        )
      },
      warning = function(w) {
        captured_warnings <<- c(captured_warnings, w$message)
      }
    )

    # Validate the stanfit object before proceeding
    if (!inherits(fit, "stanfit")) {
      stop(
        "Stan sampling returned an unexpected object of class '",
        paste(class(fit), collapse = ", "), "' instead of 'stanfit'. ",
        "This may indicate a version mismatch between the compiled model ",
        "and the installed rstan package."
      )
    }

    glassIncProgress(
      value = 0.70,
      detail = "Processing results...",
      step = "Step 4/9"
    )

    # Parse warnings
    parsed_advice <- parse_stan_warnings(
      warnings = captured_warnings,
      current_iter = total_iter,
      current_adapt_delta = app_state$adapt_delta * 100,
      current_max_treedepth = app_state$max_treedepth
    )

    glassIncProgress(
      value = 0.80,
      detail = "Extracting posteriors...",
      step = "Step 5/9"
    )

    # Extract results
    relevant_pars <- c(
      "beta", "df_I", "df_A",
      "sigma_A", "sigma_G", "sigma_I_pred",
      "sigma_I", "sigma_I_sd", "sigma_i",
      "G", "lp__"
    )

    trace_plot_obj <- rstan::traceplot(
      object = fit,
      pars = relevant_pars,
      inc_warmup = FALSE,
      unconstrain = FALSE
    )

    if (!inherits(trace_plot_obj, "gg")) {
      stop(
        "rstan::traceplot() returned an object of class '",
        paste(class(trace_plot_obj), collapse = ", "),
        "' instead of a ggplot."
      )
    }
    trace_fit <- trace_plot_obj$data

    trace_and_posterior_plotting_data <- calculate_trace_and_posterior_plotting_data( # nolint
      trace_fit = trace_fit,
      log_transformed = app_state$log_transformed
    )

    nonpermuted_extracted_fit <- rstan::extract(
      object = fit,
      pars = relevant_pars[relevant_pars != "lp__"],
      permute = FALSE,
      inc_warmup = FALSE
    )

    fit_summary <- rstan::summary(
      object = fit,
      pars = relevant_pars[relevant_pars != "lp__"]
    )
    if (!is.list(fit_summary) || is.null(fit_summary$summary)) {
      stop(
        "summary() on the stanfit object returned an unexpected result ",
        "(class: '", paste(class(fit_summary), collapse = ", "), "'). ",
        "Expected a list with a '$summary' matrix. The stanfit object may ",
        "be malformed."
      )
    }
    rhat_and_n_eff <- fit_summary$summary[, c("Rhat", "n_eff"), drop = FALSE]

    parameter_diagnostics <- calculate_parameter_diagnostics(
      nonpermuted_fit = nonpermuted_extracted_fit,
      rhat_and_n_eff = rhat_and_n_eff
    )

    glassIncProgress(
      value = 0.85,
      detail = "Building output tables...",
      step = "Step 6/9"
    )

    extracted_fit <- rstan::extract(fit)
    if (!is.list(extracted_fit)) {
      stop(
        "rstan::extract() returned an unexpected object of class '",
        paste(class(extracted_fit), collapse = ", "),
        "' instead of a named list. The stanfit object may be malformed ",
        "or there is a version mismatch."
      )
    }

    # Build priors list for table output
    priors_list <- list(
      beta = app_state$hyper_beta,
      cvi = app_state$hyper_cvi,
      cva = app_state$hyper_cva,
      cvg = app_state$hyper_cvg,
      dfi = app_state$hyper_dfi,
      dfa = app_state$hyper_dfa,
      hbhr = app_state$hyper_hbhr,
      beta_weakness = app_state$hyper_beta_weakness,
      cvi_weakness = app_state$hyper_cvi_weakness,
      cva_weakness = app_state$hyper_cva_weakness,
      cvg_weakness = app_state$hyper_cvg_weakness,
      dfi_weakness = app_state$hyper_dfi_weakness,
      dfa_weakness = app_state$hyper_dfa_weakness,
      hbhr_weakness = app_state$hyper_hbhr_weakness
    )

    # Build stan_options list for table output
    stan_options_list <- list(
      iterations = total_iter,
      warmup = warmup_iter,
      burn_fraction = app_state$burn_in_fraction,
      chains = app_state$num_chains,
      cores = app_state$num_cores,
      adapt_delta = app_state$adapt_delta,
      max_treedepth = app_state$max_treedepth,
      seed = app_state$seed
    )

    processed_stan_output <- process_stan_output(
      fit = extracted_fit,
      log_transformed = app_state$log_transformed,
      analyte = app_state$analyte_name,
      material = app_state$material_name,
      sex = app_state$sex_name,
      group = app_state$group_name,
      data = data_for_stan,
      priors = priors_list,
      stan_options = stan_options_list
    )

    glassIncProgress(
      value = 0.90,
      detail = "Creating plots...",
      step = "Step 7/9"
    )

    # Determine grouping aesthetics
    color_by_input <- NULL
    if (!is.null(app_state$sex_col) && app_state$sex_col != "") {
      if (!is.null(app_state$selected_sex) && length(app_state$selected_sex) >= 2) { # nolint
        color_by_input <- "Sex"
      }
    }

    glassIncProgress(
      value = 0.95,
      detail = "Extracting sampler diagnostics...",
      step = "Step 8/9"
    )

    # Extract sampler diagnostics for the overview panel
    sampler_params_list <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
    n_divergent <- sum(
      sapply(sampler_params_list, function(x) sum(x[, "divergent__"]))
    )
    total_post_warmup <- sum(sapply(sampler_params_list, nrow))
    n_max_td <- sum(sapply(sampler_params_list, function(x) {
      sum(x[, "treedepth__"] >= app_state$max_treedepth)
    }))

    # For sampler diagnostics display, exclude generated quantities
    # (sigma_I_pred, df_I, df_A, sigma_I) so that the reported max R-hat
    # matches Stan's own C++ diagnostic output. Generated quantities use
    # normal_rng() which can inflate R-hat dramatically.
    generated_qty_names <- c("sigma_I_pred", "df_I", "df_A", "sigma_I")
    diag_rows <- !rownames(rhat_and_n_eff) %in% generated_qty_names
    diag_rhat_and_n_eff <- rhat_and_n_eff[diag_rows, , drop = FALSE]

    rhats_vec <- diag_rhat_and_n_eff[, "Rhat"]
    n_effs_vec <- diag_rhat_and_n_eff[, "n_eff"]

    # Extract bulk and tail ESS from parameter_diagnostics
    diag_param_names <- parameter_diagnostics$parameter
    diag_param_rows <- !diag_param_names %in% generated_qty_names
    diag_params_dt <- parameter_diagnostics[diag_param_rows]
    low_bulk_ess_params <- diag_params_dt$parameter[
      !is.na(diag_params_dt$bulk_n_eff) & diag_params_dt$bulk_n_eff < 100
    ]
    low_tail_ess_params <- diag_params_dt$parameter[
      !is.na(diag_params_dt$tail_n_eff) & diag_params_dt$tail_n_eff < 100
    ]

    sampler_diagnostics <- list(
      n_divergent = n_divergent,
      total_post_warmup = total_post_warmup,
      divergent_pct = round(n_divergent / max(total_post_warmup, 1) * 100, 2),
      n_max_treedepth = n_max_td,
      treedepth_pct = round(n_max_td / max(total_post_warmup, 1) * 100, 2),
      max_rhat = round(max(rhats_vec, na.rm = TRUE), 4),
      chains_mixed = all(rhats_vec < 1.05, na.rm = TRUE),
      poor_rhat_params = rownames(diag_rhat_and_n_eff)[!is.na(rhats_vec) & rhats_vec >= 1.05],
      low_neff_params = rownames(diag_rhat_and_n_eff)[!is.na(n_effs_vec) & n_effs_vec < 100],
      low_bulk_ess_params = low_bulk_ess_params,
      low_tail_ess_params = low_tail_ess_params,
      n_chains = app_state$num_chains,
      n_iterations = total_iter,
      n_warmup = warmup_iter
    )

    glassIncProgress(0.98, detail = "Finalising...", step = "Step 9/9")

    # Build Stan Diagnostics table (Table 7) now that diagnostics are available
    low_bulk <- if (length(low_bulk_ess_params) > 0) "Yes" else "No"
    low_tail <- if (length(low_tail_ess_params) > 0) "Yes" else "No"
    mixed <- if (isTRUE(sampler_diagnostics$chains_mixed)) "Yes" else "No"
    processed_stan_output$Stan_Diagnostics <- data.table::data.table(
      "Measurand" = app_state$analyte_name,
      "Sex" = app_state$sex_name,
      "Material" = app_state$material_name,
      "Group" = app_state$group_name,
      "Divergent Transitions" = as.integer(n_divergent),
      "Exceed Maximum Treedepth" = as.integer(n_max_td),
      "Low Bulk Effective Sample Size" = low_bulk,
      "Low Tail Effective Sample Size" = low_tail,
      "Mixed" = mixed
    )

    # Return results
    list(
      table = processed_stan_output[[1]],
      results = processed_stan_output,
      additional_results = list(
        trace_and_posterior_plotting_data = trace_and_posterior_plotting_data,
        parameter_diagnostics = parameter_diagnostics
      ),
      sampler_diagnostics = sampler_diagnostics,
      advice = parsed_advice,
      raw_fit = fit
    )
  })
}


# ==============================================================================
# Descriptive Statistics & ANOVA Utilities (Data Exploration)
# ==============================================================================

#' Compute Descriptive Statistics from Analysis Data
#'
#' Returns grand, per-subject, and per-sample summary
#' tables.
#'
#' @param data A \code{data.table} with columns
#'   \code{y}, \code{SubjectID}, \code{SampleID}.
#' @param log_transformed Logical.  If \code{TRUE} the
#'   \code{y} values are log-transformed and
#'   CVs are derived via \code{\link{lognormal_sd_to_cv}}.
#' @return A named list with \code{grand_summary},
#'   \code{subject_summary}, and \code{sample_summary}.
#' @keywords internal
compute_descriptive_stats <- function(data, log_transformed = FALSE) {
  # Use data.table for efficient group-wise calculations
  SubjectID <- SampleID <- Mean <- SD_i <- NULL
  `CV_i (%)` <- y <- NULL

  data_copy <- data.table::copy(data)

  # -- Per-subject variance components via C++ backend ---------
  subject_levels <- as.integer(
    as.factor(data_copy$SubjectID)
  )
  sample_levels <- as.integer(
    as.factor(
      paste0(data_copy$SubjectID, "___", data_copy$SampleID)
    )
  )
  coded_data <- list(
    SubjectID = subject_levels,
    SampleID = sample_levels,
    y = if (log_transformed) {
      log(as.numeric(data_copy$y))
    } else {
      as.numeric(data_copy$y)
    }
  )

  grand_mean <- mean(coded_data$y, na.rm = TRUE)
  grand_median <- stats::median(coded_data$y, na.rm = TRUE)
  grand_sd <- sd(coded_data$y, na.rm = TRUE)
  grand_cv <- if (grand_mean != 0) {
    if (log_transformed) {
      lognormal_sd_to_cv(grand_sd)
    } else {
      grand_sd / grand_mean * 100
    }
  } else {
    NA_real_
  }
  grand_iqr <- stats::IQR(coded_data$y, na.rm = TRUE)
  grand_min <- min(coded_data$y, na.rm = TRUE)
  grand_max <- max(coded_data$y, na.rm = TRUE)

  n_observations <- nrow(data_copy)
  n_subjects <- data.table::uniqueN(data_copy$SubjectID)
  n_samples <- data.table::uniqueN(
    data_copy[, list(SubjectID, SampleID)]
  )
  avg_samples_per_subject <- if (n_subjects > 0) {
    n_samples / n_subjects
  } else {
    NA_real_
  }
  avg_replicates_per_sample <- if (n_samples > 0) {
    n_observations / n_samples
  } else {
    NA_real_
  }

  grand_summary <- data.table::data.table(
    Statistic = c(
      "Grand Mean",
      "Grand Median",
      "Overall SD",
      "Overall CV (%)",
      "Overall IQR",
      "Overall Minimum",
      "Overall Maximum",
      "N Observations",
      "N Subjects",
      "N Samples",
      "Average Samples per Subject",
      "Average Replicates per Sample"
    ),
    Value = c(
      format_bv_value(grand_mean),
      format_bv_value(grand_median),
      format_bv_value(grand_sd),
      format_bv_value(grand_cv),
      format_bv_value(grand_iqr),
      format_bv_value(grand_min),
      format_bv_value(grand_max),
      n_observations,
      n_subjects,
      n_samples,
      format_bv_value(avg_samples_per_subject),
      format_bv_value(avg_replicates_per_sample)
    )
  )

  # -- Per-subject variance components via C++ backend ---------
  vc_result <- tryCatch(
    variance_components(
      data = coded_data,
      output_type = "sigma",
      mult = 1.0,
      level = 0.95,
      cv_anova = FALSE
    ),
    error = function(e) NULL
  )

  subject_summary <- data_copy[
    ,
    list(
      Mean = mean(y, na.rm = TRUE),
      SD = format_bv_value(sd(y, na.rm = TRUE)),
      `SD_i` = format_bv_value(if (!is.null(vc_result)) {
        vc_result$sigma_i[1]
      } else {
        NA_real_
      }),
      `CV (%)` = format_bv_value(
        if (log_transformed) {
          lognormal_sd_to_cv(sd(y, na.rm = TRUE))
        } else {
          sd(y, na.rm = TRUE) /
            mean(y, na.rm = TRUE) * 100
        }
      ),
      `CV_i (%)` = format_bv_value(if (!is.null(vc_result)) {
        if (log_transformed) {
          lognormal_sd_to_cv(vc_result$sigma_i[1])
        } else {
          vc_result$sigma_i[1] /
            mean(y, na.rm = TRUE) * 100
        }
      } else {
        NA_real_
      }),
      `N Observations` = .N,
      `N Samples` = data.table::uniqueN(SampleID),
      `Average Replicates per Sample` = if (.N > 0) {
        .N / data.table::uniqueN(SampleID)
      } else {
        NA_real_
      }
    ),
    by = SubjectID
  ]
  data.table::setorder(subject_summary, SubjectID)

  # With their correct positions secured
  # Fill in SD_i and CV_i (%) from variance components if available
  if (!is.null(vc_result) &&
    length(vc_result$sigma_i) == nrow(subject_summary)) {
    subject_summary[, `SD_i` := format_bv_value(vc_result$sigma_i)]
    subject_summary[, `CV_i (%)` := format_bv_value(
      if (log_transformed) {
        lognormal_sd_to_cv(vc_result$sigma_i)
      } else {
        vc_result$sigma_i / Mean * 100
      }
    )]
    subject_summary[, Mean := format_bv_value(Mean)]
  }

  list(
    grand_summary = grand_summary,
    subject_summary = subject_summary
  )
}


#' Compute Nested ANOVA Variance Component Estimates
#'
#' Computes CV estimates and Burdick--Graybill confidence
#' intervals from a nested (subjects / samples / replicates)
#' ANOVA design.  Three estimation paths are supported:
#'
#' \describe{
#'   \item{Standard (default)}{
#'     \code{log_transformed = FALSE, cv_anova = FALSE}.
#'     CVs and CIs are obtained directly from
#'     \code{\link{variance_components}} with
#'     \code{output_type = "cv_ci"}.
#'   }
#'   \item{Log-transformed data}{
#'     \code{log_transformed = TRUE}.
#'     Variance components are estimated on the log scale
#'     via \code{output_type = "sigma_ci"}, then converted
#'     to identity-scale CVs (%) with
#'     \code{\link{lognormal_sd_to_cv}}.  The grand mean is
#'     back-transformed as \code{exp(beta_log)}.
#'     \code{cv_anova} is ignored (forced \code{FALSE}).
#'   }
#'   \item{CV-ANOVA (identity scale only)}{
#'     \code{log_transformed = FALSE, cv_anova = TRUE}.
#'     The ANOVA is run on subject-mean-scaled values so
#'     that CIs apply directly to CV_A and CV_I.
#'     Because scaling removes between-subject variation,
#'     CV_G and its CI are taken from the standard
#'     (non-CV-ANOVA) approach instead.
#'   }
#' }
#'
#' Variance-component point estimates and
#' Burdick--Graybill confidence intervals are computed
#' by \code{\link{variance_components}}.  The ANOVA
#' table (df, SS, MS) is obtained from
#' \code{\link{bv_anova}}.
#'
#' @param data A \code{data.table} with columns
#'   \code{y}, \code{SubjectID}, \code{SampleID},
#'   \code{ReplicateID}.
#' @param log_transformed Logical.  If \code{TRUE} the
#'   \code{y} values are assumed to be log-transformed and
#'   CVs are derived via \code{\link{lognormal_sd_to_cv}}.
#' @param cv_anova Logical.  If \code{TRUE} and
#'   \code{log_transformed} is \code{FALSE}, the CV-ANOVA
#'   approach is used for CV_A and CV_I (CIs computed
#'   directly on CVs).  Ignored when
#'   \code{log_transformed = TRUE}.
#' @return A named list with \code{cv_table},
#'   \code{anova_table}, and scalar estimates.
#' @keywords internal
compute_anova_estimates <- function(data,
                                    log_transformed = FALSE,
                                    cv_anova = FALSE) {
  data_copy <- data.table::copy(data)
  n_observations <- nrow(data_copy)

  # -- Integer-coded data for C++ back-end -----
  subject_levels <- as.integer(
    as.factor(data_copy$SubjectID)
  )
  sample_levels <- as.integer(
    as.factor(
      paste0(
        data_copy$SubjectID,
        "___",
        data_copy$SampleID
      )
    )
  )
  coded_data <- list(
    SubjectID = subject_levels,
    SampleID = sample_levels,
    y = if (isTRUE(log_transformed)) {
      log(as.numeric(data_copy$y))
    } else {
      as.numeric(data_copy$y)
    }
  )

  # -- Validate parameter combinations --------
  if (log_transformed && cv_anova) {
    message(
      "[ANOVA] cv_anova is ignored for log-transformed ",
      "data; using cv_anova on identity scale data."
    )
    cv_anova <- TRUE
    # Transform back to identity scale for variance_components() since
    # cv_anova is only relevant for identity-scale CV estimation.
    coded_data$y <- exp(coded_data$y)
    log_transformed <- FALSE
  }

  # -- Variance components (point + CI) --------
  cva <- NA_real_
  cvi <- NA_real_
  cvg <- NA_real_
  cva_lower <- cva_upper <- NA_real_
  cvi_lower <- cvi_upper <- NA_real_
  cvg_lower <- cvg_upper <- NA_real_
  sda <- sdi <- sdg <- NA_real_
  beta_hat <- mean(data_copy$y, na.rm = TRUE)

  tryCatch(
    {
      if (log_transformed) {
        # === Log-transformed path ===
        # Get SDs with CIs on the log scale
        vc_result <- variance_components(
          data = coded_data,
          output_type = "sigma_ci",
          mult = 1,
          level = 0.95,
          cv_anova = FALSE
        )

        # Convert log-scale SDs → identity-scale CVs (%)
        # lognormal_sd_to_cv is monotonically increasing,
        # so CI bounds transform correctly.
        cva <- lognormal_sd_to_cv(vc_result$sigma_A[1])
        cva_lower <- round(
          x = lognormal_sd_to_cv(vc_result$sigma_A[2]),
          digits = 2
        )
        cva_upper <- round(
          x = lognormal_sd_to_cv(vc_result$sigma_A[3]),
          digits = 2
        )

        cvi <- lognormal_sd_to_cv(vc_result$sigma_I[1])
        cvi_lower <- round(
          x = lognormal_sd_to_cv(vc_result$sigma_I[2]),
          digits = 2
        )
        cvi_upper <- round(
          x = lognormal_sd_to_cv(vc_result$sigma_I[3]),
          digits = 2
        )

        cvg <- lognormal_sd_to_cv(vc_result$sigma_G[1])
        cvg_lower <- round(
          x = lognormal_sd_to_cv(vc_result$sigma_G[2]),
          digits = 2
        )
        cvg_upper <- round(
          x = lognormal_sd_to_cv(vc_result$sigma_G[3]),
          digits = 2
        )

        # Back-transform grand mean to identity scale
        beta_hat <- exp(vc_result$beta)
      } else if (cv_anova) {
        # === CV-ANOVA path (identity scale) ===
        # CV_A and CV_I via CV-ANOVA (direct CIs on CVs)
        vc_cv_anova <- variance_components(
          data = coded_data,
          output_type = "cv_ci",
          mult = 100,
          level = 0.95,
          cv_anova = TRUE
        )

        # CV_G from standard approach — CV-ANOVA removes
        # between-subject variation so CV_G is invalid there
        vc_standard <- variance_components(
          data = coded_data,
          output_type = "cv_ci",
          mult = 100,
          level = 0.95,
          cv_anova = FALSE
        )

        # Use the standard grand mean (cv_anova sets it to 1)
        beta_hat <- vc_standard$beta

        cva <- vc_cv_anova$sigma_A[1]
        cva_lower <- round(vc_cv_anova$sigma_A[2], 2)
        cva_upper <- round(vc_cv_anova$sigma_A[3], 2)

        cvi <- vc_cv_anova$sigma_I[1]
        cvi_lower <- round(vc_cv_anova$sigma_I[2], 2)
        cvi_upper <- round(vc_cv_anova$sigma_I[3], 2)

        # CV_G from standard (non-CV-ANOVA) approach
        cvg <- vc_standard$sigma_G[1]
        cvg_lower <- round(vc_standard$sigma_G[2], 2)
        cvg_upper <- round(vc_standard$sigma_G[3], 2)
      } else {
        # === Standard path (original behaviour) ===
        vc_result <- variance_components(
          data = coded_data,
          output_type = "cv_ci",
          mult = 100,
          level = 0.95,
          cv_anova = FALSE
        )
        beta_hat <- vc_result$beta

        cva <- vc_result$sigma_A[1]
        cva_lower <- round(vc_result$sigma_A[2], 2)
        cva_upper <- round(vc_result$sigma_A[3], 2)

        cvi <- vc_result$sigma_I[1]
        cvi_lower <- round(vc_result$sigma_I[2], 2)
        cvi_upper <- round(vc_result$sigma_I[3], 2)

        cvg <- vc_result$sigma_G[1]
        cvg_lower <- round(vc_result$sigma_G[2], 2)
        cvg_upper <- round(vc_result$sigma_G[3], 2)
      }

      # Derive SDs from CV estimates
      sda <- cva / 100 * beta_hat
      sdi <- cvi / 100 * beta_hat
      sdg <- cvg / 100 * beta_hat
    },
    error = function(e) {
      message(
        "[ANOVA] variance_components failed: ",
        e$message
      )
    }
  )

  # -- ANOVA table via bv_anova() (C++) --------
  anova_fit <- bv_anova(coded_data)

  # -- Degrees of freedom from ANOVA fit -------
  dfg_anova <- anova_fit$n1
  dfi_anova <- anova_fit$n2
  dfa_anova <- anova_fit$n3

  # -- Mean squares from ANOVA fit (for SS calculations) --
  msg_anova <- 0
  msi_anova <- 0
  msa_anova <- 0
  msg_anova <- if (!is.na(anova_fit$S1U_squared)) {
    anova_fit$S1U_squared
  }
  msi_anova <- if (!is.na(anova_fit$S2U_squared)) {
    anova_fit$S2U_squared
  }
  msa_anova <- if (!is.na(anova_fit$S3_squared)) {
    anova_fit$S3_squared
  }

  # --- Sum of Squares -------------------------
  ssg_anova <- msg_anova * dfg_anova
  ssi_anova <- msi_anova * dfi_anova
  ssa_anova <- msa_anova * dfa_anova
  ss_total <- ssg_anova + ssi_anova + ssa_anova

  # -- Build output tables ---------------------

  # Build CV table with estimates, CIs, and variance/SD for each component
  cv_table <- data.table::data.table(
    "Component" = c(
      "\\(\\mathrm{CV}_{\\mathrm{A}} (\\%)\\)",
      "\\(\\mathrm{CV}_{\\mathrm{I}} (\\%)\\)",
      "\\(\\mathrm{CV}_{\\mathrm{G}} (\\%)\\)"
    ),
    `Estimate (%)` = format_bv_value(c(cva, cvi, cvg)),
    `Lower CI 95%` = format_bv_value(c(cva_lower, cvi_lower, cvg_lower)),
    `Upper CI 95%` = format_bv_value(c(cva_upper, cvi_upper, cvg_upper))
  )


  df_total <- n_observations - 1
  ms_total <- ss_total / max(1, df_total)

  # Build ANOVA table with df, SS, MS for each source and total
  anova_table <- data.table::data.table(
    Source = c(
      "Between Subjects BV",
      "Within Subjects BV",
      "Analytical Variation",
      "Total BV"
    ),
    df = c(dfg_anova, dfi_anova, dfa_anova, df_total),
    SS = format_bv_value(c(ssg_anova, ssi_anova, ssa_anova, ss_total)),
    MS = format_bv_value(c(msg_anova, msi_anova, msa_anova, ms_total))
  )

  list(
    cv_table = cv_table,
    anova_table = anova_table,
    CV_A = cva,
    CV_I = cvi,
    CV_G = cvg,
    CV_A_lower = cva_lower,
    CV_A_upper = cva_upper,
    CV_I_lower = cvi_lower,
    CV_I_upper = cvi_upper,
    CV_G_lower = cvg_lower,
    CV_G_upper = cvg_upper,
    grand_mean = beta_hat
  )
}

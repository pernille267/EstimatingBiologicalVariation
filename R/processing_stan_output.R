#' Format a credible interval from a vector of three values.
#' @param stats_vec A numeric vector of length 3:
#'   c(point_estimate, lower, upper).
#' @param digits The number of decimal places to format to.
#' @return A formatted string like "point_est (lower-upper)".
format_ci <- function(stats_vec, digits) {
  if (!is.numeric(stats_vec) || length(stats_vec) != 3) {
    # Return NA for invalid input to avoid crashing the whole summary
    return(NA_character_)
  }

  format_string <- paste0("%.", as.integer(digits), "f")

  sprintf(
    "%s (%s-%s)",
    sprintf(format_string, stats_vec[1]),
    sprintf(format_string, stats_vec[2]),
    sprintf(format_string, stats_vec[3])
  )
}

# ============================================================================
# Shared Statistical Transformation Helpers
# ============================================================================

#' Convert a log-Student-t scale parameter to a CV (%).
#'
#' For a log-t distributed variable, the variance is equivalent to a
#' log-normal with \code{sd = sqrt(scale^2 * df / (df - 2))}.
#'
#' @param scale Numeric vector of scale parameters.
#' @param df Numeric vector of degrees-of-freedom parameters.
#' @return Numeric vector of CVs in percent.
#' @keywords internal
logt_scale_to_cv <- function(scale, df) {
  sqrt(exp(scale^2 * df / (df - 2)) - 1) * 100
}

#' Convert a log-normal standard deviation to a CV (%).
#'
#' @param log_sd Numeric vector of log-scale standard deviations.
#' @return Numeric vector of CVs in percent.
#' @keywords internal
lognormal_sd_to_cv <- function(log_sd) {
  sqrt(exp(log_sd^2) - 1) * 100
}

#' Convert a Student-t scale parameter to a standard deviation.
#'
#' @param scale Numeric vector of scale parameters.
#' @param df Numeric vector of degrees-of-freedom parameters.
#' @return Numeric vector of standard deviations.
#' @keywords internal
t_scale_to_sd <- function(scale, df) {
  scale * sqrt(df / (df - 2))
}

#' Calculate a coefficient of variation (%) from SD and mean.
#'
#' @param sd Numeric vector of standard deviations.
#' @param mean Numeric vector of means.
#' @return Numeric vector of CVs in percent.
#' @keywords internal
calculate_cv <- function(sd, mean) {
  (sd / mean) * 100
}

#' Calculate Trace Plot and Posterior Distribution Plotting data
#' @param trace_fit A \code{data.frame} generated from
#'   \code{rstan::traceplot(...)$data}.
#' @param log_transformed A \code{logical} value. Set to
#'   \code{TRUE} if the multiplicative model is utilized.
#' @return
#' A \code{list} object containing three \code{data.table} objects:
#' \itemize{
#'    \item \code{univariate_parameters}: Draws for single
#'      parameters (beta, dfs, CVs).
#'    \item \code{homeostatic_set_points}: Draws for
#'      subject-specific means (mu_p(i)).
#'    \item \code{subject_specific_cvs}: Draws for
#'      subject-specific CVs (CV_p(i)).
#' }
calculate_trace_and_posterior_plotting_data <- function(trace_fit, # nolint
                                                        log_transformed) {
  # Convert from data.frame to data.table
  data.table::setDT(trace_fit)

  # Fix global binding variables for data.table
  chain <- iteration <- value <- parameter <- NULL

  # --- Local helper: build a standardised trace data.table ---
  make_trace_dt <- function(src, value_expr, param_name, param_label) {
    data.table::copy(src)[, list(
      value = value_expr,
      parameter = param_name,
      parameter_label = param_label,
      chain = chain,
      chain_label = paste0("Chain[", chain, "]"),
      iteration = iteration
    )]
  }

  # Extract the relevant individual components
  beta_data <- trace_fit[parameter == "beta"]
  dfi_data <- trace_fit[parameter == "df_I"]
  dfa_data <- trace_fit[parameter == "df_A"]
  siga_data <- trace_fit[parameter == "sigma_A"]
  sigg_data <- trace_fit[parameter == "sigma_G"]
  sigi_pred_data <- trace_fit[parameter == "sigma_I_pred"]
  sigi_mean_data <- trace_fit[parameter == "sigma_I"]
  sigi_sd_data <- trace_fit[parameter == "sigma_I_sd"]
  g_data <- trace_fit[grepl("G\\[", parameter, ignore.case = FALSE)]
  sigpi_data <- trace_fit[grepl("sigma_i\\[", parameter, ignore.case = FALSE)]

  dfi_vector <- dfi_data$value
  dfa_vector <- dfa_data$value
  beta_pred_vector <- beta_data$value
  n_subj <- length(unique(sigpi_data$parameter))

  # --- Transform parameters depending on model scale ---
  if (log_transformed) {
    beta_data <- make_trace_dt(beta_data, exp(beta_data$value), "beta", "beta")
    dfi_data <- make_trace_dt(dfi_data, dfi_data$value, "df_I", "df[I]")
    dfa_data <- make_trace_dt(dfa_data, dfa_data$value, "df_A", "df[A]")
    cva_data <- make_trace_dt(
      siga_data,
      logt_scale_to_cv(siga_data$value, dfa_vector),
      "cv_A",
      "CV[A]"
    )
    cvg_data <- make_trace_dt(
      sigg_data,
      lognormal_sd_to_cv(sigg_data$value),
      "cv_G",
      "CV[G]"
    )
    cvi_pred_data <- make_trace_dt( # nolint
      sigi_pred_data,
      lognormal_sd_to_cv(sigi_pred_data$value),
      "cv_I_pred",
      "dCV[P(i)]"
    )
    cvi_mean_data <- make_trace_dt( # nolint
      sigi_mean_data,
      logt_scale_to_cv(sigi_mean_data$value, dfi_vector),
      "cv_I",
      "E(CV[I])"
    )
    cvi_sd_data <- make_trace_dt( # nolint
      sigi_sd_data,
      logt_scale_to_cv(sigi_sd_data$value, dfi_vector),
      "cv_I_sd",
      "SD(CV[I])"
    )

    cvpi_data <- data.table::copy(sigpi_data)[, list(
      value = logt_scale_to_cv(
        value,
        rep(dfi_vector, n_subj)
      ),
      parameter = gsub("sigma_i\\[(\\d+)\\]", "CV_p(\\1)", parameter),
      parameter_label = gsub("sigma_i\\[(\\d+)\\]", "CV[p(\\1)]", parameter),
      chain = chain,
      chain_label = paste0("Chain[", chain, "]"),
      iteration = iteration
    )]
    mupi_data <- data.table::copy(g_data)[, list(
      value = exp(value + rep(beta_pred_vector, n_subj)),
      parameter = gsub("G\\[(\\d+)\\]", "mu_p(\\1)", parameter),
      parameter_label = gsub("G\\[(\\d+)\\]", "mu[p(\\1)]", parameter),
      chain = chain,
      chain_label = paste0("Chain[", chain, "]"),
      iteration = iteration
    )]
  } else {
    beta_data <- make_trace_dt(beta_data, beta_data$value, "beta", "beta")
    dfi_data <- make_trace_dt(dfi_data, dfi_data$value, "df_I", "df[I]")
    dfa_data <- make_trace_dt(dfa_data, dfa_data$value, "dfa_vector", "df[A]")
    cva_data <- make_trace_dt(
      src = siga_data,
      value_expr = calculate_cv(
        sd = t_scale_to_sd(siga_data$value, dfa_vector),
        mean = beta_pred_vector
      ),
      param_name = "cv_A",
      param_label = "CV[A]"
    )
    cvg_data <- make_trace_dt(
      sigg_data,
      calculate_cv(sigg_data$value, beta_pred_vector),
      "cv_G",
      "CV[G]"
    )
    cvi_pred_data <- make_trace_dt(
      sigi_pred_data,
      calculate_cv(sigi_pred_data$value, beta_pred_vector),
      "cv_I_pred",
      "dCV[P(i)]"
    )
    cvi_mean_data <- make_trace_dt(
      sigi_mean_data,
      calculate_cv(
        t_scale_to_sd(sigi_mean_data$value, dfi_vector),
        beta_pred_vector
      ),
      "cv_I",
      "E(CV[I])"
    )
    cvi_sd_data <- make_trace_dt(
      sigi_sd_data,
      calculate_cv(
        t_scale_to_sd(sigi_sd_data$value, dfi_vector),
        beta_pred_vector
      ),
      "cv_I_sd",
      "SD(CV[I])"
    )

    mu_pi_vec <- g_data$value + rep(beta_pred_vector, n_subj)

    cvpi_data <- data.table::copy(sigpi_data)[, list(
      value = calculate_cv(
        t_scale_to_sd(value, rep(dfi_vector, n_subj)),
        mu_pi_vec
      ),
      parameter = gsub("sigma_i\\[(\\d+)\\]", "CV_p(\\1)", parameter),
      parameter_label = gsub("sigma_i\\[(\\d+)\\]", "CV[p(\\1)]", parameter),
      chain = chain,
      chain_label = paste0("Chain[", chain, "]"),
      iteration = iteration
    )]
    mupi_data <- data.table::copy(g_data)[, list(
      value = value + rep(beta_pred_vector, n_subj),
      parameter = gsub("G\\[(\\d+)\\]", "mu_p(\\1)", parameter),
      parameter_label = gsub("G\\[(\\d+)\\]", "mu[p(\\1)]", parameter),
      chain = chain,
      chain_label = paste0("Chain[", chain, "]"),
      iteration = iteration
    )]
  }

  plotting_data_univariate <- data.table::rbindlist(
    l = list(
      beta_data,
      "df_I_data" = dfi_data,
      "df_A_data" = dfa_data,
      "cv_A_data" = cva_data,
      "cv_G_data" = cvg_data,
      "cv_I_pred_data" = cvi_pred_data,
      "cv_I_mean_data" = cvi_mean_data,
      "cv_I_sd_data" = cvi_sd_data
    ),
    use.names = FALSE,
    fill = TRUE,
    idcol = NULL
  )

  output <- list(
    "univariate_parameters" = plotting_data_univariate,
    "homeostatic_set_points" = mupi_data,
    "subject_specific_cvs" = cvpi_data
  )

  output
}

#' Calculate and Merge Advanced MCMC Diagnostics
#'
#' @description
#' This function calculates bulk and tail Effective Sample Sizes (ESS) from a
#' non-permuted MCMC simulations array (e.g., from \code{rstan::extract}) and
#' merges them with pre-calculated R-hat and mean ESS values (e.g., from
#' \code{rstan::summary}).
#'
#' The function is written to be robust, performing several critical checks:
#' \enumerate{
#'  \item Validates input types (array, matrix).
#'  \item Ensures column names (\code{Rhat}, \code{n_eff}) exist in
#'        \code{rhat_and_n_eff}.
#'  \item Ensures parameter names match exactly between both inputs.
#'  \item Ensures parameter order matches exactly between both inputs.
#' }
#'
#' @details
#' This function is written for a *specific* set of Bayesian models, namely
#' the NTT (or NNN, NTN, or NNT) and NTTDFGAM biological variation models.
#'
#' It relies on the \code{rstan} package for \code{ess_bulk()} and
#' \code{ess_tail()}.
#'
#' This function is not intended to be used by end-users. It is intended to be
#' used as a helper function for the package developer, in her biological
#' variation application.
#'
#' @param nonpermuted_fit An array of MCMC simulations, the output of
#'   \code{rstan::extract(..., permuted = FALSE)}. The dimension names should
#'   be \code{iterations}, \code{chains}, and \code{parameters}.
#' @param rhat_and_n_eff A matrix containing the R-hat and n_eff values for
#'   each parameter. This is typically the output of
#'   \code{rstan::summary(...)$summary} subsetted to the \code{Rhat} and
#'   \code{n_eff} columns. It must have rownames corresponding to parameter
#'   names. It must have columns named \code{Rhat} and \code{n_eff}.
#'
#' @return
#' A \code{data.table} with columns: \code{parameter}, \code{Rhat},
#' \code{n_eff}, \code{bulk_n_eff}, and \code{tail_n_eff}. \code{n_eff} is the
#' original mean effective sample sizes, while the bulk/tail
#' effective sample sizes are newly calculated.
#'
#' @importFrom data.table :=
#' @examples
#' \dontrun{
#' # This example requires a 'stanfit' object 'my_stan_fit' and the
#' # 'rstan', 'posterior', and 'data.table' packages.
#'
#' # 1. Load required libraries
#' library(biovar)
#'
#' # 2. Extract non-permuted draws
#' np_fit <- rstan::extract(object = my_stan_fit, permuted = FALSE)
#'
#' # 3. Get Rhat and n_eff summary
#' fit_summary <- rstan::summary(my_stan_fit)$summary
#' rhat_n_eff_matrix <- fit_summary[, c("Rhat", "n_eff"), drop = FALSE]
#'
#' # 4. Calculate all diagnostics
#' all_diags <- calculate_parameter_diagnostics(
#'   nonpermuted_fit = np_fit,
#'   rhat_and_n_eff = rhat_n_eff_matrix
#' )
#'
#' print(all_diags)
#' }
#'
calculate_parameter_diagnostics <- function(nonpermuted_fit, rhat_and_n_eff) { # nolint

  # --- Fix global binding variables for data.table ---
  Rhat <- n_eff <- NULL # nolint

  # --- Ensure `nonpermutedfit` is an array ---
  if (!is.array(nonpermuted_fit)) {
    stop(
      "The input `nonpermuted_fit` is not an array, but is of class `",
      class(nonpermuted_fit)[1],
      "`. Make sure that you have passed the nonpermuted output of ",
      "rstan::extract(..., permute = FALSE)"
    )
  }

  # --- Ensure `rhat_and_n_eff` is a matrix ---
  if (!is.matrix(rhat_and_n_eff)) {
    stop(
      "The input `rhat_and_n_eff` is expected to be a matrix, but is of class ",
      class(rhat_and_n_eff)[1],
      ". Ensure that it is a matrix with two columns (`Rhat` and `n_eff`)."
    )
  }

  # Validate that `rhat_and_n_eff` has the required columns
  expected_cols <- c("Rhat", "n_eff")
  if (!all(expected_cols %in% colnames(rhat_and_n_eff))) {
    stop(
      "The input `rhat_and_n_eff` is missing required columns. ",
      "Expected columns 'Rhat' and 'n_eff'. Found: ",
      paste(colnames(rhat_and_n_eff), collapse = ", ")
    )
  }

  # --- Get parameter names and check if the required names are there ---
  fit_names <- colnames(nonpermuted_fit[, 1, ])

  # Check if the parameters in `nonpermuted_fit` matches
  # those in `Rhat_and_n_eff`
  rhat_and_neff_names <- rownames(rhat_and_n_eff) # nolint

  # Check if both have the same names (order ignored)
  names_match_1 <- fit_names %in% rhat_and_neff_names
  names_match_2 <- rhat_and_neff_names %in% fit_names

  # Check for parameters of `fit_names` not in `rhat_and_neff_names`
  if (sum(!names_match_1) >= 1) {
    stop(
      "Some parameters that is part of `nonpermuted_fit` were not found in ",
      "`rhat_and_n_eff`. These are: ",
      paste(
        fit_names[!names_match_1],
        collapse = ", "
      ),
      "."
    )
  }
  # Check which `rhat_and_neff_names` parameters are not in `fit_names`
  if (sum(!names_match_2) >= 1) {
    stop(
      "Some parameters that is part of `rhat_and_n_eff` were not found in ",
      "`nonpermuted_fit`. These are: ",
      paste(
        rhat_and_neff_names[!names_match_2],
        collapse = ", "
      ),
      "."
    )
  }

  # Check order of parameters.
  if (!all(fit_names == rhat_and_neff_names)) {
    # Re-order rhat_and_n_eff to match nonpermuted_fit
    message(
      "Re-ordering `rhat_and_n_eff` rows to match ",
      "`nonpermuted_fit` parameter order."
    )
    rhat_and_n_eff <- rhat_and_n_eff[fit_names, , drop = FALSE]
  }

  # Check for specific expected parameters (univariate case)
  expected_univ_par_names <- c(
    "beta", "df_I", "df_A",
    "sigma_A", "sigma_G", "sigma_I_pred",
    "sigma_I", "sigma_I_sd"
  )

  # Found and missing parameters (univariate case)
  found <- expected_univ_par_names %in% fit_names
  missing_parameters <- NULL
  if (sum(!found) >= 1) {
    missing_parameters <- expected_univ_par_names[!found]
  }

  # Throw an error if there are missing parameters
  if (!is.null(missing_parameters)) {
    stop(
      "These parameters were expected to be found in `nonpermuted_fit` and ",
      "`Rhat_and_n_eff`, but were not found: ",
      paste0(
        missing_parameters
      )
    )
  }

  # Check for sigma_i[1], ..., sigma_i[n]
  # Number of matches with substring `sigma_i[`
  found <- grepl(
    pattern = "sigma_i\\[",
    x = fit_names,
    ignore.case = FALSE
  )

  # Expects at least one match. Typically there will be more matches.
  if (!any(found)) {
    stop(
      "The sigma_i[k] parameters (within-subject biological variation) ",
      "were expected to be found. However, none was found."
    )
  }

  # 3. Check for G[1], ..., G[n]
  # ----------------------------------------------------------------------------

  # Potential "issue": Same as for 2.

  # Number of matches with substring `G[`
  found <- grepl(
    pattern = "G\\[",
    x = fit_names,
    ignore.case = FALSE
  )

  # Expects at least one match. Typically there will be more matches.
  if (!any(found)) {
    stop(
      "The G[k] parameters (subject effects `G_k` of the model) ",
      "were expected to be found. However, none was found."
    )
  }

  bulk_n_eff <- rep(NA_real_, length(fit_names))
  tail_n_eff <- rep(NA_real_, length(fit_names))

  for (parameter in fit_names) {
    k <- which(parameter == fit_names)
    bulk_n_eff[k] <- rstan::ess_bulk(
      sims = nonpermuted_fit[, , parameter]
    )
    tail_n_eff[k] <- rstan::ess_tail(
      sims = nonpermuted_fit[, , parameter]
    )
  }

  # ---  Merge results ---

  # Convert `rhat_and_n_eff` to data.table
  rhat_and_n_eff_dt <- data.table::as.data.table(
    x = rhat_and_n_eff,
    keep.rownames = TRUE
  )

  # Change first column name from default `rn` to `parameter`
  data.table::setnames(
    rhat_and_n_eff_dt,
    old = "rn",
    new = "parameter",
    skip_absent = TRUE
  )

  # Make data.table for bulk_n_eff and tail_n_eff
  bulk_and_tail_eff_n <- data.table::data.table(
    "parameter" = fit_names,
    "bulk_n_eff" = bulk_n_eff,
    "tail_n_eff" = tail_n_eff
  )

  parameter_diagnostics <- data.table::merge.data.table(
    x = rhat_and_n_eff_dt,
    y = bulk_and_tail_eff_n,
    by = "parameter",
    sort = FALSE # Important because alphabetic sorting is nonsense here
  )

  # Round Rhat to two decimal places, and n_eff, bulk_n_eff and tail_n_eff
  # to nearest integers.
  parameter_diagnostics[, `:=`(
    Rhat = round(Rhat, 2),
    n_eff = round(n_eff),
    bulk_n_eff = round(bulk_n_eff),
    tail_n_eff = round(tail_n_eff)
  )]

  parameter_diagnostics
}


#' Process and Summarize Stan Model Output
#'
#' Takes the posterior draws from a Stan model fit and computes key biological
#' and analytical metrics, such as coefficients of variation (CVs) and credible
#' intervals, formatting them into user-friendly tables.
#'
#' @param fit An object, typically a \code{list} generated from
#'   \code{rstan::extract}, containing the posterior draws from the Stan model.
#'   It is expected to contain vectors and matrices of draws for parameters
#'   like \code{beta}, \code{sigma_I}, \code{sigma_A}, \code{sigma_G},
#'   \code{df_I}, \code{df_A}, etc.
#' @param log_transformed A \code{logical} value. If
#'   \code{TRUE}, it assumes the model was fitted on
#'   log-transformed data, and results (like \code{beta})
#'   are back-transformed. CV calculations are also
#'   adjusted accordingly.
#' @param analyte A \code{character} string used to label the measurand in the
#'   output tables. Defaults to "X".
#' @param material A \code{character} string to label the biological material.
#' @param sex A \code{character} string to label the sex. Defaults to "Unknown".
#' @param group A \code{character} string used to label the group or cohort in
#'   the output tables. Defaults to "Y".
#' @param data An optional \code{data.frame} or \code{data.table} containing the
#'   original input data. If provided, its
#'   \code{SubjectID} column will be used to label the
#'   subjects in the subject-specific output table.
#'   Defaults to \code{NULL}.
#' @param digits An \code{integer} specifying the number
#'   of decimal places to use when formatting the credible
#'   intervals. Defaults to 1.
#'
#' @details
#' This function serves as the primary post-processing step. It calculates the
#' mean concentration, within-subject CV (\code{CVI}), between-subject
#' CV (\code{CVG}),
#' and analytical CV (\code{CVA}) from the posterior distributions.
#' It handles calculations differently depending on whether
#' the model was run on a log-transformed scale. The function
#' also computes the distribution of the
#' predictive within-subject CV (\code{dCV_P(i)}) and the heterogeneity of this
#' distribution (HBHR).
#'
#' @section Calculation of CVs:
#' The method for calculating CVs depends on the \code{log_transformed} flag.
#' \itemize{
#'   \item \strong{If \code{log_transformed = TRUE}:} The function assumes that
#'     variance components (\code{sigma} parameters) are
#'     on the natural log scale. It uses the appropriate
#'     formulas to convert these into CVs on the original
#'     scale. For parameters with Student-t distributions
#'     (\code{sigma_I}, \code{sigma_A}), it accounts for
#'     the degrees of freedom (\code{df}).
#'   \item \strong{If \code{log_transformed = FALSE}:}
#'     The function assumes a model on the identity scale.
#'     CVs are calculated as \code{sd / mean * 100}. The
#'     standard deviation for t-distributed parameters is
#'     derived from the scale parameter (\code{sigma}) and
#'     degrees of freedom (\code{df}).
#' }
#'
#' @section Important Note on Model Validity:
#' The reliability of the metrics produced by this function depends entirely
#' on the quality and convergence of the Stan model fit. Before trusting the
#' output, it is essential to perform standard MCMC diagnostics (e.g., check for
#' divergent transitions, R-hat < 1.01, and sufficient \code{n_eff}).
#'
#' @return A \code{list} containing three \code{data.table} objects:
#' \describe{
#'   \item{Table 1}{A single-row summary table containing the overall model
#'     estimates, including mean concentration, CVI, CVG, CVA, the distribution
#'     of the predictive within-subject CV, and the HBHR, all with 95\% credible
#'     intervals.}
#'   \item{Table 2}{A subject-specific table showing the
#'     individual within-subject CV (\code{CV_P(i)}) for
#'     each subject, including the mean, median, and 95\%
#'     credible interval. Subjects are ordered by their
#'     median \code{CV_P(i)}.}
#'   \item{Table 3}{A summary table for the model's
#'     degrees of freedom parameters (\code{df_I} and
#'     \code{df_A}), showing their mean values and 95\%
#'     credible intervals.}
#' }
#'
#' @seealso \code{\link{plot_subject_specific_CVI}} for visualizing the results.
#' @export
process_stan_output <- function(fit,
                                log_transformed,
                                analyte = "X",
                                material = "Unknown",
                                sex = "Unknown",
                                group = "Y",
                                data = NULL,
                                digits = 1L,
                                priors = NULL,
                                stan_options = NULL,
                                stan_diagnostics = NULL) {
  # Fix global binding variables for data.table
  `median_CV_P(i)` <- SubjectIndex <- SubjectID <- NULL # nolint

  # This central step keeps the complex math separate from table formatting.
  if (log_transformed) {
    cvs <- calculate_cvs_log_scale(fit)
  } else {
    cvs <- calculate_cvs_identity_scale(fit)
  }

  # --- Build Summary Table (Table 1) ---
  table1 <- data.table::data.table(
    "Measurand" = analyte,
    "Sex" = sex,
    "Material" = material,
    "Group" = group,
    "No. of individuals" = if (!is.null(fit$G_raw)) {
      ncol(fit$G_raw)
    } else {
      ncol(fit$sigma_i)
    },
    "No. of results" = if (!is.null(fit$I_raw)) {
      ncol(fit$I_raw)
    } else {
      nrow(fit$beta)
    },
    "Mean concentration (95% CrI)" = format_ci(
      c(mean(cvs$beta_pred), quantile(cvs$beta_pred, c(0.025, 0.975))), digits
    ),
    "CVI (95% CrI) %" = format_ci(
      c(mean(cvs$cv_I), quantile(cvs$cv_I, c(0.025, 0.975))), digits
    ),
    "dCV_P(i) 50% (20%-80%) %" = format_ci(
      quantile(cvs$cv_I_pred, c(0.50, 0.20, 0.80)), digits
    ),
    "CVG (95% CrI) %" = format_ci(
      c(mean(cvs$cv_G), quantile(cvs$cv_G, c(0.025, 0.975))), digits
    ),
    "CVA (95% CrI) %" = format_ci(
      c(mean(cvs$cv_A), quantile(cvs$cv_A, c(0.025, 0.975))), digits
    ),
    "Estimated HBHR %" = format(
      mean(cvs$cv_I_sd) / mean(cvs$cv_I) * 100,
      nsmall = 1, digits = 1
    ),
    "Predicted SD HBHR %" = format(
      sd(cvs$sd_I_pred) / mean(cvs$sd_I_pred) * 100,
      nsmall = 1, digits = 1
    ),
    "Predicted CV HBHR %" = format(
      sd(cvs$cv_I_pred) / mean(cvs$cv_I_pred) * 100,
      nsmall = 1, digits = 1
    )
  )

  # --- Hierustical method for saving predicted CV HBHR % ---
  cvi_pred_sd_unstable <- sd(cvs$cv_I_pred) > 1.5 * mad(cvs$cv_I_pred)
  cvi_pred_mean_unstable <- mean(cvs$cv_I_pred) > 1.5 * median(cvs$cv_I_pred)
  if (isTRUE(cvi_pred_sd_unstable || cvi_pred_mean_unstable)) {
    table1$`Predicted CV HBHR %` <- paste0(
      format(
        x = mad(cvs$sd_I_pred) / median(cvs$sd_I_pred) * 100,
        nsmall = 1,
        digits = 1
      ),
      "*"
    )
  }

  # --- Build Subject-Specific Table (Table 2) ---
  individual_cvs_long <- data.table::melt(
    data.table::data.table(cvs$cv_pi),
    measure.vars = seq_len(ncol(cvs$cv_pi)),
    variable.name = "SubjectIndex",
    value.name = "CV_pi"
  )
  # Convert V1, V2 -> 1, 2
  individual_cvs_long[, SubjectIndex := as.integer(gsub("V", "", SubjectIndex))]

  # Summarize by subject
  table2 <- individual_cvs_long[, list(
    `mean_CV_P(i)` = mean(CV_pi),
    `median_CV_P(i)` = median(CV_pi),
    `CV_P(i)_lwr` = quantile(CV_pi, 0.025),
    `CV_P(i)_upr` = quantile(CV_pi, 0.975)
  ), by = SubjectIndex]

  # Predicted CV_p(i) + 60% Prediction Interval (20%-80%)
  table2[, `:=`(
    `dCV_P(i)_median` = median(cvs$cv_I_pred),
    `dCV_P(i)_lwr` = quantile(cvs$cv_I_pred, 0.20),
    `dCV_P(i)_upr` = quantile(cvs$cv_I_pred, 0.80)
  )]

  # Add concentration
  table2_conc <- data.table::melt(
    data.table::data.table(cvs$mu_pi),
    measure.vars = seq_len(ncol(cvs$mu_pi)),
    variable.name = "SubjectIndex",
    value.name = "mu_pi"
  )

  # Convert V1, V2 -> 1, 2
  table2_conc[, SubjectIndex := as.integer(gsub("V", "", SubjectIndex))]

  # Summarize by subject
  table2_conc <- table2_conc[, list(
    `concentration_mean_P(i)` = mean(mu_pi),
    `concentration_median_P(i)` = median(mu_pi),
    `concentration_P(i)_lwr` = quantile(mu_pi, 0.025),
    `concentration_P(i)_upr` = quantile(mu_pi, 0.975)
  ), by = SubjectIndex]

  table2 <- data.table::merge.data.table(
    x = table2,
    y = table2_conc,
    by = "SubjectIndex"
  )

  # Add subject labels if provided
  if (!is.null(data)) {
    subject_ids <- unique(
      data.table::as.data.table(data)[, "SubjectID", with = FALSE]
    )
    table2[, SubjectID := as.character(
      subject_ids[SubjectIndex, 1, with = FALSE][[1]]
    )]
  } else {
    table2[, SubjectID := as.character(SubjectIndex)]
  }

  # Order by median CV and create an ordered factor for plotting
  data.table::setorder(table2, `median_CV_P(i)`)
  table2[, SubjectID := factor(
    x = SubjectID,
    levels = unique(SubjectID)
  )]
  data.table::setcolorder(table2, c("SubjectIndex", "SubjectID"))


  # --- 4. Build Degrees of Freedom Table (Table 3) ---
  table3 <- data.table::data.table(
    "Measurand" = analyte,
    "Sex" = sex,
    "Material" = material,
    "Group" = group,
    "No. of individuals" = if (!is.null(fit$G_raw)) {
      ncol(fit$G_raw)
    } else {
      ncol(fit$sigma_i)
    },
    "No. of results" = if (!is.null(fit$I_raw)) {
      ncol(fit$I_raw)
    } else {
      nrow(fit$beta)
    },
    "Mean DFI (95% CrI)" = format_ci(
      c(mean(fit$df_I), quantile(fit$df_I, c(0.025, 0.975))), digits
    ),
    "Mean DFA (95% CrI)" = format_ci(
      c(mean(fit$df_A), quantile(fit$df_A, c(0.025, 0.975))), digits
    )
  )

  # --- 5. Build Samples Table (Table 4) ---
  table4 <- data.table::data.table(
    "Measurand" = analyte,
    "Sex" = sex,
    "Material" = material,
    "Group" = group,
    "beta" = cvs$beta_pred,
    "mean_cvi" = cvs$cv_I,
    "sd_cvi" = cvs$cv_I_sd,
    "pred_cvi" = cvs$cv_I_pred,
    "cva" = cvs$cv_A,
    "cvg" = cvs$cv_G
  )

  # --- Build Priors Table (Table 5) (Optional) ---
  table5 <- NULL
  if (!is.null(priors)) {
    fmt_prior <- function(expected, weakness) {
      paste0(
        format(expected, digits = digits),
        " (",
        format(weakness, digits = digits),
        ")"
      )
    }
    table5 <- data.table::data.table(
      "Measurand" = analyte,
      "Sex" = sex,
      "Material" = material,
      "Group" = group,
      "beta" = fmt_prior(priors$beta, priors$beta_weakness),
      "CVI" = fmt_prior(priors$cvi, priors$cvi_weakness),
      "CVA" = fmt_prior(priors$cva, priors$cva_weakness),
      "CVG" = fmt_prior(priors$cvg, priors$cvg_weakness),
      "DFI" = fmt_prior(priors$dfi, priors$dfi_weakness),
      "DFA" = fmt_prior(priors$dfa, priors$dfa_weakness),
      "HBHR" = fmt_prior(priors$hbhr, priors$hbhr_weakness)
    )
  }

  # --- Build Stan Options Table (Table 6) (Optional) ---
  table6 <- NULL
  if (!is.null(stan_options)) {
    warmup_pct <- round(stan_options$burn_fraction * 100)
    adapt_delta_pct <- round(stan_options$adapt_delta * 100)
    table6 <- data.table::data.table(
      "Measurand" = analyte,
      "Sex" = sex,
      "Material" = material,
      "Group" = group,
      "Log" = if (log_transformed) "Yes" else "No",
      "Iterations" = as.integer(stan_options$iterations),
      "Burn-in (%)" = warmup_pct,
      "Maximum Treedepth" = as.integer(stan_options$max_treedepth),
      "Acceptance Probability (%)" = adapt_delta_pct,
      "Chains" = as.integer(stan_options$chains),
      "Cores" = as.integer(stan_options$cores)
    )
  }

  # --- Build Stan Diagnostics Table (Table 7) (Optional) ---
  table7 <- NULL
  if (!is.null(stan_diagnostics)) {
    low_bulk <- if (length(stan_diagnostics$low_bulk_ess_params) > 0) {
      "Yes"
    } else {
      "No"
    }
    low_tail <- if (length(stan_diagnostics$low_tail_ess_params) > 0) {
      "Yes"
    } else {
      "No"
    }
    mixed <- if (isTRUE(stan_diagnostics$chains_mixed)) "Yes" else "No"
    table7 <- data.table::data.table(
      "Measurand" = analyte,
      "Sex" = sex,
      "Material" = material,
      "Group" = group,
      "Divergent Transitions" = as.integer(stan_diagnostics$n_divergent),
      "Exceed Maximum Treedepth" = as.integer(stan_diagnostics$n_max_treedepth),
      "Low Bulk Effective Sample Size" = low_bulk,
      "Low Tail Effective Sample Size" = low_tail,
      "Mixed" = mixed
    )
  }

  # --- 6. Return Results ---
  return(
    list(
      "Summary" = table1,
      "Subject_Specific" = table2,
      "Degrees_of_Freedom" = table3,
      "Samples" = table4,
      "Priors" = table5,
      "Stan_Options" = table6,
      "Stan_Diagnostics" = table7
    )
  )
}

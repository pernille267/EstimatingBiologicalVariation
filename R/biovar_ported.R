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
calculate_trace_and_posterior_plotting_data <- function(trace_fit, log_transformed) {
  # Convert from data.frame to data.table
  setDT(trace_fit)

  # --- Local helper: build a standardised trace data.table ---
  make_trace_dt <- function(src, value_expr, param_name, param_label) {
    copy(src)[, list(
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
  df_I_data <- trace_fit[parameter == "df_I"]
  df_A_data <- trace_fit[parameter == "df_A"]
  sigma_A_data <- trace_fit[parameter == "sigma_A"]
  sigma_G_data <- trace_fit[parameter == "sigma_G"]
  sigma_I_pred_data <- trace_fit[parameter == "sigma_I_pred"]
  sigma_I_mean_data <- trace_fit[parameter == "sigma_I"]
  sigma_I_sd_data <- trace_fit[parameter == "sigma_I_sd"]
  G_data <- trace_fit[grepl("G\\[", parameter, ignore.case = FALSE)]
  sigma_i_data <- trace_fit[grepl("sigma_i\\[", parameter, ignore.case = FALSE)]

  df_I <- df_I_data$value
  df_A <- df_A_data$value
  pred_beta <- beta_data$value
  n_subj <- length(unique(sigma_i_data$parameter))

  # --- Transform parameters depending on model scale ---
  if (log_transformed) {
    beta_data <- make_trace_dt(beta_data, exp(beta_data$value), "beta", "beta")
    df_I_data <- make_trace_dt(df_I_data, df_I_data$value, "df_I", "df[I]")
    df_A_data <- make_trace_dt(df_A_data, df_A_data$value, "df_A", "df[A]")
    cv_A_data <- make_trace_dt(sigma_A_data, logt_scale_to_cv(sigma_A_data$value, df_A), "cv_A", "CV[A]")
    cv_G_data <- make_trace_dt(sigma_G_data, lognormal_sd_to_cv(sigma_G_data$value), "cv_G", "CV[G]")
    cv_I_pred_data <- make_trace_dt(sigma_I_pred_data, lognormal_sd_to_cv(sigma_I_pred_data$value), "cv_I_pred", "dCV[P(i)]")
    cv_I_mean_data <- make_trace_dt(sigma_I_mean_data, logt_scale_to_cv(sigma_I_mean_data$value, df_I), "cv_I", "E(CV[I])")
    cv_I_sd_data <- make_trace_dt(sigma_I_sd_data, logt_scale_to_cv(sigma_I_sd_data$value, df_I), "cv_I_sd", "SD(CV[I])")

    cv_pi_data <- copy(sigma_i_data)[, list(
      value = logt_scale_to_cv(value, rep(df_I, n_subj)),
      parameter = gsub("sigma_i\\[(\\d+)\\]", "CV_p(\\1)", parameter),
      parameter_label = gsub("sigma_i\\[(\\d+)\\]", "CV[p(\\1)]", parameter),
      chain = chain, chain_label = paste0("Chain[", chain, "]"), iteration = iteration
    )]
    mu_pi_data <- copy(G_data)[, list(
      value = exp(value + rep(pred_beta, n_subj)),
      parameter = gsub("G\\[(\\d+)\\]", "mu_p(\\1)", parameter),
      parameter_label = gsub("G\\[(\\d+)\\]", "mu[p(\\1)]", parameter),
      chain = chain, chain_label = paste0("Chain[", chain, "]"), iteration = iteration
    )]
  } else {
    beta_data <- make_trace_dt(beta_data, beta_data$value, "beta", "beta")
    df_I_data <- make_trace_dt(df_I_data, df_I_data$value, "df_I", "df[I]")
    df_A_data <- make_trace_dt(df_A_data, df_A_data$value, "df_A", "df[A]")
    cv_A_data <- make_trace_dt(
      src = sigma_A_data,
      value_expr = calculate_cv(
        sd = t_scale_to_sd(sigma_A_data$value, df_A),
        mean = pred_beta
      ),
      param_name = "cv_A",
      param_label = "CV[A]"
    )
    cv_G_data <- make_trace_dt(sigma_G_data, calculate_cv(sigma_G_data$value, pred_beta), "cv_G", "CV[G]")
    cv_I_pred_data <- make_trace_dt(sigma_I_pred_data, calculate_cv(sigma_I_pred_data$value, pred_beta), "cv_I_pred", "dCV[P(i)]")
    cv_I_mean_data <- make_trace_dt(sigma_I_mean_data, calculate_cv(t_scale_to_sd(sigma_I_mean_data$value, df_I), pred_beta), "cv_I", "E(CV[I])")
    cv_I_sd_data <- make_trace_dt(sigma_I_sd_data, calculate_cv(t_scale_to_sd(sigma_I_sd_data$value, df_I), pred_beta), "cv_I_sd", "SD(CV[I])")

    mu_pi_vec <- G_data$value + rep(pred_beta, n_subj)

    cv_pi_data <- copy(sigma_i_data)[, list(
      value = calculate_cv(t_scale_to_sd(value, rep(df_I, n_subj)), mu_pi_vec),
      parameter = gsub("sigma_i\\[(\\d+)\\]", "CV_p(\\1)", parameter),
      parameter_label = gsub("sigma_i\\[(\\d+)\\]", "CV[p(\\1)]", parameter),
      chain = chain, chain_label = paste0("Chain[", chain, "]"), iteration = iteration
    )]
    mu_pi_data <- copy(G_data)[, list(
      value = value + rep(pred_beta, n_subj),
      parameter = gsub("G\\[(\\d+)\\]", "mu_p(\\1)", parameter),
      parameter_label = gsub("G\\[(\\d+)\\]", "mu[p(\\1)]", parameter),
      chain = chain, chain_label = paste0("Chain[", chain, "]"), iteration = iteration
    )]
  }

  plotting_data_univariate <- rbindlist(
    l = list(
      beta_data,
      df_I_data,
      df_A_data,
      cv_A_data,
      cv_G_data,
      cv_I_pred_data,
      cv_I_mean_data,
      cv_I_sd_data
    ),
    use.names = FALSE,
    fill = TRUE,
    idcol = NULL
  )

  output <- list(
    "univariate_parameters" = plotting_data_univariate,
    "homeostatic_set_points" = mu_pi_data,
    "subject_specific_cvs" = cv_pi_data
  )

  return(output)
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
calculate_parameter_diagnostics <- function(nonpermuted_fit, rhat_and_n_eff) {
  # --- Check if input `nonpermutedfit` is on the exected form (`array`) ---
  if (!is.array(nonpermuted_fit)) {
    stop(
      "The input `nonpermuted_fit` is not an array, but is of class `",
      class(nonpermuted_fit)[1],
      "`. Make sure that you have passed the nonpermuted output of ",
      "rstan::extract(..., permute = FALSE)"
    )
  }

  # --- Check if input `rhat_and_n_eff` is on the expected form (`matrix`)

  # Check class
  if (!is.matrix(rhat_and_n_eff)) {
    stop(
      "The input `rhat_and_n_eff` is expected to be a matrix, but is of class ",
      class(rhat_and_n_eff)[1],
      ". Ensure that it is a matrix with two columns (`Rhat` and `n_eff`)."
    )
  }

  # Check for expected column names
  expected_cols <- c("Rhat", "n_eff")
  if (!all(expected_cols %in% colnames(rhat_and_n_eff))) {
    stop(
      "The input `rhat_and_n_eff` is missing required columns. ",
      "Expected columns 'Rhat' and 'n_eff'. Found: ",
      paste(colnames(rhat_and_n_eff), collapse = ", ")
    )
  }

  # --- Get parameter names and check if the required names are there ---
  recorded_parameter_names <- colnames(nonpermuted_fit[, 1, ])

  # Check if the parameters in `nonpermuted_fit` matches those in `Rhat_and_n_eff`
  recorded_parameter_names_rhat_and_n_eff <- rownames(rhat_and_n_eff)

  # Check if both have the same names (order ignored)
  names_match_1 <- recorded_parameter_names %in% recorded_parameter_names_rhat_and_n_eff
  names_match_2 <- recorded_parameter_names_rhat_and_n_eff %in% recorded_parameter_names

  # Check for parameters of `recorded_parameter_names` not in
  # `recorded_parameter_names_rhat_and_n_eff`
  if (sum(!names_match_1) >= 1) {
    stop(
      "Some parameters that is part of `nonpermuted_fit` were not found in ",
      "`rhat_and_n_eff`. These are: ",
      paste(
        recorded_parameter_names[!names_match_1],
        collapse = ", "
      ),
      "."
    )
  }
  # Check for parameters of `recorded_parameter_names_rhat_and_n_eff` not in
  # `recorded_parameter_names`
  if (sum(!names_match_2) >= 1) {
    stop(
      "Some parameters that is part of `rhat_and_n_eff` were not found in ",
      "`nonpermuted_fit`. These are: ",
      paste(
        recorded_parameter_names[!names_match_2],
        collapse = ", "
      ),
      "."
    )
  }

  # Now we know that both set of names are equal, so we know that the number of
  # parameters are equal in both inputs. However, their order may be
  # different. We check if the order is equal.
  if (!all(recorded_parameter_names == recorded_parameter_names_rhat_and_n_eff)) {
    # Re-order rhat_and_n_eff to match nonpermuted_fit
    # This is less annoying than stopping.
    message(
      "Re-ordering `rhat_and_n_eff` rows to match `nonpermuted_fit` parameter order."
    )
    rhat_and_n_eff <- rhat_and_n_eff[recorded_parameter_names, , drop = FALSE]
  }

  # Now that we have checked that both inputs have the same parameters and
  # are given in the same order, we can check whether we have the individual
  # parameters. It is enough to check `nonpermuted_fit`.

  # 1. Check for `beta`, `df_I`, `df_A`, ..., `sigma_I`, `sigma_I_sd`
  # ----------------------------------------------------------------------------
  expected_univariate_parameters <- c(
    "beta", "df_I", "df_A",
    "sigma_A", "sigma_G", "sigma_I_pred",
    "sigma_I", "sigma_I_sd"
  )

  # Found and missing parameters (univariate)
  found <- expected_univariate_parameters %in% recorded_parameter_names # Found: TRUE, Missing: FALSE
  missing_parameters <- NULL
  if (sum(!found) >= 1) {
    missing_parameters <- expected_univariate_parameters[!found]
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

  # 2. Check for sigma_i[1], ..., sigma_i[n]
  # ----------------------------------------------------------------------------

  # Potential "issue": How can we check if EVERY sigma_i[k] is there. To do
  # this we must know k from before. It may not be worth implementing this at
  # this stage. For now we just check if there are ANY names that contain
  # `sigma_i[`. There is no real value of counting how many matches there are
  # as long as we do not know k from somewhere else.

  # Number of matches with substring `sigma_i[`
  found <- grepl(
    pattern = "sigma_i\\[",
    x = recorded_parameter_names,
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
    x = recorded_parameter_names,
    ignore.case = FALSE
  )

  # Expects at least one match. Typically there will be more matches.
  if (!any(found)) {
    stop(
      "The G[k] parameters (subject effects `G_k` of the model) ",
      "were expected to be found. However, none was found."
    )
  }

  # We have now checked:
  # 1. That the input is an array
  # 2. The array contain data for the required parameters
  # 3. ... We could implement further checks, but we postpone this to later.
  # We do not expect that this will be critical. Good documentation are key,
  # so that the user KNOW what `nonpermuted_fit` should look like and contain.

  # We should loop over the elements in `recorded_parameter_names`
  # For each element (j) here, we extract
  # nonpermuted_fit[, , recorded_parameter_names[k]] and calculate
  # bulk and tail effective sample sizes
  bulk_n_eff <- rep(NA_real_, length(recorded_parameter_names))
  tail_n_eff <- rep(NA_real_, length(recorded_parameter_names))

  for (parameter in recorded_parameter_names) {
    k <- which(parameter == recorded_parameter_names)
    bulk_n_eff[k] <- rstan::ess_bulk(
      sims = nonpermuted_fit[, , parameter]
    )
    tail_n_eff[k] <- rstan::ess_tail(
      sims = nonpermuted_fit[, , parameter]
    )
  }

  # ---  Merge results ---

  # Convert `rhat_and_n_eff` to data.table
  rhat_and_n_eff_dt <- as.data.table(
    x = rhat_and_n_eff,
    keep.rownames = TRUE
  )

  # Change first column name from default `rn` to `parameter`
  setnames(rhat_and_n_eff_dt, old = "rn", new = "parameter", skip_absent = TRUE)

  # Make data.table for bulk_n_eff and tail_n_eff
  bulk_and_tail_eff_n <- data.table(
    "parameter" = recorded_parameter_names,
    "bulk_n_eff" = bulk_n_eff,
    "tail_n_eff" = tail_n_eff
  )

  parameter_diagnostics <- merge.data.table(
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

  return(parameter_diagnostics)
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
                                digits = 1L) {
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
    "No. of individuals" = if (!is.null(fit$G_raw)) ncol(fit$G_raw) else ncol(fit$sigma_i),
    "No. of results" = if (!is.null(fit$I_raw)) ncol(fit$I_raw) else nrow(fit$beta),
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

  # --- Check if sd(cvs$cv_I_pred) or mean(cvs$cv_I_pred) are unstable ---
  cv_I_pred_sd_unstable <- sd(cvs$cv_I_pred) > 1.5 * mad(cvs$cv_I_pred)
  cv_I_pred_mean_unstable <- mean(cvs$cv_I_pred) > 1.5 * median(cvs$cv_I_pred)
  if (cv_I_pred_sd_unstable | cv_I_pred_mean_unstable) {
    table1$`Predicted CV HBHR %` <- paste0(
      format(
        x = mad(cvs$sd_I_pred) / median(cvs$sd_I_pred) * 100,
        nsmall = 1,
        digits = 1
      ),
      " (robust estimate)"
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

  # Add population predictive distribution for plotting
  table2[, `:=`(
    `dCV_P(i)_median` = median(cvs$cv_I_pred),
    `dCV_P(i)_lwr` = quantile(cvs$cv_I_pred, 0.20),
    `dCV_P(i)_upr` = quantile(cvs$cv_I_pred, 0.80)
  )]

  # Add concentration
  table2_conc <- data.table::melt(
    data.table::data.table(cvs$mu_pi),
    measure.vars = 1:ncol(cvs$mu_pi),
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

  table2 <- merge.data.table(
    x = table2,
    y = table2_conc,
    by = "SubjectIndex"
  )

  # Add subject labels if provided
  if (!is.null(data)) {
    subject_ids <- unique(as.data.table(data)[, "SubjectID", with = FALSE])
    table2[, SubjectID := as.character(
      subject_ids[SubjectIndex, 1, with = FALSE][[1]]
    )]
  } else {
    table2[, SubjectID := as.character(SubjectIndex)]
  }

  # Order by median CV and create an ordered factor for plotting
  data.table::setorder(table2, `median_CV_P(i)`)
  table2[, SubjectID := factor(SubjectID, levels = unique(SubjectID))]
  data.table::setcolorder(table2, c("SubjectIndex", "SubjectID"))


  # --- 4. Build Degrees of Freedom Table (Table 3) ---
  table3 <- data.table::data.table(
    "Measurand" = analyte,
    "Sex" = sex,
    "Material" = material,
    "Group" = group,
    "No. of individuals" = if (!is.null(fit$G_raw)) ncol(fit$G_raw) else ncol(fit$sigma_i),
    "No. of results" = if (!is.null(fit$I_raw)) ncol(fit$I_raw) else nrow(fit$beta),
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

  # --- 6. Return Results ---
  return(
    list(
      "Summary" = table1,
      "Subject_Specific" = table2,
      "Degrees_of_Freedom" = table3,
      "Samples" = table4
    )
  )
}

#' Plot Subject-Specific (Within Individual) CVs
#'
#' Creates a dot-and-whisker plot visualizing the 95% credible intervals for
#' the intra-individual CV (\code{CVI}) for each subject.
#' It includes reference lines for the population predictive
#' distribution and allows for flexible grouping by color
#' and shape.
#'
#' @param processed_output A \code{list} object returned by
#'   \code{process_stan_output}. The function specifically
#'   uses the \code{Subject_Specific} data.table.
#' @param data An optional \code{data.frame} or \code{data.table} containing the
#'   original input data. This is \strong{required} if you
#'   want to use the \code{color_by} or \code{shape_by}
#'   arguments, as it must contain the grouping columns.
#' @param color_by A \code{character} string specifying
#'   the column name in \code{data} to use for coloring
#'   the points (e.g., \code{"sex"}). Defaults to
#'   \code{NULL} (no color grouping).
#' @param shape_by A \code{character} string specifying
#'   the column name in \code{data} to use for shaping
#'   the points (e.g., \code{"material"}). Defaults to
#'   \code{NULL} (no shape grouping).
#' @param title A \code{character} string for the plot title. Defaults to a
#'   standard title.
#' @param subtitle A \code{character} string for the plot subtitle. Can be used
#'   to provide context like the analyte name.
#' @param against_concentration A \code{logical} value. If \code{TRUE},
#'   estimated concentration values for the subjects are plotted against CVs
#'   instead of subject-specific CV values plotted against subject IDs.
#' @param against_RCV A \code{logical} value. If \code{TRUE}, Reference Change
#'   values (RCVs) are plotted against CVs instead of subject-specific CV
#'   values plotted against subject IDs. If both \code{against_concentration}
#'   and \code{against_RCV} are set to \code{TRUE}, \code{against_concentration}
#'   wins.
#' @param log_RCV A \code{logical} value. If \code{TRUE}, RCVs are calculated
#'   assuming data is log-transformed.
#'
#' @details
#' The plot is designed for clear visual comparison of individual CVI estimates.
#' \itemize{
#'   \item \strong{Points and Error Bars:} The central
#'     point for each subject represents their median
#'     posterior CVI, and the horizontal bars represent
#'     the 95\% credible interval.
#'   \item \strong{Green Reference Band:} The shaded
#'     green area represents the 60\% prediction interval
#'     for a new subject's CVI (from the 20th to the 80th
#'     percentile). The solid green line is the median of
#'     this predictive distribution.
#'   \item \strong{Ordering:} Subjects are ordered from
#'     bottom to top by their median CVI.
#' }
#'
#' @return A \code{ggplot} object, which can be further customized.
#'
#' @export
plot_subject_specific_CVI <- function(processed_output, # nolint
                                      data = NULL,
                                      color_by = NULL,
                                      shape_by = NULL,
                                      title = "Subject-Specific CVs with 95% Credible Intervals", # nolint
                                      subtitle = NULL,
                                      against_concentration = FALSE,
                                      against_RCV = FALSE, # nolint
                                      log_RCV = FALSE) { # nolint
  # --- 1. Input Validation and Data Preparation ---
  if (!"Subject_Specific" %in% names(processed_output)) {
    stop(
      "Input 'processed_output' must be a list from the ",
      "process_stan_output() function."
    )
  }

  plotting_data <- data.table::copy(processed_output$Subject_Specific)

  grouping_vars <- c(color_by, shape_by)
  if (length(grouping_vars) > 0) {
    if (is.null(data)) {
      stop(
        "You must provide the 'data' argument to use ",
        "'color_by' or 'shape_by'."
      )
    }
    group_data <- data.table::as.data.table(data)
    missing_vars <- setdiff(grouping_vars, names(group_data))
    if (length(missing_vars) > 0) {
      stop(
        paste(
          "Grouping variables not in 'data' frame:",
          paste(missing_vars, collapse = ", ")
        )
      )
    }
    subject_metadata <- unique(
      group_data[, c("SubjectID", grouping_vars), with = FALSE]
    )

    plotting_data[, SubjectID := as.character(SubjectID)]
    subject_metadata[, SubjectID := as.character(SubjectID)]

    plotting_data <- data.table::merge.data.table(
      plotting_data,
      subject_metadata,
      by = "SubjectID",
      all.x = TRUE
    )
  }

  # --- Re-sort the data by median CV AFTER the merge ---
  # This ensures the y-axis is ordered correctly from lowest to highest CV.
  data.table::setorder(plotting_data, `median_CV_P(i)`)

  # Create the ordered factor for the y-axis
  # based on the new, correct sort order
  plotting_data[, SubjectID := factor(SubjectID, levels = unique(SubjectID))]

  # --- 2. Build the ggplot object ---

  if (against_concentration) {
    p <- ggplot(data = plotting_data, aes(x = `concentration_median_P(i)`))
    pop_summary <- unique(plotting_data[, list(
      `dCV_P(i)_lwr`,
      `dCV_P(i)_upr`,
      `dCV_P(i)_median`
    )])
    p <- p + geom_rect(
      mapping = aes(
        xmin = -Inf,
        xmax = Inf,
        ymin = pop_summary$`dCV_P(i)_lwr`[1],
        ymax = pop_summary$`dCV_P(i)_upr`[1]
      ),
      fill = "#28A745",
      alpha = 0.1,
      inherit.aes = FALSE
    ) +
      geom_hline(
        yintercept = c(
          pop_summary$`dCV_P(i)_lwr`[1],
          pop_summary$`dCV_P(i)_median`[1],
          pop_summary$`dCV_P(i)_upr`[1]
        ),
        color = "#1a702e",
        linewidth = 1.0
      )

    p <- p + geom_errorbar(
      mapping = aes(
        ymin = `CV_P(i)_lwr`,
        ymax = `CV_P(i)_upr`
      ),
      height = 0.5,
      linewidth = 0.75,
      color = "black"
    )

    point_aes <- aes(y = `median_CV_P(i)`)
    if (!is.null(color_by)) {
      point_aes$fill <- as.symbol(color_by)
    }
    if (!is.null(shape_by)) {
      point_aes$shape <- as.symbol(shape_by)
    }

    p <- p + geom_point(
      mapping = point_aes,
      size = 3,
      color = "black",
      shape = if (is.null(shape_by)) 21 else NA,
      fill = if (is.null(color_by)) "#A7288A" else NA
    )

    p <- p +
      scale_x_continuous(
        name = "Concentration",
        labels = function(x) format(x, nsmall = 1, digits = 1),
        n.breaks = 8
      ) +
      scale_y_continuous(
        name = expression(paste("Within-Individual CV (", CV[p(i)], ", %)")),
        labels = function(y) paste0(format(y, nsmall = 1, digits = 1), " %"),
        n.breaks = 8
      ) +
      labs(title = title, subtitle = subtitle) +
      theme_classic() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(face = "bold")
      )

    return(p)
  } else if (against_RCV) {
    CV_A <- as.numeric(
      sub(" .*", "", processed_output$Summary$`CVA (95% CrI) %`)
    )
    CV_I <- as.numeric(
      sub(" .*", "", processed_output$Summary$`CVI (95% CrI) %`)
    )

    RCVs <- c(0, 0)

    cov_factor <- qnorm(0.975) * sqrt(2)

    if (log_RCV) {
      plotting_data[, `:=`(
        sdlog_median = sqrt(
          log((CV_A / 100)^2 + (`median_CV_P(i)` / 100)^2 + 1)
        ),
        sdlog_lower = sqrt(
          log((CV_A / 100)^2 + (`CV_P(i)_lwr` / 100)^2 + 1)
        ),
        sdlog_upper = sqrt(
          log((CV_A / 100)^2 + (`CV_P(i)_upr` / 100)^2 + 1)
        )
      )]
      plotting_data[, `:=`(
        upper_RCV_median = (exp(cov_factor * sdlog_median) - 1) * 100,
        upper_RCV_lower = (exp(cov_factor * sdlog_lower) - 1) * 100,
        upper_RCV_upper = (exp(cov_factor * sdlog_upper) - 1) * 100,
        lower_RCV_median = (exp(-cov_factor * sdlog_median) - 1) * 100,
        lower_RCV_lower = (exp(-cov_factor * sdlog_lower) - 1) * 100,
        lower_RCV_upper = (exp(-cov_factor * sdlog_upper) - 1) * 100
      )]
      sdlog <- sqrt(log((CV_A / 100)^2 + (CV_I / 100)^2 + 1))
      RCVs <- c(
        (exp(cov_factor * sdlog) - 1) * 100,
        (exp(-cov_factor * sdlog) - 1) * 100
      )
    } else {
      plotting_data[, `:=`(
        upper_RCV_median = cov_factor * sqrt(CV_A^2 + `median_CV_P(i)`^2),
        upper_RCV_lower = cov_factor * sqrt(CV_A^2 + `CV_P(i)_lwr`^2),
        upper_RCV_upper = cov_factor * sqrt(CV_A^2 + `CV_P(i)_upr`^2),
        lower_RCV_median = -cov_factor * sqrt(CV_A^2 + `median_CV_P(i)`^2),
        lower_RCV_lower = -cov_factor * sqrt(CV_A^2 + `CV_P(i)_lwr`^2),
        lower_RCV_upper = -cov_factor * sqrt(CV_A^2 + `CV_P(i)_upr`^2)
      )]
      RCVs <- c(
        cov_factor * sqrt(CV_A^2 + CV_I^2),
        -cov_factor * sqrt(CV_A^2 + CV_I^2)
      )
    }
    p <- ggplot(data = plotting_data)

    # Start of by adding layers for ribbons and lines for RCV curves
    p <- p +
      geom_ribbon(
        mapping = aes(
          x = `median_CV_P(i)`,
          ymin = upper_RCV_lower,
          ymax = upper_RCV_upper
        ),
        alpha = 0.8,
        fill = "#28A745",
        color = "black"
      ) +
      geom_line(
        mapping = aes(
          x = `median_CV_P(i)`,
          y = upper_RCV_median
        ),
        linewidth = 1,
        color = "black"
      ) +
      geom_ribbon(
        mapping = aes(
          x = `median_CV_P(i)`,
          ymin = lower_RCV_lower,
          ymax = lower_RCV_upper
        ),
        alpha = 0.8,
        fill = "#28A745",
        color = "black"
      ) +
      geom_line(
        mapping = aes(
          x = `median_CV_P(i)`,
          y = lower_RCV_median
        ),
        linewidth = 1,
        color = "black"
      ) +
      geom_hline(
        yintercept = RCVs,
        color = "#605CA8",
        linewidth = 1.25
      )

    # Construct point aestics
    point_aes_upper <- aes(
      x = `median_CV_P(i)`,
      y = upper_RCV_median
    )
    point_aes_lower <- aes(
      x = `median_CV_P(i)`,
      y = lower_RCV_median
    )

    # Make grouping aestics for the points
    if (!is.null(color_by)) {
      point_aes_upper$fill <- as.symbol(color_by)
      point_aes_lower$fill <- as.symbol(color_by)
    }
    if (!is.null(shape_by)) {
      point_aes_upper$shape <- as.symbol(shape_by)
      point_aes_lower$shape <- as.symbol(shape_by)
    }

    # Add layers for points (`center` of the ribbons)
    p <- p +
      geom_point(
        mapping = point_aes_upper,
        size = 3,
        color = "black",
        shape = if (is.null(shape_by)) 21 else NA,
        fill = if (is.null(color_by)) "#605CA8" else NA
      ) +
      geom_point(
        mapping = point_aes_lower,
        size = 3,
        color = "black",
        shape = if (is.null(shape_by)) 21 else NA,
        fill = if (is.null(color_by)) "#605CA8" else NA
      )

    # Check if the title is the default. Change to a different default if so
    if (title == "Subject-Specific CVs with 95% Credible Intervals") {
      title <- "Subject-Specific RCVs with Uncertainty"
      subtitle <- "Incorporated from CV Credible Intervals"
    }

    # Set ticks, labels, titles and more
    p <- p +
      scale_x_continuous(
        name = expression(paste("Within-Individual CV (", CV[p(i)], ", %)")),
        n.breaks = 12,
        labels = function(x) paste0(format(x, nsmall = 1, digits = 1), " %")
      ) +
      scale_y_continuous(
        name = "RCV (%)",
        n.breaks = 12,
        labels = function(y) paste0(format(y, nsmall = 1, digits = 1), " %")
      ) +
      labs(
        title = title,
        subtitle = subtitle
      ) +
      theme_classic() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(face = "bold")
      )

    return(p)
  }

  p <- ggplot(data = plotting_data, aes(y = SubjectID))
  pop_summary <- unique(plotting_data[, .(`dCV_P(i)_lwr`, `dCV_P(i)_upr`, `dCV_P(i)_median`)])
  p <- p +
    geom_rect(
      aes(
        xmin = pop_summary$`dCV_P(i)_lwr`,
        xmax = pop_summary$`dCV_P(i)_upr`,
        ymin = -Inf, ymax = Inf
      ),
      fill = "#28A745",
      alpha = 0.1,
      inherit.aes = FALSE
    ) +
    geom_vline(
      xintercept = c(
        pop_summary$`dCV_P(i)_lwr`,
        pop_summary$`dCV_P(i)_median`,
        pop_summary$`dCV_P(i)_upr`
      ),
      color = "#1a702e",
      linewidth = 1.0
    )

  p <- p + geom_errorbarh(
    aes(xmin = `CV_P(i)_lwr`, xmax = `CV_P(i)_upr`),
    height = 0.5, linewidth = 0.75, color = "black"
  )

  if (!is.null(color_by)) {
    plotting_data[, (color_by) := as.factor(get(color_by))]
    p <- p + scale_fill_brewer(palette = "Set2", name = toTitleCase(color_by))
  } else {
    p <- p + scale_fill_manual(values = "orange", guide = "none")
  }

  if (!is.null(shape_by)) {
    plotting_data[, (shape_by) := as.factor(get(shape_by))]
    p <- p + scale_shape_manual(
      values = c(21, 22, 24, 25),
      name = toTitleCase(shape_by)
    )
  }

  # --- 3. Dynamically add point aesthetics ---
  point_aes <- aes(x = `median_CV_P(i)`)
  if (!is.null(color_by)) {
    point_aes$fill <- as.symbol(color_by)
  }
  if (!is.null(shape_by)) {
    point_aes$shape <- as.symbol(shape_by)
  }

  p <- p + geom_point(
    mapping = point_aes,
    size = 3,
    color = "black",
    shape = if (is.null(shape_by)) 21 else NA,
    fill = if (is.null(color_by)) "#A7288A" else NA
  )

  # --- 4. Customize scales, labels, and theme ---
  p <- p +
    scale_x_continuous(
      name = expression(paste("Within-Individual CV (", CV[p(i)], ", %)")),
      labels = function(x) paste0(format(x, nsmall = 1, digits = 1), " %"),
      n.breaks = 8
    ) +
    ylab("Subject") +
    labs(title = title, subtitle = subtitle) +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(face = "bold")
    )

  return(p)
}


# ============================================================================
# Shared Plotting Helpers (used by plot_trace_plots &
# plot_posterior_density)
# ============================================================================

#' Validate shared inputs for trace / density plots.
#'
#' @param plotting_data A \code{list} from
#'   \code{calculate_trace_and_posterior_plotting_data()}.
#' @param parameter_set A \code{character} string.
#' @keywords internal
validate_plotting_inputs <- function(plotting_data,
                                     parameter_set) {
  if (!"univariate_parameters" %in% names(plotting_data)) {
    stop(
      "Input 'plotting_data' must be a list from the ",
      "calculate_trace_and_posterior_plotting_data() ",
      "function."
    )
  }
  valid_options <- c(
    "univariate", "subject_cvs", "h_set_points"
  )
  if (length(parameter_set) != 1 ||
    !parameter_set %in% valid_options) {
    stop(
      "Input 'parameter_set' must be a single character ",
      "string. Valid options include: 'univariate', ",
      "'subject_cvs', and 'h_set_points'."
    )
  }
  invisible(NULL)
}

#' Canonical order for univariate parameter labels.
#'
#' @return A character vector of parameter label strings.
#' @keywords internal
get_univariate_parameter_order <- function() {
  c(
    "beta",
    "df[I]",
    "df[A]",
    "CV[A]",
    "CV[G]",
    "dCV[P(i)]",
    "E(CV[I])",
    "SD(CV[I])"
  )
}

#' Order factor levels of \code{parameter_label} by
#' the embedded integer index.
#'
#' Works for labels like \code{"CV[p(3)]"} or
#' \code{"mu[p(12)]"}.
#'
#' @param dt A \code{data.table} with a
#'   \code{parameter_label} column.
#' @param regex_pattern Regex with one capture group
#'   extracting the integer (e.g.,
#'   \code{"^CV\\\\[p\\\\(([0-9]+)\\\\)\\\\]$"}).
#' @return \code{dt}, modified in place with
#'   \code{parameter_label} set as an ordered factor.
#' @keywords internal
order_parameter_labels <- function(dt, regex_pattern) {
  unique_labels <- unique(dt$parameter_label)
  integer_order <- order(
    as.numeric(
      gsub(
        pattern = regex_pattern,
        replacement = "\\1",
        x = unique_labels
      )
    )
  )
  desired_order <- unique_labels[integer_order]
  dt[, parameter_label := factor(
    parameter_label,
    levels = desired_order
  )]
  dt
}

#' Compute axis breaks and formatted labels for a
#' parameter's values.
#'
#' Uses the 1st/99th percentile effective range to
#' choose the number of decimal places.
#'
#' @param values Numeric vector of parameter draws.
#' @param n_breaks Integer vector of length 3: number of
#'   breaks for the small, medium, and large ranges.
#' @return A \code{list} with elements \code{breaks} and
#'   \code{labels}.
#' @keywords internal
get_breaks_and_labels <- function(values, n_breaks = c(6L, 6L, 6L)) {
  p01 <- quantile(values, probs = 0.01)
  p99 <- quantile(values, probs = 0.99)
  eff_range <- p99 - p01

  if (eff_range <= 1.2) {
    brks <- seq(p01, p99, length.out = n_breaks[1L])
    labs <- format(brks, nsmall = 2, digits = 2)
  } else if (eff_range <= 12) {
    brks <- seq(p01, p99, length.out = n_breaks[2L])
    labs <- format(brks, nsmall = 1, digits = 1)
  } else {
    brks <- seq(p01, p99, length.out = n_breaks[3L])
    labs <- as.character(round(brks))
  }
  list(breaks = brks, labels = labs)
}

#' Return \code{" \%"} for CV parameters, \code{""} for
#' others.
#'
#' @param parameter_name A character string (plotmath
#'   label).
#' @return A single-element character string.
#' @keywords internal
get_percent_suffix <- function(parameter_name) {
  cv_params <- c(
    "CV[A]", "CV[G]", "dCV[P(i)]",
    "E(CV[I])", "SD(CV[I])"
  )
  if (parameter_name %in% cv_params) " %" else ""
}

#' Compute hard axis limits that trim extreme
#' outliers.
#'
#' @param values Numeric vector of parameter draws.
#' @param parameter_name Character; plotmath label used
#'   to determine the trimming strategy.
#' @param hard_trim Numeric; quantile probability used
#'   for trimming.
#' @return A numeric vector of length 2
#'   \code{c(lower, upper)}.
#' @keywords internal
compute_hard_axis_limits <- function(values,
                                     parameter_name,
                                     hard_trim = 0.001) {
  if (parameter_name == "beta") {
    return(quantile(
      values,
      probs = c(hard_trim, 1 - hard_trim),
      names = FALSE
    ))
  }
  if (parameter_name %in% c("df[I]", "df[A]")) {
    return(c(
      2,
      quantile(values,
        probs = 1 - 2 * hard_trim,
        names = FALSE
      )
    ))
  }
  c(
    min(values),
    quantile(values,
      probs = 1 - 2 * hard_trim,
      names = FALSE
    )
  )
}

#' Create a range-aware label function for
#' \code{scale_*_continuous}.
#'
#' Chooses 0.01 / 0.1 / 1 accuracy depending on the
#' data range and appends a suffix.
#'
#' @param suffix Character; appended to each label
#'   (e.g. \code{" \%"}).
#' @return A function suitable for \code{labels}
#'   in \code{scale_*_continuous}.
#' @keywords internal
make_range_based_labeller <- function(suffix = "") {
  function(x) {
    data_range <- diff(range(x, na.rm = TRUE))
    if (data_range <= 0.6) {
      acc <- 0.01
    } else if (data_range <= 12) {
      acc <- 0.1
    } else {
      acc <- 1
    }
    scales::number(x, accuracy = acc, suffix = suffix)
  }
}

#' Compute iteration-axis breaks based on the total
#' number of post-warmup iterations.
#'
#' @param iterations Numeric vector of iteration IDs.
#' @return A \code{list} with elements \code{breaks}
#'   and \code{shift} (the minimum iteration ID).
#' @keywords internal
compute_iteration_breaks <- function(iterations) {
  n_iter <- diff(range(iterations))
  iter_shift <- min(iterations)
  iter_max <- max(iterations)
  step <- if (n_iter <= 1000) {
    200
  } else if (n_iter <= 2500) {
    300
  } else if (n_iter <= 5000) {
    400
  } else {
    600
  }
  list(
    breaks = seq(iter_shift - 1, iter_max, by = step),
    shift = iter_shift
  )
}

#' Assemble a cowplot grid with title, subtitle, and
#' optional shared legend.
#'
#' @param plot_list A named list of ggplot objects.
#' @param title Character; main title.
#' @param subtitle Character; subtitle.
#' @param shared_legend Optional grob for a shared
#'   legend (placed to the right).
#' @param ncol Number of columns in the grid.
#' @return A cowplot object.
#' @keywords internal
assemble_titled_cowplot_grid <- function(
  plot_list,
  title,
  subtitle,
  shared_legend = NULL,
  ncol = 3L
) {
  gathered <- cowplot::plot_grid(
    plotlist = plot_list, ncol = ncol
  )

  margin_extra <- if (!is.null(shared_legend)) {
    margin(0, 0, 0, 7)
  } else {
    margin(0, 0, 0, 0)
  }

  title_grob <- cowplot::ggdraw() +
    cowplot::draw_label(
      title,
      fontface = "bold", x = 0.5, hjust = 0.5
    ) +
    theme(plot.margin = margin_extra)

  subtitle_grob <- cowplot::ggdraw() +
    cowplot::draw_label(
      subtitle,
      x = 0.5, hjust = 0.5
    ) +
    theme(plot.margin = margin_extra)

  assembled <- cowplot::plot_grid(
    title_grob, subtitle_grob, gathered,
    ncol = 1,
    rel_heights = c(0.05, 0.05, 1)
  )

  if (!is.null(shared_legend)) {
    assembled <- cowplot::plot_grid(
      assembled, shared_legend,
      ncol = 2, rel_widths = c(1, 0.15)
    )
  }
  assembled
}


#' Plot Trace Plots for Stan Model Parameters
#'
#' This function generates trace plots using ggplot2 from the output of
#' \code{calculate_trace_and_posterior_plotting_data()}.
#'
#' @param plotting_data A \code{list} generated from
#'   \code{calculate_trace_and_posterior_plotting_data()}.
#' @param parameter_set A \code{character} string. One of:
#'   "univariate" (default), "subject_cvs", or "h_set_points".
#' @param title A \code{character} string for the plot title.
#' @param subtitle A \code{character} string for the plot subtitle.
#'
#' @return A \code{ggplot} object.
plot_trace_plots <- function(plotting_data,
                             parameter_set = "univariate",
                             title = NULL,
                             subtitle = NULL) {
  # --- 1. Input Validation ---
  validate_plotting_inputs(plotting_data, parameter_set)

  # Default values
  y_label_appendix <- ""
  y_name <- "CV[p(i)]"

  # Select the correct data and set default titles
  if (parameter_set == "univariate") {
    plotting_data_selected <- copy(
      plotting_data$univariate_parameters
    )
    if (is.null(title)) {
      title <- "Trace Plots for the Univariate Model Parameters"
    }

    desired_order <- get_univariate_parameter_order()
    plotting_data_selected[, parameter_label := factor(
      parameter_label,
      levels = desired_order
    )]

    # --- Create list of individual plots ---
    plot_list <- list()

    for (relevant_parameter in desired_order) {
      parameter_data <- plotting_data_selected[
        parameter_label == relevant_parameter,
      ]
      if (nrow(parameter_data) == 0) next

      # x-axis breaks
      iter_info <- compute_iteration_breaks(
        parameter_data$iteration
      )

      # y-axis breaks & labels
      bl <- get_breaks_and_labels(
        parameter_data$value
      )

      y_label_appendix <- get_percent_suffix(
        relevant_parameter
      )

      # Hard limits to trim extreme outliers
      hard_ylims <- compute_hard_axis_limits(
        parameter_data$value,
        relevant_parameter,
        hard_trim = 0.001
      )

      # Build trace plot for this parameter
      p_individual <- ggplot(data = parameter_data) +
        geom_path(
          mapping = aes(
            x = iteration, y = value,
            color = chain_label
          ),
          alpha = 0.2
        ) +
        geom_smooth(
          mapping = aes(
            x = iteration, y = value,
            color = chain_label
          ),
          se = FALSE, linewidth = 0.8,
          method = "gam",
          formula = y ~ s(x, bs = "cs")
        ) +
        scale_x_continuous(
          name = NULL,
          breaks = iter_info$breaks,
          labels = function(x) {
            paste0(
              format(
                x = (x - iter_info$shift) / 1e3,
                nsmall = 1, digits = 1
              ),
              "K"
            )
          },
          expand = expansion(mult = c(0, 0))
        ) +
        scale_y_continuous(
          name = parse(text = relevant_parameter),
          breaks = bl$breaks,
          labels = paste0(bl$labels, y_label_appendix),
          expand = expansion(c(0, 0.02))
        ) +
        scale_color_discrete(
          name = "Chain ID",
          labels = scales::label_parse()
        ) +
        coord_cartesian(ylim = hard_ylims) +
        theme_classic() +
        theme(legend.position = "none")

      plot_list[[relevant_parameter]] <- p_individual
    }

    # --- Create shared legend ---
    p_dummy <- ggplot(
      data = plotting_data_selected[parameter == "beta"]
    ) +
      geom_point(
        mapping = aes(
          x = iteration, y = value,
          color = chain_label
        )
      ) +
      scale_color_discrete(
        name = "Chain ID",
        labels = scales::label_parse()
      ) +
      theme(legend.position = "right")

    shared_legend <- cowplot::get_plot_component(
      plot = p_dummy,
      pattern = "guide-box-right",
      return_all = TRUE
    )

    return(assemble_titled_cowplot_grid(
      plot_list, title, subtitle,
      shared_legend = shared_legend
    ))
  } else if (parameter_set == "subject_cvs") {
    plotting_data_selected <- copy(
      plotting_data$subject_specific_cvs
    )
    order_parameter_labels(
      plotting_data_selected,
      "^CV\\[p\\(([0-9]+)\\)\\]$"
    )
    y_label_appendix <- " %"
    if (is.null(title)) {
      title <- "Trace Plots for the Subject-Specific CVs"
    }
  } else if (parameter_set == "h_set_points") {
    plotting_data_selected <- copy(
      plotting_data$homeostatic_set_points
    )
    order_parameter_labels(
      plotting_data_selected,
      "^mu\\[p\\(([0-9]+)\\)\\]$"
    )
    if (is.null(title)) {
      title <- "Trace Plots for the Homeostatic Set Points"
    }
    y_name <- "mu[p(i)]"
  }

  # --- 2. Faceted plot (subject_cvs / h_set_points) ---

  if (!exists("plotting_data_selected") ||
    nrow(plotting_data_selected) == 0) {
    warning(paste(
      "No data found for parameter_set:",
      parameter_set
    ))
    return(ggplot() +
      labs(title = "No data to plot"))
  }

  iter_info <- compute_iteration_breaks(
    plotting_data_selected$iteration
  )

  p <- ggplot(data = plotting_data_selected) +
    geom_path(
      mapping = aes(
        x = iteration, y = value,
        color = chain_label
      ),
      alpha = 0.2
    ) +
    geom_smooth(
      mapping = aes(
        x = iteration, y = value,
        color = chain_label
      ),
      se = FALSE, linewidth = 0.8,
      method = "gam",
      formula = y ~ s(x, bs = "cs")
    ) +
    facet_wrap(
      facets = . ~ parameter_label,
      ncol = 3, scales = "free",
      labeller = label_parsed,
      strip.position = "top", dir = "h"
    ) +
    scale_x_continuous(
      name = "Iteration ID (after warmup)",
      breaks = iter_info$breaks,
      labels = function(x) {
        paste0(
          format(
            x = (x - iter_info$shift + 1) / 1e3,
            nsmall = 1, digits = 1
          ),
          "K"
        )
      },
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      name = parse(text = y_name),
      n.breaks = 6,
      labels = make_range_based_labeller(y_label_appendix),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_color_discrete(
      name = "Chain",
      labels = scales::label_parse()
    ) +
    labs(title = title, subtitle = subtitle) +
    theme_bw() +
    theme(
      legend.position = "right",
      strip.background = element_rect(
        fill = "#605CA8", color = "black"
      ),
      strip.text = element_text(
        face = "bold", color = "white"
      ),
      plot.title = element_text(
        face = "bold", color = "black", hjust = 0.5
      ),
      plot.subtitle = element_text(
        color = "black", hjust = 0.5
      )
    )

  return(p)
}

#' Plot Posterior Density Plots for Stan Model Parameters
#'
#' This function generates density plots using ggplot2 from the output of
#' \code{calculate_trace_and_posterior_plotting_data()}.
#'
#' @param plotting_data A \code{list} generated from
#'   \code{calculate_trace_and_posterior_plotting_data()}.
#' @param parameter_set A \code{character} string. One of:
#'   "univariate" (default), "subject_cvs", or "h_set_points".
#' @param title A \code{character} string for the plot title.
#' @param subtitle A \code{character} string for the plot subtitle.
#' @param include_histogram A \code{logical} value. If \code{TRUE}, a histogram
#'   is plotted behind the density curve.
#'
#' @return A \code{ggplot} object.
plot_posterior_density <- function(plotting_data,
                                   parameter_set = "univariate",
                                   title = NULL,
                                   subtitle = NULL,
                                   include_histogram = FALSE) {
  # --- 1. Input Validation ---
  validate_plotting_inputs(plotting_data, parameter_set)

  # Default values
  x_label_appendix <- ""
  x_name <- "CV[p(i)]"

  # --- Density geom builder (shared by univariate
  # individual panels and the faceted path) ---
  add_density_geoms <- function(p, include_histogram) {
    if (include_histogram) {
      p <- p +
        geom_histogram(
          mapping = aes(y = after_stat(density)),
          fill = "#28A745", color = "black",
          bins = 60, alpha = 0.8
        ) +
        geom_density(
          color = "#605CA8", fill = NA,
          linewidth = 1
        )
    } else {
      p <- p +
        geom_density(
          color = "#605CA8", fill = "#28A745",
          linewidth = 1, alpha = 0.8
        )
    }
    p
  }

  if (parameter_set == "univariate") {
    plotting_data_selected <- copy(
      plotting_data$univariate_parameters
    )
    if (is.null(title)) {
      title <- "Posterior Density Plots"
      subtitle <- "Univariate Model Parameters"
    }

    desired_order <- get_univariate_parameter_order()
    plotting_data_selected[, parameter_label := factor(
      parameter_label,
      levels = desired_order
    )]

    # --- Create a list of individual plots ---
    plot_list <- list()

    for (relevant_parameter in desired_order) {
      parameter_data <- plotting_data_selected[
        parameter_label == relevant_parameter,
      ]
      if (nrow(parameter_data) == 0) next

      # x-axis breaks & labels (density uses 5/6/8)
      bl <- get_breaks_and_labels(
        parameter_data$value,
        n_breaks = c(5L, 6L, 8L)
      )

      x_label_appendix <- get_percent_suffix(
        relevant_parameter
      )

      # Summary statistics for reference lines
      parameter_mean <- mean(parameter_data$value)
      parameter_cri <- quantile(
        parameter_data$value,
        probs = c(0.025, 0.975), names = FALSE
      )

      # Hard axis limits
      hard_xlims <- compute_hard_axis_limits(
        parameter_data$value,
        relevant_parameter,
        hard_trim = 0.0025
      )

      # Build density plot for this parameter
      p_individual <- ggplot(
        data = parameter_data, mapping = aes(x = value)
      )
      p_individual <- add_density_geoms(
        p_individual, include_histogram
      )

      p_individual <- p_individual +
        geom_vline(
          xintercept = parameter_mean,
          linetype = "solid", linewidth = 1.25,
          color = "#605CA8"
        ) +
        geom_vline(
          xintercept = c(parameter_cri[1], parameter_cri[2]),
          linetype = "solid", linewidth = 0.6,
          color = "gray"
        ) +
        scale_x_continuous(
          name = parse(text = relevant_parameter),
          breaks = bl$breaks,
          labels = paste0(bl$labels, x_label_appendix),
          expand = expansion(mult = c(0, 0))
        ) +
        scale_y_continuous(
          name = "Density", breaks = NULL,
          expand = expansion(mult = c(0, 0.025))
        ) +
        coord_cartesian(xlim = hard_xlims) +
        theme_classic() +
        theme(legend.position = "none")

      plot_list[[relevant_parameter]] <- p_individual
    }

    return(assemble_titled_cowplot_grid(
      plot_list, title, subtitle
    ))
  } else if (parameter_set == "subject_cvs") {
    plotting_data_selected <- copy(
      plotting_data$subject_specific_cvs
    )
    if (is.null(title)) {
      title <- "Posterior Density Plots"
      subtitle <- "Subject-Specific CVs (%)"
    }
    x_label_appendix <- " %"
    order_parameter_labels(
      plotting_data_selected,
      "^CV\\[p\\(([0-9]+)\\)\\]$"
    )
  } else if (parameter_set == "h_set_points") {
    plotting_data_selected <- copy(
      plotting_data$homeostatic_set_points
    )
    if (is.null(title)) {
      title <- "Posterior Density Plots"
      subtitle <- "Homeostatic Set Points"
    }
    x_name <- "mu[p(i)]"
    order_parameter_labels(
      plotting_data_selected,
      "^mu\\[p\\(([0-9]+)\\)\\]$"
    )
  }

  # --- Faceted plot (subject_cvs / h_set_points) ---

  if (!exists("plotting_data_selected") ||
    nrow(plotting_data_selected) == 0) {
    warning(paste(
      "No data found for parameter_set:",
      parameter_set
    ))
    return(ggplot() +
      labs(title = "No data to plot"))
  }

  p <- ggplot(
    data = plotting_data_selected,
    mapping = aes(x = value)
  )
  p <- add_density_geoms(p, include_histogram)

  p <- p +
    facet_wrap(
      facets = . ~ parameter_label,
      ncol = 3, scales = "free",
      labeller = label_parsed,
      strip.position = "top", dir = "h"
    ) +
    scale_x_continuous(
      name = parse(text = x_name),
      n.breaks = 6,
      labels = make_range_based_labeller(x_label_appendix),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      name = "Density", breaks = NULL,
      expand = expansion(mult = c(0, 0.025))
    ) +
    labs(title = title, subtitle = subtitle) +
    theme_classic() +
    theme(
      legend.position = "none",
      strip.background = element_rect(
        fill = "#605CA8", color = "black"
      ),
      strip.text = element_text(
        face = "bold", color = "white"
      ),
      plot.title = element_text(
        face = "bold", color = "black", hjust = 0.5
      ),
      plot.subtitle = element_text(
        color = "black", hjust = 0.5
      )
    )

  return(p)
}

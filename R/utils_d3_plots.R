#' @title Data Preparation Utilities for D3 Plots
#' @description
#' Convert R data structures (from \code{process_stan_output},
#' \code{calculate_trace_and_posterior_plotting_data},
#' \code{plot_prior_density_plots}, etc.) into JSON-ready lists
#' that the JavaScript \code{GlassD3Plot} engine expects.
#'
#' @name utils_d3_plots
NULL


# -- Constants --------------------------------------------------------

DEFAULT_POINT_COLOR <- "#A7288A"

DEFAULT_GROUP_PALETTE <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c",
  "#d62728", "#9467bd", "#8c564b"
)

CV_PARAMETER_LABELS <- c(
  "CV[A]", "CV[G]", "dCV[P(i)]",
  "E(CV[I])", "SD(CV[I])"
)

Z_975 <- qnorm(0.975)
SQRT_2 <- sqrt(2)


# ====================================================================
# 1. Subject-Specific CVI (Dot-and-Whisker)
# ====================================================================

#' Prepare Data for the Subject-Specific CVI D3 Chart
#'
#' Converts the \code{Subject_Specific} table from
#' \code{process_stan_output()} into the JSON-friendly format
#' expected by the JavaScript plot.
#'
#' @param processed_output A list returned by
#'   \code{process_stan_output()}.
#' @param color_by Optional column name (character) to use for
#'   point colors.
#' @param data Optional data frame with a \code{SubjectID}
#'   column and the \code{color_by} column.
#' @param title Character string for the chart title.
#' @param subtitle Character string for the chart subtitle.
#'
#' @return A list with elements \code{plot_type}, \code{data},
#'   and \code{params}.
#' @export
prepare_subject_cvi_d3 <- function(
    processed_output,
    color_by = NULL,
    data = NULL,
    title = paste(
      "Subject-Specific CVs",
      "with 95% Credible Intervals"
    ),
    subtitle = NULL) {

  subject_data <- data.table::copy(
    processed_output$Subject_Specific
  )
  n_subjects <- nrow(subject_data)

  # -- Assign point colours by group (or default) --
  point_colors <- rep(DEFAULT_POINT_COLOR, n_subjects)

  has_color_info <- !is.null(color_by) && !is.null(data)
  if (has_color_info) {
    point_colors <- resolve_subject_colors(
      subject_data, data, color_by
    )
  }

  # -- Build per-subject list items --
  subjects <- lapply(seq_len(n_subjects), function(idx) {
    row <- subject_data[idx, ]
    list(
      label  = as.character(row$SubjectID),
      median = row$`median_CV_P(i)`,
      mean   = row$`mean_CV_P(i)`,
      lwr    = row$`CV_P(i)_lwr`,
      upr    = row$`CV_P(i)_upr`,
      color  = unname(point_colors[idx])
    )
  })

  population_band <- list(
    median = subject_data$`dCV_P(i)_median`[1],
    lwr    = subject_data$`dCV_P(i)_lwr`[1],
    upr    = subject_data$`dCV_P(i)_upr`[1]
  )

  list(
    plot_type = "subject_cvi",
    data      = list(
      subjects = subjects,
      pop_band = population_band
    ),
    params    = list(title = title, subtitle = subtitle)
  )
}


#' Resolve Subject-Level Colors from a Grouping Column
#'
#' @param subject_data A \code{data.table} with a
#'   \code{SubjectID} column.
#' @param raw_data A data frame with \code{SubjectID} and the
#'   grouping column.
#' @param color_by Character name of the grouping column.
#'
#' @return A named character vector of hex colours, one per row
#'   of \code{subject_data}.
#' @keywords internal
resolve_subject_colors <- function(subject_data,
                                   raw_data,
                                   color_by) {

  n_subjects <- nrow(subject_data)
  fallback   <- rep(DEFAULT_POINT_COLOR, n_subjects)

  input_dt <- data.table::as.data.table(raw_data)
  has_columns <- color_by %in% names(input_dt) &&
    "SubjectID" %in% names(input_dt)

  if (!has_columns) {
    return(fallback)
  }

  group_meta <- unique(
    input_dt[, c("SubjectID", color_by), with = FALSE]
  )
  group_meta[, SubjectID := as.character(SubjectID)]
  subject_data[, SubjectID := as.character(SubjectID)]

  merged <- merge(
    subject_data, group_meta,
    by = "SubjectID", all.x = TRUE
  )

  group_values <- unique(merged[[color_by]])
  color_map <- stats::setNames(
    DEFAULT_GROUP_PALETTE[seq_along(group_values)],
    group_values
  )
  unname(color_map[as.character(merged[[color_by]])])
}


# ====================================================================
# 2. CVI vs. Concentration
# ====================================================================

#' Prepare Data for CVI vs. Concentration D3 Chart
#'
#' @param processed_output A list returned by
#'   \code{process_stan_output()}.
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#'
#' @return A list with \code{plot_type}, \code{data},
#'   \code{params}.
#' @export
prepare_cvi_vs_concentration_d3 <- function(
    processed_output,
    title = paste(
      "Subject-Specific CVs",
      "vs. Concentration"
    ),
    subtitle = NULL) {

  subject_data <- data.table::copy(
    processed_output$Subject_Specific
  )

  subjects <- lapply(
    seq_len(nrow(subject_data)),
    function(idx) {
      row <- subject_data[idx, ]
      list(
        label         = as.character(row$SubjectID),
        concentration = row$`concentration_median_P(i)`,
        median        = row$`median_CV_P(i)`,
        lwr           = row$`CV_P(i)_lwr`,
        upr           = row$`CV_P(i)_upr`,
        color         = DEFAULT_POINT_COLOR
      )
    }
  )

  population_band <- list(
    median = subject_data$`dCV_P(i)_median`[1],
    lwr    = subject_data$`dCV_P(i)_lwr`[1],
    upr    = subject_data$`dCV_P(i)_upr`[1]
  )

  list(
    plot_type = "cvi_vs_concentration",
    data      = list(
      subjects = subjects,
      pop_band = population_band
    ),
    params    = list(title = title, subtitle = subtitle)
  )
}


# ====================================================================
# 3. CVI vs. RCV
# ====================================================================

#' Prepare Data for CVI vs. RCV D3 Chart
#'
#' @param processed_output A list returned by
#'   \code{process_stan_output()}.
#' @param use_log_rcv Logical; use log-based RCV formula?
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#'
#' @return A list with \code{plot_type}, \code{data},
#'   \code{params}.
#' @export
prepare_cvi_vs_rcv_d3 <- function(
    processed_output,
    use_log_rcv = FALSE,
    title = "Subject-Specific RCVs with Uncertainty",
    subtitle = paste(
      "Incorporated from",
      "CV Credible Intervals"
    )) {

  subject_data <- data.table::copy(
    processed_output$Subject_Specific
  )
  summary_table <- processed_output$Summary

  cv_analytical <- as.numeric(
    sub(" .*", "", summary_table$`CVA (95% CrI) %`)
  )
  cv_individual <- as.numeric(
    sub(" .*", "", summary_table$`CVI (95% CrI) %`)
  )

  if (use_log_rcv) {
    subject_data <- compute_log_rcv(
      subject_data, cv_analytical
    )
    population_rcv <- compute_population_log_rcv(
      cv_analytical, cv_individual
    )
  } else {
    subject_data <- compute_standard_rcv(
      subject_data, cv_analytical
    )
    population_rcv <- compute_population_standard_rcv(
      cv_analytical, cv_individual
    )
  }

  subjects <- lapply(
    seq_len(nrow(subject_data)),
    function(idx) {
      row <- subject_data[idx, ]
      list(
        label            = as.character(row$SubjectID),
        cv_median        = row$`median_CV_P(i)`,
        upper_rcv_median = row$upper_rcv_median,
        upper_rcv_lwr    = row$upper_rcv_lwr,
        upper_rcv_upr    = row$upper_rcv_upr,
        lower_rcv_median = row$lower_rcv_median,
        lower_rcv_lwr    = row$lower_rcv_lwr,
        lower_rcv_upr    = row$lower_rcv_upr
      )
    }
  )

  list(
    plot_type = "cvi_vs_rcv",
    data      = list(
      subjects = subjects,
      pop_rcv  = population_rcv
    ),
    params    = list(title = title, subtitle = subtitle)
  )
}


#' Compute Log-Based RCV Columns
#'
#' Adds \code{upper_rcv_*} and \code{lower_rcv_*} columns to
#' \code{subject_data} using the log-normal formula.
#'
#' @param subject_data A \code{data.table} of subject results.
#' @param cv_analytical Numeric scalar; analytical CV (%).
#'
#' @return The modified \code{subject_data} with new columns.
#' @keywords internal
compute_log_rcv <- function(subject_data, cv_analytical) {
  cv_a_frac <- cv_analytical / 100

  subject_data[, sdlog_median := sqrt(
    log(cv_a_frac^2 + (`median_CV_P(i)` / 100)^2 + 1)
  )]
  subject_data[, sdlog_lower := sqrt(
    log(cv_a_frac^2 + (`CV_P(i)_lwr` / 100)^2 + 1)
  )]
  subject_data[, sdlog_upper := sqrt(
    log(cv_a_frac^2 + (`CV_P(i)_upr` / 100)^2 + 1)
  )]

  z_sqrt2 <- Z_975 * SQRT_2

  subject_data[, `:=`(
    upper_rcv_median =
      (exp( z_sqrt2 * sdlog_median) - 1) * 100,
    upper_rcv_lwr    =
      (exp( z_sqrt2 * sdlog_lower)  - 1) * 100,
    upper_rcv_upr    =
      (exp( z_sqrt2 * sdlog_upper)  - 1) * 100,
    lower_rcv_median =
      (exp(-z_sqrt2 * sdlog_median) - 1) * 100,
    lower_rcv_lwr    =
      (exp(-z_sqrt2 * sdlog_lower)  - 1) * 100,
    lower_rcv_upr    =
      (exp(-z_sqrt2 * sdlog_upper)  - 1) * 100
  )]

  subject_data
}


#' Compute Standard (Symmetric) RCV Columns
#'
#' Adds \code{upper_rcv_*} and \code{lower_rcv_*} columns using
#' the standard linear formula.
#'
#' @param subject_data A \code{data.table} of subject results.
#' @param cv_analytical Numeric scalar; analytical CV (%).
#'
#' @return The modified \code{subject_data} with new columns.
#' @keywords internal
compute_standard_rcv <- function(subject_data,
                                 cv_analytical) {
  z_sqrt2 <- Z_975 * SQRT_2
  cv_a_sq <- cv_analytical^2

  subject_data[, `:=`(
    upper_rcv_median =  z_sqrt2 * sqrt(
      cv_a_sq + `median_CV_P(i)`^2
    ),
    upper_rcv_lwr    =  z_sqrt2 * sqrt(
      cv_a_sq + `CV_P(i)_lwr`^2
    ),
    upper_rcv_upr    =  z_sqrt2 * sqrt(
      cv_a_sq + `CV_P(i)_upr`^2
    ),
    lower_rcv_median = -z_sqrt2 * sqrt(
      cv_a_sq + `median_CV_P(i)`^2
    ),
    lower_rcv_lwr    = -z_sqrt2 * sqrt(
      cv_a_sq + `CV_P(i)_lwr`^2
    ),
    lower_rcv_upr    = -z_sqrt2 * sqrt(
      cv_a_sq + `CV_P(i)_upr`^2
    )
  )]

  subject_data
}


#' Compute Population-Level Log-Based RCV
#'
#' @param cv_analytical Numeric. Analytical CV (%).
#' @param cv_individual Numeric. Individual CV (%).
#'
#' @return A list with \code{upper} and \code{lower} RCV.
#' @keywords internal
compute_population_log_rcv <- function(cv_analytical,
                                       cv_individual) {
  sdlog <- sqrt(
    log(
      (cv_analytical / 100)^2 +
        (cv_individual / 100)^2 + 1
    )
  )
  z_sqrt2 <- Z_975 * SQRT_2
  list(
    upper = (exp( z_sqrt2 * sdlog) - 1) * 100,
    lower = (exp(-z_sqrt2 * sdlog) - 1) * 100
  )
}


#' Compute Population-Level Standard RCV
#'
#' @param cv_analytical Numeric. Analytical CV (%).
#' @param cv_individual Numeric. Individual CV (%).
#'
#' @return A list with \code{upper} and \code{lower} RCV.
#' @keywords internal
compute_population_standard_rcv <- function(cv_analytical,
                                            cv_individual) {
  z_sqrt2    <- Z_975 * SQRT_2
  combined   <- sqrt(cv_analytical^2 + cv_individual^2)
  list(
    upper =  z_sqrt2 * combined,
    lower = -z_sqrt2 * combined
  )
}


# ====================================================================
# 4. Trace Plots
# ====================================================================

#' Prepare Data for Trace D3 Plots
#'
#' Converts the output of
#' \code{calculate_trace_and_posterior_plotting_data()} into
#' the JSON-friendly list for the D3 trace plot engine.
#'
#' @param plotting_data A list from
#'   \code{calculate_trace_and_posterior_plotting_data()}.
#' @param parameter_set One of \code{"univariate"},
#'   \code{"subject_cvs"}, \code{"h_set_points"}.
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#' @param max_points_per_chain Maximum trace points per chain
#'   per panel. Longer traces are thinned by even sampling.
#'   Defaults to 500 to keep JSON payload manageable.
#'
#' @return A list with \code{plot_type}, \code{data},
#'   \code{params}.
#' @export
prepare_trace_d3 <- function(
    plotting_data,
    parameter_set = "univariate",
    title = NULL,
    subtitle = NULL,
    max_points_per_chain = 500L) {

  resolved <- resolve_parameter_data(
    plotting_data, parameter_set, chart_type = "trace"
  )
  trace_data    <- resolved$data
  title         <- title %||% resolved$title
  desired_order <- resolved$order

  panels <- lapply(desired_order, function(param_label) {
    parameter_data <- trace_data[
      parameter_label == param_label
    ]
    if (nrow(parameter_data) == 0L) {
      return(NULL)
    }

    traces <- build_thinned_traces(
      parameter_data, max_points_per_chain
    )

    is_cv_param <- param_label %in% CV_PARAMETER_LABELS ||
      parameter_set == "subject_cvs"

    list(
      parameter = param_label,
      label     = param_label,
      is_cv     = is_cv_param,
      traces    = traces
    )
  })

  panels <- Filter(Negate(is.null), panels)

  list(
    plot_type = "trace",
    data      = list(panels = panels),
    params    = list(
      title    = title,
      subtitle = subtitle,
      ncols    = 3L
    )
  )
}


#' Build Thinned Trace Data for All Chains
#'
#' @param parameter_data A \code{data.table} filtered to a
#'   single parameter, with columns \code{chain},
#'   \code{iteration}, \code{value}.
#' @param max_points Maximum number of points per chain.
#'
#' @return A list of trace point lists
#'   (iteration, value, chain).
#' @keywords internal
build_thinned_traces <- function(parameter_data,
                                 max_points) {

  chain_ids <- unique(parameter_data$chain)
  traces    <- list()

  for (current_chain in chain_ids) {
    chain_data <- parameter_data[chain == current_chain]
    data.table::setorder(chain_data, iteration)

    if (nrow(chain_data) > max_points) {
      thin_indices <- round(
        seq(1, nrow(chain_data), length.out = max_points)
      )
      chain_data <- chain_data[thin_indices]
    }

    for (row_idx in seq_len(nrow(chain_data))) {
      traces[[length(traces) + 1L]] <- list(
        iteration = chain_data$iteration[row_idx],
        value     = chain_data$value[row_idx],
        chain     = as.character(current_chain)
      )
    }
  }

  traces
}


# ====================================================================
# 5. Posterior Density
# ====================================================================

#' Prepare Data for Posterior Density D3 Plots
#'
#' @param plotting_data A list from
#'   \code{calculate_trace_and_posterior_plotting_data()}.
#' @param parameter_set One of \code{"univariate"},
#'   \code{"subject_cvs"}, \code{"h_set_points"}.
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#' @param include_histogram Logical; overlay a histogram?
#' @param max_samples Maximum number of samples per panel. If
#'   more samples exist they are thinned. Defaults to 5000.
#'
#' @return A list with \code{plot_type}, \code{data},
#'   \code{params}.
#' @export
prepare_posterior_density_d3 <- function(
    plotting_data,
    parameter_set = "univariate",
    title = NULL,
    subtitle = NULL,
    include_histogram = FALSE,
    max_samples = 5000L) {

  resolved <- resolve_parameter_data(
    plotting_data, parameter_set,
    chart_type = "posterior"
  )
  posterior_data <- resolved$data
  title          <- title %||% resolved$title
  subtitle       <- subtitle %||% resolved$subtitle
  desired_order  <- resolved$order

  panels <- lapply(desired_order, function(param_label) {
    parameter_data <- posterior_data[
      parameter_label == param_label
    ]
    if (nrow(parameter_data) == 0L) {
      return(NULL)
    }

    values <- parameter_data$value
    if (length(values) > max_samples) {
      thin_indices <- round(
        seq(1, length(values), length.out = max_samples)
      )
      values <- values[thin_indices]
    }

    is_cv_param <- param_label %in% CV_PARAMETER_LABELS ||
      parameter_set == "subject_cvs"

    list(
      parameter = param_label,
      label     = param_label,
      is_cv     = is_cv_param,
      values    = as.numeric(values)
    )
  })

  panels <- Filter(Negate(is.null), panels)

  list(
    plot_type = "posterior",
    data      = list(panels = panels),
    params    = list(
      title             = title,
      subtitle          = subtitle,
      ncols             = 3L,
      include_histogram = include_histogram
    )
  )
}


# ====================================================================
# Shared helpers for trace / posterior
# ====================================================================

#' Resolve Parameter Data, Title, and Ordering
#'
#' Shared logic for \code{prepare_trace_d3()} and
#' \code{prepare_posterior_density_d3()} to select the correct
#' data subset and determine parameter ordering.
#'
#' @param plotting_data The raw plotting data list.
#' @param parameter_set One of \code{"univariate"},
#'   \code{"subject_cvs"}, \code{"h_set_points"}.
#' @param chart_type Either \code{"trace"} or
#'   \code{"posterior"}.
#'
#' @return A list with \code{data}, \code{title},
#'   \code{subtitle}, and \code{order}.
#' @keywords internal
resolve_parameter_data <- function(plotting_data,
                                   parameter_set,
                                   chart_type) {

  univariate_order <- c(
    "beta", "df[I]", "df[A]", "CV[A]", "CV[G]",
    "dCV[P(i)]", "E(CV[I])", "SD(CV[I])"
  )

  if (parameter_set == "univariate") {
    param_data <- data.table::copy(
      plotting_data$univariate_parameters
    )
    title <- if (chart_type == "trace") {
      "Trace Plots for the Univariate Model Parameters"
    } else {
      "Posterior Density Plots"
    }
    subtitle <- if (chart_type == "posterior") {
      "Univariate Model Parameters"
    } else {
      NULL
    }
    desired_order <- univariate_order

  } else if (parameter_set == "subject_cvs") {
    param_data <- data.table::copy(
      plotting_data$subject_specific_cvs
    )
    title <- if (chart_type == "trace") {
      "Trace Plots for the Subject-Specific CVs"
    } else {
      "Posterior Density Plots"
    }
    subtitle <- if (chart_type == "posterior") {
      "Subject-Specific CVs (%)"
    } else {
      NULL
    }
    desired_order <- sort_parameter_labels(
      param_data,
      pattern = "^CV\\[p\\(([0-9]+)\\)\\]$"
    )

  } else if (parameter_set == "h_set_points") {
    param_data <- data.table::copy(
      plotting_data$homeostatic_set_points
    )
    title <- if (chart_type == "trace") {
      "Trace Plots for the Homeostatic Set Points"
    } else {
      "Posterior Density Plots"
    }
    subtitle <- if (chart_type == "posterior") {
      "Homeostatic Set Points"
    } else {
      NULL
    }
    desired_order <- sort_parameter_labels(
      param_data,
      pattern = "^mu\\[p\\(([0-9]+)\\)\\]$"
    )

  } else {
    stop(
      "Invalid parameter_set: '", parameter_set,
      "'. Expected 'univariate', 'subject_cvs', ",
      "or 'h_set_points'."
    )
  }

  list(
    data     = param_data,
    title    = title,
    subtitle = subtitle,
    order    = desired_order
  )
}


#' Sort Parameter Labels by Embedded Numeric Index
#'
#' @param param_data A \code{data.table} with a
#'   \code{parameter_label} column.
#' @param pattern A regex with one capture group that extracts
#'   the numeric index.
#'
#' @return A character vector of labels in numeric order.
#' @keywords internal
sort_parameter_labels <- function(param_data, pattern) {
  labels  <- unique(param_data$parameter_label)
  indices <- as.numeric(gsub(pattern, "\\1", labels))
  labels[order(indices)]
}


# ====================================================================
# 6. Prior Density
# ====================================================================

#' Prepare Data for Prior Density D3 Plots
#'
#' Samples from the prior distributions and converts the
#' samples into a format suitable for the D3 prior density
#' chart.  Mirrors the R \code{plot_prior_density_plots()}.
#'
#' @param beta Expected value of beta.
#' @param cvi Expected E[CV_I] as a percentage (e.g. 10).
#' @param cva Expected CV_A as a percentage.
#' @param cvg Expected CV_G as a percentage.
#' @param dfi Expected df_I.
#' @param dfa Expected df_A.
#' @param hbhr Expected HBHR as a percentage.
#' @param strength Numeric vector of length 7.
#' @param log_transformed Logical.
#' @param model Character. \code{"NTT"} or \code{"NTTDFGAM"}.
#' @param n_samples Number of Monte Carlo samples.
#'   Defaults to 50 000.
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#'
#' @return A list with \code{plot_type}, \code{data},
#'   \code{params}.
#' @export
prepare_prior_density_d3 <- function(
    beta,
    cvi = 10,
    cva = 3,
    cvg = 20,
    dfi = 20,
    dfa = 20,
    hbhr = 50,
    strength = c(1, 1, 1, 1, 1, 1, 0.667),
    log_transformed = FALSE,
    model = "NTT",
    n_samples = 50000L,
    title = "Prior Density Plots",
    subtitle = NULL) {

  # -- Compute hyperparameters (same as Stan model) --
  hyperparams <- process_stan_data_priors(
    beta = beta,
    cvi  = cvi * (dfi - 2) / dfi,
    cva  = cva * (dfa - 2) / dfa,
    cvg  = cvg,
    dfi  = dfi,
    dfa  = dfa,
    hbhr = hbhr,
    strength = strength,
    log_transformed = log_transformed
  )

  hbhr_mean <- hbhr / 100
  hbhr_sd   <- strength[7] * hbhr_mean

  # -- Draw prior samples --
  prior_samples <- draw_prior_samples(
    hyperparams     = hyperparams,
    n_samples       = n_samples,
    model           = model,
    hbhr_mean       = hbhr_mean,
    hbhr_sd         = hbhr_sd,
    log_transformed = log_transformed,
    beta            = beta
  )

  # -- Transform to user-friendly CV scale --
  user_scale <- transform_prior_to_user_scale(
    prior_samples,
    log_transformed = log_transformed,
    beta = beta
  )

  # -- Assemble panel list --
  panels <- list(
    build_prior_panel(
      "beta", "\u03B2", FALSE, user_scale$beta
    ),
    build_prior_panel(
      "dfi", "df[I]", FALSE, user_scale$dfi
    ),
    build_prior_panel(
      "dfa", "df[A]", FALSE, user_scale$dfa
    ),
    build_prior_panel(
      "cvi", "CV[i]", TRUE, user_scale$cvi
    ),
    build_prior_panel(
      "cva", "CV[A]", TRUE, user_scale$cva
    ),
    build_prior_panel(
      "cvg", "CV[G]", TRUE, user_scale$cvg
    ),
    build_prior_panel(
      "cvi_mean", "E(CV[I])", TRUE, user_scale$cvi_mean
    ),
    build_prior_panel(
      "cvi_sd", "SD(CV[I])", TRUE, user_scale$cvi_sd
    ),
    build_prior_panel(
      "hbhr", "HBHR", TRUE, user_scale$hbhr
    )
  )

  list(
    plot_type = "prior",
    data      = list(panels = panels),
    params    = list(
      title    = title,
      subtitle = subtitle,
      ncols    = 3L
    )
  )
}


# -- Prior sampling helpers ------------------------------------------

#' Draw Monte Carlo Samples from Effective Priors
#'
#' @param hyperparams Numeric vector of hyperparameter values.
#' @param n_samples Number of draws.
#' @param model \code{"NTT"} or \code{"NTTDFGAM"}.
#' @param hbhr_mean Mean of the HBHR prior on [0, 1] scale.
#' @param hbhr_sd SD of the HBHR prior on [0, 1] scale.
#' @param log_transformed Logical.
#' @param beta Numeric. Expected beta value.
#'
#' @return A named list of sample vectors.
#' @keywords internal
draw_prior_samples <- function(hyperparams,
                               n_samples,
                               model,
                               hbhr_mean,
                               hbhr_sd,
                               log_transformed,
                               beta) {

  hp <- hyperparams

  beta_eff <- rnorm(
    n_samples, mean = hp[[1]], sd = hp[[2]]
  )
  cvi_mean_eff <- sample_truncnorm(
    n_samples, hp[[3]], hp[[4]]
  )
  cvi_sd_eff <- sample_truncnorm(
    n_samples, hp[[5]], hp[[6]]
  )
  cva_eff <- sample_truncnorm(
    n_samples, hp[[7]], hp[[8]]
  )
  cvg_eff <- sample_truncnorm(
    n_samples, hp[[9]], hp[[10]]
  )

  # Degrees of freedom: gamma vs truncated-normal
  if (model == "NTTDFGAM") {
    dfi_eff <- sample_gamma_ms(
      n_samples, hp[[11]], hp[[12]]
    ) + 2
    dfa_eff <- sample_gamma_ms(
      n_samples, hp[[13]], hp[[14]]
    ) + 2
  } else {
    dfi_eff <- sample_truncnorm(
      n_samples, hp[[11]], hp[[12]], lower = 0
    ) + 2
    dfa_eff <- sample_truncnorm(
      n_samples, hp[[13]], hp[[14]], lower = 0
    ) + 2
  }

  # Hierarchical CVI: sigma_i ~ TN(cvi_mean, cvi_sd)
  cvi_eff <- sample_truncnorm(
    n_samples, cvi_mean_eff, cvi_sd_eff
  )

  # HBHR samples (scaled to %)
  hbhr_samples <- sample_truncnorm(
    n_samples, hbhr_mean, hbhr_sd
  ) * 100

  # Beta on user scale
  beta_user <- if (log_transformed) {
    exp(beta_eff)
  } else {
    beta_eff
  }

  list(
    beta     = beta_user,
    cvi_mean = cvi_mean_eff,
    cvi_sd   = cvi_sd_eff,
    cva      = cva_eff,
    cvg      = cvg_eff,
    dfi      = dfi_eff,
    dfa      = dfa_eff,
    cvi      = cvi_eff,
    hbhr     = hbhr_samples
  )
}


#' Transform Prior Samples to User-Friendly CV Scale
#'
#' @param samples Named list of prior sample vectors (from
#'   \code{draw_prior_samples()}).
#' @param log_transformed Logical.
#' @param beta Numeric. Expected beta value.
#'
#' @return A named list of user-scale sample vectors.
#' @keywords internal
transform_prior_to_user_scale <- function(samples,
                                          log_transformed,
                                          beta) {

  dfi <- samples$dfi
  dfa <- samples$dfa

  if (log_transformed) {
    logt_to_cv <- function(sigma, df) {
      sqrt(exp(sigma^2 * df / (df - 2)) - 1) * 100
    }
    lognormal_to_cv <- function(sigma) {
      sqrt(exp(sigma^2) - 1) * 100
    }

    cvi_mean <- logt_to_cv(samples$cvi_mean, dfi)
    cvi_sd   <- logt_to_cv(samples$cvi_sd, dfi)
    cvi      <- logt_to_cv(samples$cvi, dfi)
    cva      <- logt_to_cv(samples$cva, dfa)
    cvg      <- lognormal_to_cv(samples$cvg)
  } else {
    lst_to_sd <- function(sigma, df) {
      sigma * sqrt(df / (df - 2))
    }
    sd_to_cv <- function(sigma, mean_val) {
      sigma / mean_val * 100
    }

    cvi_mean <- sd_to_cv(
      lst_to_sd(samples$cvi_mean, dfi), beta
    )
    cvi_sd <- sd_to_cv(
      lst_to_sd(samples$cvi_sd, dfi), beta
    )
    cvi <- sd_to_cv(
      lst_to_sd(samples$cvi, dfi), beta
    )
    cva <- sd_to_cv(
      lst_to_sd(samples$cva, dfa), beta
    )
    cvg <- sd_to_cv(samples$cvg, beta)
  }

  list(
    beta     = samples$beta,
    dfi      = dfi,
    dfa      = dfa,
    cvi_mean = cvi_mean,
    cvi_sd   = cvi_sd,
    cvi      = cvi,
    cva      = cva,
    cvg      = cvg,
    hbhr     = samples$hbhr
  )
}


#' Build a Single Prior Panel Entry
#'
#' @param parameter Parameter identifier string.
#' @param label Display label for the panel.
#' @param is_cv Logical; is this a CV parameter?
#' @param values Numeric vector of samples.
#' @param max_display Maximum values to include in output.
#'
#' @return A list suitable for the \code{panels} array.
#' @keywords internal
build_prior_panel <- function(parameter,
                              label,
                              is_cv,
                              values,
                              max_display = 5000L) {
  list(
    parameter = parameter,
    label     = label,
    is_cv     = is_cv,
    values    = clean_finite_values(values, max_display)
  )
}


#' Remove Non-Finite Values and Optionally Thin
#'
#' @param x Numeric vector.
#' @param max_length Maximum length of the returned vector.
#'
#' @return A numeric vector with only finite values, thinned
#'   if necessary.
#' @keywords internal
clean_finite_values <- function(x, max_length = 5000L) {
  x <- x[is.finite(x)]
  if (length(x) > max_length) {
    thin_indices <- round(
      seq(1, length(x), length.out = max_length)
    )
    x <- x[thin_indices]
  }
  as.numeric(x)
}


# -- Distribution sampling helpers ----------------------------------

#' Sample from a Truncated Normal Distribution
#'
#' Wrapper around \code{truncnorm::rtruncnorm()} with sensible
#' defaults for non-negative support.
#'
#' @param n Number of samples.
#' @param mean Mean of the untruncated normal.
#' @param sd Standard deviation of the untruncated normal.
#' @param lower Lower truncation bound (default 0).
#' @param upper Upper truncation bound (default Inf).
#'
#' @return Numeric vector of length \code{n}.
#' @keywords internal
sample_truncnorm <- function(n, mean, sd, lower = 0, upper = Inf) {
  truncnorm::rtruncnorm(
    n = n,
    a = lower,
    b = upper,
    mean = mean,
    sd = sd
  )
}


#' Sample from a Gamma Distribution (Mean / SD Form)
#'
#' Parameterises the gamma distribution by its mean and
#' standard deviation, then draws samples.
#'
#' @param n Number of samples.
#' @param mean Mean of the gamma distribution.
#' @param sd Standard deviation of the gamma distribution.
#'
#' @return Numeric vector of length \code{n}.
#' @keywords internal
sample_gamma_ms <- function(n, mean, sd) {
  shape <- mean^2 / sd^2
  rate  <- mean / sd^2
  rgamma(n, shape = shape, rate = rate)
}


# ====================================================================
# 7. Exploration Scatter (Interactive Click-to-Exclude)
# ====================================================================

#' Prepare Data for the Exploration Scatter D3 Chart
#'
#' Converts analysis data + exclusion info into a JSON-friendly
#' list for the interactive scatter plot that supports
#' click-to-exclude / click-to-restore.
#'
#' @param data A \code{data.table} of \strong{all filtered data}
#'   (before exclusions), with columns \code{y},
#'   \code{SubjectID}, \code{SampleID}, \code{ReplicateID}.
#' @param excluded_rows A \code{data.table} with keys
#'   \code{SubjectID}, \code{SampleID}, \code{ReplicateID}.
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#'
#' @return A list with \code{plot_type}, \code{data},
#'   \code{params}.
#' @export
prepare_exploration_scatter_d3 <- function(
    data,
    excluded_rows = data.table::data.table(),
    title = "Data Points \u2014 Click to Exclude / Include",
    subtitle = "Each point is one measurement",
    view_mode = "combined") {

  dt <- data.table::copy(data)
  dt[, obs_idx := .I]

  # --- Colour palette by Subject ---
  subjects <- unique(as.character(dt$SubjectID))

  # Use a larger palette for many subjects
  base_palette <- c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
    "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5",
    "#c49c94", "#f7b6d2", "#c7c7c7", "#dbdb8d", "#9edae5"
  )
  palette  <- rep(base_palette, length.out = length(subjects))
  color_map <- stats::setNames(palette, subjects)

  # --- Mark excluded rows ---
  dt[, excluded := FALSE]
  if (!is.null(excluded_rows) &&
      nrow(excluded_rows) > 0 &&
      "SubjectID" %in% names(excluded_rows)) {
    excl <- data.table::copy(excluded_rows)
    excl[, .excl_flag := TRUE]
    dt <- merge(
      dt, excl,
      by = c("SubjectID", "SampleID", "ReplicateID"),
      all.x = TRUE
    )
    dt[is.na(.excl_flag), .excl_flag := FALSE]
    dt[, excluded := .excl_flag]
    dt[, .excl_flag := NULL]
    data.table::setorder(dt, obs_idx) # preserve original order
  }

  points <- lapply(seq_len(nrow(dt)), function(i) {
    row <- dt[i]
    list(
      idx         = row$obs_idx,
      y           = row$y,
      SubjectID   = as.character(row$SubjectID),
      SampleID    = as.character(row$SampleID),
      ReplicateID = as.character(row$ReplicateID),
      excluded    = row$excluded,
      color       = unname(color_map[as.character(row$SubjectID)])
    )
  })

  list(
    plot_type = "exploration_scatter",
    data      = list(
      points     = points,
      grand_mean = mean(dt$y, na.rm = TRUE)
    ),
    params = list(
      title     = title,
      subtitle  = subtitle,
      view_mode = view_mode
    )
  )
}


# ====================================================================
# 8. Descriptive Dot-plot (by Subject)
# ====================================================================

#' Prepare Data for the Descriptive Dot-plot D3 Chart
#'
#' Creates a horizontal strip/dot chart grouped by subject,
#' with per-subject mean diamonds and SD whiskers.
#'
#' @param data A \code{data.table} of analysis data.
#' @param subject_means A \code{data.table} with columns
#'   \code{SubjectID}, \code{Mean}, \code{SD} (from
#'   \code{compute_descriptive_stats()$subject_summary}).
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#'
#' @return A list with \code{plot_type}, \code{data},
#'   \code{params}.
#' @export
prepare_descriptive_dotplot_d3 <- function(
    data,
    subject_means,
    title = "Data Distribution by Subject",
    subtitle = "Individual measurements with subject means") {

  dt <- data.table::copy(data)

  subjects <- unique(as.character(dt$SubjectID))
  palette  <- rep(DEFAULT_GROUP_PALETTE,
                  length.out = length(subjects))
  color_map <- stats::setNames(palette, subjects)

  points <- lapply(seq_len(nrow(dt)), function(i) {
    row <- dt[i]
    list(
      SubjectID = as.character(row$SubjectID),
      y         = row$y,
      color     = unname(color_map[as.character(row$SubjectID)])
    )
  })

  subj_means <- lapply(seq_len(nrow(subject_means)), function(i) {
    row <- subject_means[i]
    list(
      SubjectID = as.character(row$SubjectID),
      mean      = row$Mean,
      sd        = row$SD,
      color     = unname(color_map[as.character(row$SubjectID)])
    )
  })

  list(
    plot_type = "descriptive_dotplot",
    data      = list(
      points        = points,
      subject_means = subj_means,
      grand_mean    = mean(dt$y, na.rm = TRUE),
      grand_sd      = sd(dt$y, na.rm = TRUE)
    ),
    params = list(title = title, subtitle = subtitle)
  )
}


# ====================================================================
# 9. ANOVA Components (Horizontal Bar Chart)
# ====================================================================

#' Prepare Data for the ANOVA Components D3 Chart
#'
#' Takes the output of \code{compute_anova_estimates()} and
#' packages it for the D3 horizontal bar chart.
#'
#' @param anova_results A list returned by
#'   \code{compute_anova_estimates()}.
#' @param title Chart title.
#' @param subtitle Chart subtitle.
#'
#' @return A list with \code{plot_type}, \code{data},
#'   \code{params}.
#' @export
prepare_anova_components_d3 <- function(
    anova_results,
    title = "ANOVA-based Variance Components",
    subtitle = "Classical nested ANOVA estimates (CV %)") {

  components <- list(
    list(
      label = "CV_A (Analytical)",
      value = anova_results$CV_A,
      lower = anova_results$CV_A_lower,
      upper = anova_results$CV_A_upper,
      color = "#1f77b4"
    ),
    list(
      label = "CV_I (Within-Individual)",
      value = anova_results$CV_I,
      lower = anova_results$CV_I_lower,
      upper = anova_results$CV_I_upper,
      color = "#ff7f0e"
    ),
    list(
      label = "CV_G (Between-Individual)",
      value = anova_results$CV_G,
      lower = anova_results$CV_G_lower,
      upper = anova_results$CV_G_upper,
      color = "#2ca02c"
    )
  )

  list(
    plot_type = "anova_components",
    data      = list(components = components),
    params    = list(title = title, subtitle = subtitle)
  )
}

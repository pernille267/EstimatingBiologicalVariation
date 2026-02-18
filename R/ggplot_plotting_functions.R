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
#' @import ggplot2
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
#'
#' @import ggplot2
#' @export
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
#'
#' @importFrom ggplot2 ggplot aes geom_density geom_histogram geom_vline
#'   scale_x_continuous scale_y_continuous coord_cartesian
#'   theme_classic theme labs
#' @export
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

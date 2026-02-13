# Prepare Data for Stan Modeling
# -------------------------------------------------------

#' Prepare data for Stan modeling
#'
#' @param data Data table with biological variation data
#' @param hypers List of hyperparameters
#' @param hyper_strength Numeric strength of hyperparameters
#' @param log_transformed Logical indicating if log transformation
#'  should be applied
#' @return List of prepared data for Stan
pernille_prepares_for_stan_modeling <- function(data, hypers, hyper_strength, log_transformed = FALSE) {
  data.table::setDT(data)
  data.table::setorder(data, SubjectID, SampleID, ReplicateID)

  stan_data_prep <- process_stan_data_indexing(
    SubjectID_orig_R = data$SubjectID,
    SampleID_orig_R = data$SampleID,
    y_R = if (log_transformed) {
      log(data$y)
    } else {
      data$y
    }
  )

  stan_data_prep_priors <- process_stan_data_priors(
    beta = hypers$beta,
    cvi = hypers$cvi,
    cva = hypers$cva,
    cvg = hypers$cvg,
    dfi = hypers$dfi,
    dfa = hypers$dfa,
    hbhr = hypers$hbhr,
    log_transformed = log_transformed,
    strength = hyper_strength
  )
  c(stan_data_prep, stan_data_prep_priors)
}

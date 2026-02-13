// --------------------------------------------------------------------------
// process_stan_data.cpp
//
// Rcpp routines for preparing data for Stan models.
// These replace the former biovar::process_stan_data_indexing() and
// biovar::process_stan_data_priors() functions.
// --------------------------------------------------------------------------

#include <Rcpp.h>
#include <string>
#include <vector>
#include <unordered_map>
#include <numeric>   // std::accumulate
#include <algorithm> // std::max

using namespace Rcpp;

//' @title Process Stan Data Indexing
//'
//' @description Converts subject and sample
//'   identifiers into the integer index arrays
//'   required by hierarchical Stan models.
//'
//' @param SubjectID_orig_R Character vector of
//'   subject identifiers, one per observation.
//' @param SampleID_orig_R Integer vector of
//'   within-subject sample numbers (1-based).
//'   Must be positive integers.
//' @param y_R Numeric vector of response values.
//'   Passed through unchanged; must be the same
//'   length as the other inputs.
//'
//' @return A named \code{list} with elements:
//' \describe{
//'   \item{N_obs}{Integer. Total number of
//'     observations.}
//'   \item{y}{Numeric vector. Response variable
//'     (unchanged from input).}
//'   \item{N_subj}{Integer. Number of unique
//'     subjects.}
//'   \item{subj_idx}{Integer vector. 1-based
//'     subject index for each observation.}
//'   \item{N_samp_total}{Integer. Total number
//'     of samples across all subjects.}
//'   \item{samp_idx}{Integer vector. Global
//'     1-based sample index for each
//'     observation.}
//'   \item{sample_to_subj_map}{Integer vector.
//'     Maps each global sample index to its
//'     1-based subject index.}
//' }
//'
//' @details
//' All three input vectors must have the same
//' length. The function:
//' \itemize{
//'   \item Maps character subject IDs to
//'     consecutive integers (1 to N_subj).
//'   \item Determines the maximum sample number
//'     per subject.
//'   \item Builds a global sample index using
//'     cumulative offsets.
//'   \item Creates a reverse map from global
//'     sample indices back to subjects.
//' }
//'
//' @examples
//' \dontrun{
//' subj <- c("A", "A", "B", "B", "B", "C")
//' samp <- c(1L, 2L, 1L, 2L, 3L, 1L)
//' y    <- c(1.2, 1.5, 2.1, 2.3, 2.0, 0.8)
//'
//' result <- process_stan_data_indexing(
//'   subj, samp, y
//' )
//' str(result)
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::List process_stan_data_indexing(
    Rcpp::CharacterVector SubjectID_orig_R,
    Rcpp::IntegerVector  SampleID_orig_R,
    Rcpp::NumericVector  y_R)
{
  const int num_obs = SubjectID_orig_R.length();

  // --- input validation ------------------------------
  if (num_obs == 0) {
    Rcpp::stop("Input data is empty.");
  }
  if (SampleID_orig_R.length() != num_obs ||
      y_R.length() != num_obs) {
    Rcpp::stop(
      "Input vectors must have the same length."
    );
  }

  // Raw pointer avoids IntegerVector proxy overhead
  const int *sample_id = INTEGER(SampleID_orig_R);

  // --- 1. Map subject labels to 1-based indices ------
  Rcpp::IntegerVector subject_index(num_obs);
  int *subj_ptr = INTEGER(subject_index);

  std::unordered_map<std::string, int> label_to_index;
  label_to_index.reserve(num_obs / 2);
  int num_subjects = 0;

  for (int i = 0; i < num_obs; ++i) {
    std::string label(SubjectID_orig_R[i]);
    auto result = label_to_index.emplace(
      std::move(label), num_subjects + 1);
    if (result.second) {
      ++num_subjects;
    }
    subj_ptr[i] = result.first->second;
  }

  // --- 2. Max sample ID per subject ------------------
  std::vector<int> max_sample_per_subject(
    num_subjects, 0);

  for (int i = 0; i < num_obs; ++i) {
    const int subj_0 = subj_ptr[i] - 1;
    max_sample_per_subject[subj_0] = std::max(
      max_sample_per_subject[subj_0],
      sample_id[i]);
  }

  // --- 3. Total samples across all subjects ----------
  const int num_samples_total = std::accumulate(
    max_sample_per_subject.begin(),
    max_sample_per_subject.end(), 0);

  if (num_samples_total == 0) {
    Rcpp::stop(
      "Total samples is 0. "
      "SampleID values must be > 0."
    );
  }

  // --- 4. Cumulative offsets for global indexing ------
  std::vector<int> cumulative_offset(num_subjects);
  cumulative_offset[0] = 0;
  for (int s = 1; s < num_subjects; ++s) {
    cumulative_offset[s] =
      cumulative_offset[s - 1] +
      max_sample_per_subject[s - 1];
  }

  // --- 5. Global sample index per observation --------
  Rcpp::IntegerVector sample_index(num_obs);
  int *samp_ptr = INTEGER(sample_index);

  for (int i = 0; i < num_obs; ++i) {
    const int subj_0 = subj_ptr[i] - 1;
    samp_ptr[i] =
      cumulative_offset[subj_0] + sample_id[i];
  }

  // --- 6. Map global sample -> subject ---------------
  Rcpp::IntegerVector sample_to_subject_map(
    num_samples_total);
  int *map_ptr = INTEGER(sample_to_subject_map);

  int write_pos = 0;
  for (int s = 0; s < num_subjects; ++s) {
    const int n_samp   = max_sample_per_subject[s];
    const int subj_1b  = s + 1;
    for (int k = 0; k < n_samp; ++k) {
      map_ptr[write_pos++] = subj_1b;
    }
  }

  // --- return Stan data list -------------------------
  return Rcpp::List::create(
    Rcpp::Named("N_obs")  = num_obs,
    Rcpp::Named("y")      = y_R,
    Rcpp::Named("N_subj") = num_subjects,
    Rcpp::Named("subj_idx")    = subject_index,
    Rcpp::Named("N_samp_total") = num_samples_total,
    Rcpp::Named("samp_idx")    = sample_index,
    Rcpp::Named("sample_to_subj_map") =
      sample_to_subject_map);
}

#include <Rcpp.h>
using namespace Rcpp;

//' @title Generate Stan Data List for Priors
//'
//' @name process_stan_data_priors
//'
//' @param beta A \code{double} representing the expected
//'   value of the main parameter.
//' @param cvi Expected CV for individual-level variance
//'   (I), as a percentage (e.g., 10 for 10\%). Must be
//'   strictly positive. Defaults to 10.
//' @param cva Expected CV for assay-level variance (A),
//'   as a percentage. Must be strictly positive.
//'   Defaults to 3.
//' @param cvg Expected CV for group-level variance (G),
//'   as a percentage. Must be strictly positive.
//'   Defaults to 20.
//' @param dfi Expected degrees of freedom for the
//'   individual-level t-distribution. Must be >= 2.1.
//'   Defaults to 9999.
//' @param dfa Expected degrees of freedom for the
//'   assay-level t-distribution. Must be >= 2.1.
//'   Defaults to 9999.
//' @param hbhr Expected Harris-Brown heterogeneity
//'   ratio, as a percentage. Must be > 0.
//'   Defaults to 50.
//' @param strength A \code{NumericVector} of length 7
//'   with non-negative multipliers controlling the SD
//'   of each hyperprior. Elements map to:
//'   \enumerate{
//'     \item \code{beta}
//'     \item \code{sigma_I} (mean)
//'     \item \code{sigma_A}
//'     \item \code{sigma_G}
//'     \item \code{df_I}
//'     \item \code{df_A}
//'     \item \code{hbhr}
//'   }
//'   Defaults to \code{c(1, 1, 1, 1, 1, 1, 2/3)}.
//' @param log_transformed Logical; if \code{TRUE},
//'   priors for \code{sigma_I}, \code{sigma_A}, and
//'   \code{sigma_G} are computed on the log scale.
//'   Defaults to \code{FALSE}.
//'
//' @description
//' Converts user-friendly prior specifications into
//' hyperparameters (means and SDs) for a Stan model.
//'
//' @details
//' Takes an expected value (\code{beta}), coefficients
//' of variation, degrees of freedom, and a
//' heterogeneity ratio, then derives the location and
//' scale parameters for the Stan prior distributions.
//' The \code{strength} vector scales each prior SD
//' independently.
//'
//' @return A named \code{list} of Stan hyperparameters:
//' \describe{
//'   \item{\code{prior_beta_mean}}{
//'     Mean for the prior on \code{beta}.}
//'   \item{\code{prior_beta_sd}}{
//'     SD for the prior on \code{beta}.}
//'   \item{\code{prior_sigma_i_mean_mean}}{
//'     Mean of the hyperprior on E[\code{sigma_I}].}
//'   \item{\code{prior_sigma_i_mean_sd}}{
//'     SD of the hyperprior on E[\code{sigma_I}].}
//'   \item{\code{prior_sigma_i_sd_mean}}{
//'     Mean of the hyperprior on SD[\code{sigma_I}].}
//'   \item{\code{prior_sigma_i_sd_sd}}{
//'     SD of the hyperprior on SD[\code{sigma_I}].}
//'   \item{\code{prior_sigma_A_mean}}{
//'     Mean for the prior on \code{sigma_A}.}
//'   \item{\code{prior_sigma_A_sd}}{
//'     SD for the prior on \code{sigma_A}.}
//'   \item{\code{prior_sigma_G_mean}}{
//'     Mean for the prior on \code{sigma_G}.}
//'   \item{\code{prior_sigma_G_sd}}{
//'     SD for the prior on \code{sigma_G}.}
//'   \item{\code{prior_df_I_mean}}{
//'     Mean for the prior on \code{df_I}.}
//'   \item{\code{prior_df_I_sd}}{
//'     SD for the prior on \code{df_I}.}
//'   \item{\code{prior_df_A_mean}}{
//'     Mean for the prior on \code{df_A}.}
//'   \item{\code{prior_df_A_sd}}{
//'     SD for the prior on \code{df_A}.}
//' }
//'
//' @examples
//' # Default priors for beta = 100
//' process_stan_data_priors(beta = 100)
//'
//' # Log-transformed model with tighter CVs
//' process_stan_data_priors(
//'   beta = 7, cvi = 5, cva = 2,
//'   log_transformed = TRUE
//' )
//'
//' # Wider beta prior (strength[1] = 2)
//' process_stan_data_priors(
//'   beta = 100,
//'   strength = c(2, 1, 1, 1, 1, 1, 2/3)
//' )
//'
//' @export
// [[Rcpp::export]]
List process_stan_data_priors(
    double beta,
    double cvi = 10.0,
    double cva = 3.0,
    double cvg = 20.0,
    double dfi = 9999.0,
    double dfa = 9999.0,
    double hbhr = 50.0,
    NumericVector strength =
        NumericVector::create(1, 1, 1, 1, 1, 1, 0.667),
    bool log_transformed = false)
{

    // --- input validation --------------------------------
    if (strength.size() != 7)
    {
        Rcpp::stop("'strength' must be of length 7.");
    }

    // Direct pointer avoids NumericVector proxy overhead
    const double *s = REAL(strength);

    for (int i = 1; i < 7; ++i)
    {
        if (s[i] < 0.0)
        {
            Rcpp::stop(
                "strength[2:7] must be non-negative. "
                "strength[1] (beta) may be zero or positive.");
        }
    }
    if (cvi <= 0.0)
    {
        Rcpp::stop(
            "cvi (hyper for E[CV_I]) must be > 0.");
    }
    if (cva <= 0.0)
    {
        Rcpp::stop(
            "cva (hyper for E[CV_A]) must be > 0.");
    }
    if (cvg <= 0.0)
    {
        Rcpp::stop(
            "cvg (hyper for E[CV_G]) must be > 0.");
    }
    if (dfi < 2.1)
    {
        Rcpp::stop(
            "dfi (hyper for E[df_I]) must be >= 2.1.");
    }
    if (dfa < 2.1)
    {
        Rcpp::stop(
            "dfa (hyper for E[df_A]) must be >= 2.1.");
    }
    if (hbhr <= 0.0)
    {
        Rcpp::stop(
            "hbhr (hyper for E[HBHR]) must be > 0.");
    }

    // --- convert percentages to proportions --------------
    const double cv_i = cvi * 0.01;
    const double cv_a = cva * 0.01;
    const double cv_g = cvg * 0.01;
    const double hr = hbhr * 0.01;

    // --- derive location parameters ----------------------
    double beta_prior, sig_I, sig_A, sig_G;

    if (log_transformed)
    {
        beta_prior = std::log(beta);
        // std::log1p(x) = log(1+x), more accurate for
        // small x than std::log(x*x + 1)
        sig_I = std::sqrt(std::log1p(cv_i * cv_i));
        sig_A = std::sqrt(std::log1p(cv_a * cv_a));
        sig_G = std::sqrt(std::log1p(cv_g * cv_g));
    }
    else
    {
        beta_prior = beta;
        sig_I = cv_i * beta_prior;
        sig_A = cv_a * beta_prior;
        sig_G = cv_g * beta_prior;
    }

    // --- build output list -------------------------------
    return List::create(
        Named("prior_beta_mean") = beta_prior,
        Named("prior_beta_sd") = s[0] * std::fabs(beta_prior),
        Named("prior_sigma_i_mean_mean") = sig_I,
        Named("prior_sigma_i_mean_sd") = s[1] * sig_I,
        Named("prior_sigma_i_sd_mean") = hr * sig_I,
        Named("prior_sigma_i_sd_sd") = s[6] * sig_I,
        Named("prior_sigma_A_mean") = sig_A,
        Named("prior_sigma_A_sd") = s[2] * sig_A,
        Named("prior_sigma_G_mean") = sig_G,
        Named("prior_sigma_G_sd") = s[3] * sig_G,
        Named("prior_df_I_mean") = dfi - 2.0,
        Named("prior_df_I_sd") = s[4] * dfi,
        Named("prior_df_A_mean") = dfa - 2.0,
        Named("prior_df_A_sd") = s[5] * dfa);
}

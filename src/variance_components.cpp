// --------------------------------------------------------------------------
// variance_components.cpp
//
// Rcpp routines for biological variation estimation via nested ANOVA.
//
// Exported functions:
//   bv_anova()                 – One-way / two-way nested ANOVA
//   variance_components()      – Variance-component point estimates & CIs
//   bv_anova_bootstrap_ci()    – Percentile bootstrap CIs
// --------------------------------------------------------------------------

#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <numeric>
#include <unordered_map>
#include <vector>

using namespace Rcpp;

// ==========================================================================
// Helper functions
// ==========================================================================

// Mean, skipping NAs
inline double mean_no_na(const Rcpp::NumericVector &x)
{
    double running_sum = 0.0;
    int valid_count = 0;
    for (int i = 0; i < x.size(); ++i)
    {
        if (!ISNAN(x[i]))
        {
            running_sum += x[i];
            ++valid_count;
        }
    }
    return (valid_count == 0) ? NA_REAL : running_sum / valid_count;
}

// Standard deviation, skipping NAs (sample SD, denominator n-1)
inline double sd_no_na(const Rcpp::NumericVector &x)
{
    double mean_value = mean_no_na(x);
    if (ISNAN(mean_value))
        return NA_REAL;
    double sum_of_squared_deviations = 0.0;
    int valid_count = 0;
    for (int i = 0; i < x.size(); ++i)
    {
        if (!ISNAN(x[i]))
        {
            double diff = x[i] - mean_value;
            sum_of_squared_deviations += diff * diff;
            ++valid_count;
        }
    }
    return (valid_count < 2) ? NA_REAL : std::sqrt(sum_of_squared_deviations / (valid_count - 1));
}

// ==========================================================================
// bv_anova – One-way & two-way nested ANOVA for biological variation data
// ==========================================================================

//' Nested ANOVA for Biological Variation Data
//'
//' @name bv_anova
//'
//' @description
//' Fits one-way (per-subject) and two-way (overall) nested
//' random-effects ANOVA models to biological variation data.
//' Decomposes total variance into between-subject,
//' within-subject, and analytical (residual) components
//' using unweighted sums of squares.
//'
//' @param data A \code{list} (or \code{data.table}) with the
//'   following columns or elements, each of equal length:
//'   \describe{
//'     \item{\code{SubjectID}}{\code{integer} vector.
//'       Identifies the individual or participant.}
//'     \item{\code{SampleID}}{\code{integer} vector.
//'       Identifies the specimen collected from a subject.}
//'     \item{\code{ReplicateID}}{\code{integer} vector.
//'       Identifies the replicate measurement within a
//'       sample.}
//'     \item{\code{y}}{\code{numeric} vector. The measured
//'       analyte values.  \code{NA} values are silently
//'       excluded.}
//'   }
//' @param cv_anova \code{logical}.  When \code{TRUE}, each
//'   subject's measurements are divided by that subject's
//'   mean before fitting the ANOVA, yielding a CV-ANOVA
//'   whose variance components estimate coefficients of
//'   variation directly.  Default is \code{FALSE}.
//'
//' @details
//' The three-level nested random-effects model is:
//'
//' \deqn{y_{isr} = \mu + G_i + I_{is} + A_{isr}}
//'
//' where
//' \itemize{
//'   \item \eqn{G_i \sim N(0,\, \sigma_G^2)} is the
//'     between-subject effect,
//'   \item \eqn{I_{is} \sim N(0,\, \sigma_I^2)} is the
//'     within-subject (between-sample) effect, and
//'   \item \eqn{A_{isr} \sim N(0,\, \sigma_A^2)} is the
//'     analytical (residual / replicate) error.
//' }
//'
//' The function computes both weighted and unweighted sums
//' of squares and returns the weighting coefficients
//' \eqn{w_{1U}}, \eqn{w_{2U}}, and \eqn{w_{3U}} needed
//' to recover variance-component point estimates from the
//' unweighted mean squares.  It also provides per-subject
//' estimates of \eqn{\sigma_I} and \eqn{\sigma_A}, which
//' are used to compute the Heterogeneity of Biological
//' Homeostatic Ratio (HBHR).
//'
//' @return A named \code{list} with:
//'   \describe{
//'     \item{\code{beta}}{Grand mean (\eqn{\mu}).}
//'     \item{\code{S1U_squared}}{Unweighted mean square
//'       between subjects.}
//'     \item{\code{S2U_squared}}{Unweighted mean square
//'       between samples within subjects.}
//'     \item{\code{S3_squared}}{Mean square within samples
//'       (analytical / residual).}
//'     \item{\code{n1}}{Degrees of freedom: between
//'       subjects (\eqn{I - 1}).}
//'     \item{\code{n2}}{Degrees of freedom: between
//'       samples within subjects
//'       (\eqn{\sum_i J_i - I}).}
//'     \item{\code{n3}}{Degrees of freedom: within
//'       samples (\eqn{N - \sum_i J_i}).}
//'     \item{\code{w1U, w2U, w3U}}{Weighting coefficients
//'       for the unweighted sums of squares.}
//'     \item{\code{sigma_i}}{\code{numeric} vector of
//'       per-subject within-subject SD estimates.}
//'     \item{\code{sigma_a}}{\code{numeric} vector of
//'       per-subject analytical SD estimates.}
//'     \item{\code{HBHR}}{Harris-Brown Heterogeneity Ratio (\%).}
//'   }
//'
//' @seealso \code{\link{variance_components}} for
//'   extracting point estimates and confidence intervals;
//'   \code{\link{bv_anova_bootstrap_ci}} for bootstrap
//'   confidence intervals.
//'
//' @examples
//' \dontrun{
//' dat       <- simulate_bv_data(
//'   15, 10, 2, 10, 2, 50, 100
//' )
//' anova_fit <- bv_anova(dat)
//' anova_fit$beta        # grand mean
//' anova_fit$S3_squared  # analytical mean square
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::List bv_anova(Rcpp::List data, bool cv_anova = false)
{

    // --- Unpack data --------------------------------------------------------
    Rcpp::IntegerVector subjects = data["SubjectID"];
    Rcpp::IntegerVector samples = data["SampleID"];
    Rcpp::NumericVector values_raw = data["y"];
    Rcpp::NumericVector values = clone(values_raw);

    int total_obs_count = values.size();
    if (total_obs_count == 0)
        Rcpp::stop("Input data is empty.");

    // --- Build sort index (subject → sample → row order) --------------------
    std::vector<int> sort_indices(total_obs_count);
    std::iota(sort_indices.begin(), sort_indices.end(), 0);
    std::sort(sort_indices.begin(), sort_indices.end(), [&](int a, int b)
              {
    if (subjects[a] != subjects[b]) return subjects[a] < subjects[b];
    if (samples[a]  != samples[b])  return samples[a]  < samples[b];
    return a < b; });

    // --- Grand mean & optional CV-ANOVA scaling -----------------------------
    double grand_mean = mean_no_na(values);
    if (ISNAN(grand_mean))
        Rcpp::stop("Grand mean is NA (all values missing?).");

    if (cv_anova)
    {
        // Divide each subject's measurements by that subject's mean
        int pre_cursor = 0;
        while (pre_cursor < total_obs_count)
        {
            while (pre_cursor < total_obs_count && ISNAN(values[sort_indices[pre_cursor]]))
                ++pre_cursor;
            if (pre_cursor >= total_obs_count)
                break;

            int current_subj = subjects[sort_indices[pre_cursor]];
            std::vector<int> subj_indices;
            double subj_sum = 0.0;
            int subj_count = 0;
            int scan = pre_cursor;

            while (scan < total_obs_count && subjects[sort_indices[scan]] == current_subj)
            {
                double val = values[sort_indices[scan]];
                if (!ISNAN(val))
                {
                    subj_indices.push_back(sort_indices[scan]);
                    subj_sum += val;
                    ++subj_count;
                }
                ++scan;
            }

            if (subj_count > 0)
            {
                double subj_mean = subj_sum / subj_count;
                if (subj_mean != 0.0)
                {
                    for (int idx : subj_indices)
                        values[idx] /= subj_mean;
                }
            }

            pre_cursor = scan;
        }
        grand_mean = 1.0;
    }

    // --- SST & effective N --------------------------------------------------
    double ss_total = 0.0;
    long long nonmissing_obs_count = 0;
    for (int k = 0; k < total_obs_count; ++k)
    {
        if (!ISNAN(values[k]))
        {
            double diff = values[k] - grand_mean;
            ss_total += diff * diff;
            ++nonmissing_obs_count;
        }
    }
    if (nonmissing_obs_count == 0)
        Rcpp::stop("No non-NA observations found in 'y'.");

    // --- Accumulators for the two-way ANOVA ---------------------------------
    double ss_between_subjects = 0.0;
    double ss_samples_within_subjects = 0.0;
    double ss_replicates_within_samples = 0.0;

    // Unweighted sums of squares accumulator
    double ss_unweighted_samples_acc = 0.0;

    int n_unique_subjects = 0;
    int n_total_unique_samples = 0;

    // Coefficients for unweighted SS
    double weight_w1u_numerator = 0.0;
    double weight_w1u_denominator = 0.0;
    double weight_w2u_sum_x_squared = 0.0;
    double weight_w3u_numerator = 0.0;
    double weight_w3u_denominator = 0.0;

    // Per-subject vectors
    Rcpp::NumericVector ss_samples_per_subject;
    Rcpp::NumericVector ss_analytical_per_subject;
    Rcpp::NumericVector ms_samples_per_subject;
    Rcpp::NumericVector ms_analytical_per_subject;
    Rcpp::IntegerVector df_samples_per_subject;
    Rcpp::IntegerVector df_analytical_per_subject;

    // For the unweighted between-subject SS
    std::vector<double> subject_unweighted_means;
    double unweighted_grand_mean_sum = 0.0;

    // --- Main loop over sorted data (subject → sample) ---------------------
    int row_cursor = 0;
    while (row_cursor < total_obs_count)
    {

        // Skip leading NAs
        while (row_cursor < total_obs_count && ISNAN(values[sort_indices[row_cursor]]))
            ++row_cursor;
        if (row_cursor >= total_obs_count)
            break;

        ++n_unique_subjects;
        int current_subject_id = subjects[sort_indices[row_cursor]];

        // Accumulators for current subject
        double subject_value_sum = 0.0;
        int subject_total_replicates = 0;
        int subject_sample_count = 0;
        double subject_inverse_replicate_sum = 0.0;
        double subject_sample_mean_sum = 0.0;

        std::vector<double> subject_values;       // all non-NA y for this subject
        std::vector<double> sample_means;         // per-sample means
        std::vector<int> sample_replicate_counts; // per-sample replicate counts

        // -- Iterate over samples within this subject --------------------------
        while (row_cursor < total_obs_count && subjects[sort_indices[row_cursor]] == current_subject_id)
        {

            // Skip NAs within the subject block
            while (row_cursor < total_obs_count &&
                   subjects[sort_indices[row_cursor]] == current_subject_id &&
                   ISNAN(values[sort_indices[row_cursor]]))
                ++row_cursor;
            if (row_cursor >= total_obs_count || subjects[sort_indices[row_cursor]] != current_subject_id)
                break;

            ++subject_sample_count;
            ++n_total_unique_samples;
            int sample_start_pos = row_cursor;
            int current_sample_id = samples[sort_indices[row_cursor]];

            double sample_value_sum = 0.0;
            int replicate_count = 0;

            // -- Iterate over replicates within this sample ----------------------
            while (row_cursor < total_obs_count &&
                   subjects[sort_indices[row_cursor]] == current_subject_id &&
                   samples[sort_indices[row_cursor]] == current_sample_id)
            {
                double observed_value = values[sort_indices[row_cursor]];
                if (!ISNAN(observed_value))
                {
                    sample_value_sum += observed_value;
                    subject_values.push_back(observed_value);
                    ++replicate_count;
                }
                ++row_cursor;
            }

            if (replicate_count > 0)
            {
                double sample_mean = sample_value_sum / replicate_count;
                sample_means.push_back(sample_mean);
                sample_replicate_counts.push_back(replicate_count);

                subject_sample_mean_sum += sample_mean;
                subject_inverse_replicate_sum += 1.0 / replicate_count;

                // Accumulate SS3 (within-sample residual)
                // Note: rows [sample_start_pos, row_cursor) already belong to
                // the current subject/sample, so only the NA check is needed.
                for (int r = sample_start_pos; r < row_cursor; ++r)
                {
                    double val = values[sort_indices[r]];
                    if (!ISNAN(val))
                    {
                        double diff = val - sample_mean;
                        ss_replicates_within_samples += diff * diff;
                    }
                }

                subject_value_sum += sample_value_sum;
                subject_total_replicates += replicate_count;
            }
            else
            {
                // All replicates for this sample were NA – discard it
                --subject_sample_count;
                --n_total_unique_samples;
            }
        } // end samples for subject

        // -- Aggregate the current subject's contributions ---------------------
        if (subject_total_replicates > 0 && subject_sample_count > 0)
        {
            double subject_weighted_mean = subject_value_sum / subject_total_replicates;

            // SS1 (between subjects, weighted)
            double diff_grand = subject_weighted_mean - grand_mean;
            ss_between_subjects += subject_total_replicates * diff_grand * diff_grand;

            // SS2 (between samples within subject)
            double ss_between_samples_for_subject = 0.0;
            for (size_t j = 0; j < sample_means.size(); ++j)
            {
                double diff = sample_means[j] - subject_weighted_mean;
                ss_between_samples_for_subject += sample_replicate_counts[j] * diff * diff;
            }
            ss_samples_within_subjects += ss_between_samples_for_subject;
            ss_samples_per_subject.push_back(ss_between_samples_for_subject);

            // SSa (residual within subject)
            double ss_total_for_subject = 0.0;
            for (double v : subject_values)
            {
                double diff = v - subject_weighted_mean;
                ss_total_for_subject += diff * diff;
            }
            double ss_analytical_for_subject = ss_total_for_subject - ss_between_samples_for_subject;
            ss_analytical_per_subject.push_back(ss_analytical_for_subject);

            // Per-subject degrees of freedom & mean squares
            int df_between_samples_subj = subject_sample_count - 1;
            int df_analytical_subj = subject_total_replicates - subject_sample_count;
            df_samples_per_subject.push_back(df_between_samples_subj);
            df_analytical_per_subject.push_back(df_analytical_subj);
            ms_samples_per_subject.push_back((df_between_samples_subj > 0) ? ss_between_samples_for_subject / df_between_samples_subj : NA_REAL);
            ms_analytical_per_subject.push_back((df_analytical_subj > 0) ? ss_analytical_for_subject / df_analytical_subj : NA_REAL);

            // --- Unweighted quantities for SS1U / SS2U --------------------------
            double subject_unweighted_mean = subject_sample_mean_sum / subject_sample_count;
            subject_unweighted_means.push_back(subject_unweighted_mean);
            unweighted_grand_mean_sum += subject_unweighted_mean;

            double ss_unweighted_for_subject = 0.0;
            for (double sm : sample_means)
            {
                double diff = sm - subject_unweighted_mean;
                ss_unweighted_for_subject += diff * diff;
            }
            ss_unweighted_samples_acc += ss_unweighted_for_subject;

            // Harmonic mean of replicates and coefficient accumulators
            double harmonic_mean_replicates = (subject_inverse_replicate_sum > 0.0)
                                                  ? static_cast<double>(subject_sample_count) / subject_inverse_replicate_sum
                                                  : 0.0;
            if (harmonic_mean_replicates > 0.0)
            {
                double num_samples_double = static_cast<double>(subject_sample_count);
                double weight_coefficient_x = 1.0 / (num_samples_double * harmonic_mean_replicates);
                weight_w1u_numerator += 1.0 / num_samples_double;
                weight_w1u_denominator += weight_coefficient_x;
                weight_w2u_sum_x_squared += weight_coefficient_x * weight_coefficient_x;

                double weight_coefficient_y = (num_samples_double - 1.0) / num_samples_double;
                weight_w3u_numerator += weight_coefficient_y;
                weight_w3u_denominator += weight_coefficient_y / harmonic_mean_replicates;
            }
        }
        else if (subject_total_replicates == 0)
        {
            --n_unique_subjects;
        }
    } // end subject loop

    // --- Guard: no valid subjects -------------------------------------------
    if (n_unique_subjects == 0)
    {
        Rcpp::warning("No subjects with valid data found.");
        return Rcpp::List::create(Rcpp::Named("Error") = "No valid subjects.");
    }

    // --- Degrees of freedom -------------------------------------------------
    int df_between_subjects = std::max(0, n_unique_subjects - 1);
    int df_between_samples = std::max(0, n_total_unique_samples - n_unique_subjects);
    int df_within_samples = std::max(0, static_cast<int>(nonmissing_obs_count) - n_total_unique_samples);

    // --- Unweighted grand mean & SS1U --------------------------------------
    double unweighted_grand_mean = (n_unique_subjects > 0)
                                       ? unweighted_grand_mean_sum / n_unique_subjects
                                       : 0.0;
    double ss_unweighted_between_subjects_raw = 0.0;
    for (double m : subject_unweighted_means)
    {
        double diff = m - unweighted_grand_mean;
        ss_unweighted_between_subjects_raw += diff * diff;
    }

    // --- w-coefficients -----------------------------------------------------
    double num_subjects_double = static_cast<double>(n_unique_subjects);
    double w1U = NA_REAL, w2U = NA_REAL, w3U = NA_REAL;

    if (weight_w1u_denominator != 0.0)
        w1U = weight_w1u_numerator / weight_w1u_denominator;

    if (df_between_subjects > 0 && num_subjects_double > 0 && weight_w1u_denominator != 0.0)
    {
        double sum_weight_x = weight_w1u_denominator;
        double sum_weight_x_squared = weight_w2u_sum_x_squared;
        double numerator = num_subjects_double - sum_weight_x_squared / (sum_weight_x * sum_weight_x);
        double denominator = (1.0 / num_subjects_double) * sum_weight_x;
        if (denominator != 0.0)
            w2U = (1.0 / df_between_subjects) * numerator / denominator;
    }

    if (df_between_samples > 0 && weight_w3u_denominator != 0.0)
        w3U = weight_w3u_numerator / weight_w3u_denominator;
    else if (df_between_samples > 0 && weight_w3u_numerator == 0.0 && weight_w3u_denominator == 0.0)
        w3U = 0.0;

    // --- Final unweighted SS ------------------------------------------------
    double ss_unweighted_between_subjects = ISNAN(w2U) ? NA_REAL : ss_unweighted_between_subjects_raw * w2U;
    double ss_unweighted_between_samples = ISNAN(w3U) ? NA_REAL : ss_unweighted_samples_acc * w3U;

    // --- Mean squares -------------------------------------------------------
    double ms_between_subjects = (df_between_subjects > 0) ? ss_between_subjects / df_between_subjects : NA_REAL;
    double ms_between_samples = (df_between_samples > 0) ? ss_samples_within_subjects / df_between_samples : NA_REAL;
    double ms_within_samples = (df_within_samples > 0) ? ss_replicates_within_samples / df_within_samples : NA_REAL;
    double ms_unweighted_between_subjects = (df_between_subjects > 0 && !ISNAN(ss_unweighted_between_subjects)) ? ss_unweighted_between_subjects / df_between_subjects : NA_REAL;
    double ms_unweighted_between_samples = (df_between_samples > 0 && !ISNAN(ss_unweighted_between_samples)) ? ss_unweighted_between_samples / df_between_samples : NA_REAL;

    // --- Per-subject sigma estimates ----------------------------------------
    Rcpp::NumericVector sigma_within_per_subject(n_unique_subjects);
    Rcpp::NumericVector sigma_analytical_per_subject(n_unique_subjects);

    for (int i = 0; i < n_unique_subjects; ++i)
    {
        // Analytical SD per subject
        if (i < ms_analytical_per_subject.size() && !ISNAN(ms_analytical_per_subject[i]))
            sigma_analytical_per_subject[i] = std::sqrt(ms_analytical_per_subject[i]);
        else
            sigma_analytical_per_subject[i] = NA_REAL;

        // Within-subject SD per subject
        if (i < ms_samples_per_subject.size() &&
            i < ms_analytical_per_subject.size() &&
            !ISNAN(ms_samples_per_subject[i]) &&
            !ISNAN(ms_analytical_per_subject[i]) &&
            !ISNAN(w3U) && w3U != 0.0)
        {
            double variance_estimate = (ms_samples_per_subject[i] - ms_analytical_per_subject[i]) / w3U;
            sigma_within_per_subject[i] = (variance_estimate >= 0.0) ? std::sqrt(variance_estimate) : NA_REAL;
        }
        else
        {
            sigma_within_per_subject[i] = NA_REAL;
        }
    }

    // --- HBHR (Heterogeneity of Biological Homeostatic Ratio) ---------------
    double heterogeneity_ratio = NA_REAL;
    if (sigma_within_per_subject.size() > 0)
    {
        double mean_sigma_within = mean_no_na(sigma_within_per_subject);
        if (!ISNAN(mean_sigma_within) && mean_sigma_within != 0.0)
        {
            double sd_sigma_within = sd_no_na(sigma_within_per_subject);
            if (!ISNAN(sd_sigma_within))
                heterogeneity_ratio = sd_sigma_within / mean_sigma_within * 100.0;
        }
        else if (!ISNAN(mean_sigma_within) && mean_sigma_within == 0.0)
        {
            double sd_sigma_within = sd_no_na(sigma_within_per_subject);
            if (!ISNAN(sd_sigma_within) && sd_sigma_within == 0.0)
                heterogeneity_ratio = 0.0;
        }
    }

    // --- Return ------------------------------------------------------------
    return Rcpp::List::create(
        Rcpp::Named("beta") = grand_mean,
        Rcpp::Named("S1U_squared") = ms_unweighted_between_subjects,
        Rcpp::Named("S2U_squared") = ms_unweighted_between_samples,
        Rcpp::Named("S3_squared") = ms_within_samples,
        Rcpp::Named("n1") = df_between_subjects,
        Rcpp::Named("n2") = df_between_samples,
        Rcpp::Named("n3") = df_within_samples,
        Rcpp::Named("w1U") = w1U,
        Rcpp::Named("w2U") = w2U,
        Rcpp::Named("w3U") = w3U,
        Rcpp::Named("sigma_i") = sigma_within_per_subject,
        Rcpp::Named("sigma_a") = sigma_analytical_per_subject,
        Rcpp::Named("HBHR") = heterogeneity_ratio);
}

// ==========================================================================
// variance_components – Point estimates & confidence intervals
// ==========================================================================

//' Variance-Component Point Estimates and Confidence
//' Intervals
//'
//' @name variance_components
//'
//' @description
//' Extracts variance-component standard deviations
//' (or CVs) from a nested ANOVA fit, with optional
//' confidence intervals based on the generalised
//' inference approach of Burdick & Graybill (1992).
//'
//' @param data A \code{list} (or \code{data.frame})
//'   passed to \code{\link{bv_anova}}.  Must contain
//'   \code{SubjectID}, \code{SampleID},
//'   \code{ReplicateID}, and \code{y}.
//' @param output_type \code{character} string selecting
//'   the type of output.  One of:
//'   \describe{
//'     \item{\code{"sigma"}}{Point-estimate SDs.}
//'     \item{\code{"cv"}}{Point-estimate CVs
//'       (SDs divided by the grand mean).}
//'     \item{\code{"sigma_ci"}}{SDs with
//'       \eqn{(1-\alpha)} confidence intervals.}
//'     \item{\code{"cv_ci"}}{CVs with
//'       \eqn{(1-\alpha)} confidence intervals.}
//'   }
//'   Default is \code{"sigma"}.
//' @param mult \code{numeric} scaling factor applied to
//'   every estimate and CI bound.  For example, use
//'   \code{mult = 100} to express CVs as percentages.
//'   Default is \code{1}.
//' @param level \code{numeric} confidence level for CIs,
//'   between 0 and 1.  Ignored when \code{output_type}
//'   is \code{"sigma"} or \code{"cv"}.
//'   Default is \code{0.95}.
//' @param cv_anova \code{logical}.  When \code{TRUE},
//'   the underlying ANOVA is run on subject-mean-scaled
//'   values (CV-ANOVA).  Default is \code{FALSE}.
//'
//' @details
//' For \code{output_type = "sigma"} or \code{"cv"},
//' the function returns scalar point estimates of
//' \eqn{\sigma_A} (analytical), \eqn{\sigma_I}
//' (within-subject), and \eqn{\sigma_G}
//' (between-subject).
//'
//' For \code{output_type = "sigma_ci"} or
//' \code{"cv_ci"}, each component is a length-3
//' \code{numeric} vector
//' \code{[estimate, lower, upper]}.  The CI for
//' \eqn{\sigma_A} uses a chi-squared pivot; CIs for
//' \eqn{\sigma_I} and \eqn{\sigma_G} follow the
//' generalised confidence-interval method using
//' approximate F-distribution pivots (Burdick &
//' Graybill, 1992).
//'
//' Negative variance estimates are truncated to zero
//' before taking square roots.
//'
//' @return A named \code{list} with:
//'   \describe{
//'     \item{\code{sigma_A}}{Analytical SD (or CV).
//'       Scalar or length-3 vector depending on
//'       \code{output_type}.}
//'     \item{\code{sigma_I}}{Within-subject SD
//'       (or CV).  Same structure as above.}
//'     \item{\code{sigma_G}}{Between-subject SD
//'       (or CV).  Same structure as above.}
//'     \item{\code{sigma_i}}{\code{numeric} vector
//'       of per-subject within-subject SD (or CV)
//'       estimates.}
//'     \item{\code{sigma_a}}{\code{numeric} vector
//'       of per-subject analytical SD (or CV)
//'       estimates.}
//'     \item{\code{HBHR}}{Heterogeneity of Biological
//'       Homeostatic Ratio (\%).}
//'     \item{\code{beta}}{Grand mean
//'       (\eqn{\hat\mu}).}
//'   }
//'
//' @references
//' Burdick, R. K. and Graybill, F. A. (1992).
//' \emph{Confidence Intervals on Variance Components}.
//' Marcel Dekker, New York.
//'
//' @seealso \code{\link{bv_anova}} for the underlying
//'   ANOVA fit; \code{\link{bv_anova_bootstrap_ci}} for
//'   non-parametric bootstrap CIs.
//'
//' @examples
//' \dontrun{
//' dat <- simulate_bv_data(
//'   15, 10, 2, 10, 2, 50, 100
//' )
//' # Point-estimate SDs
//' variance_components(dat, "sigma")
//'
//' # CVs (%) with 95% CIs
//' variance_components(
//'   dat, "cv_ci", mult = 100, level = 0.95
//' )
//' }
//'
//' @export
// [[Rcpp::export]]
Rcpp::List variance_components(List data,
                               std::string output_type = "sigma",
                               double mult = 1.0,
                               double level = 0.95,
                               bool cv_anova = false)
{

    // --- Run underlying ANOVA -----------------------------------------------
    List anova = bv_anova(data, cv_anova);

    double ms_unweighted_between_subjects = Rcpp::as<double>(anova["S1U_squared"]);
    double ms_unweighted_between_samples = Rcpp::as<double>(anova["S2U_squared"]);
    double ms_within_samples = Rcpp::as<double>(anova["S3_squared"]);
    double w1U = Rcpp::as<double>(anova["w1U"]);
    double w2U = Rcpp::as<double>(anova["w2U"]);
    double w3U = Rcpp::as<double>(anova["w3U"]);
    double grand_mean = Rcpp::as<double>(anova["beta"]);

    Rcpp::NumericVector sigma_within_per_subject = Rcpp::clone(
        Rcpp::as<Rcpp::NumericVector>(anova["sigma_i"]));
    Rcpp::NumericVector sigma_analytical_per_subject = Rcpp::clone(
        Rcpp::as<Rcpp::NumericVector>(anova["sigma_a"]));
    double HBHR = Rcpp::as<double>(anova["HBHR"]);

    // --- Point estimates for variance-component SDs -------------------------
    double sigma_analytical_estimate = NA_REAL;
    double sigma_within_subject_estimate = NA_REAL;
    double sigma_between_subject_estimate = NA_REAL;
    double coeff_c2 = NA_REAL, coeff_c3 = NA_REAL;

    // sigma_A (analytical / residual)
    if (!ISNAN(ms_within_samples) && ms_within_samples >= 0.0)
        sigma_analytical_estimate = std::sqrt(ms_within_samples);

    // sigma_I (within-subject)
    if (!ISNAN(w1U) && !ISNAN(w3U) && w3U != 0.0)
    {
        coeff_c2 = w1U / w3U;
        coeff_c3 = coeff_c2 - 1.0;
        if (!ISNAN(ms_unweighted_between_samples) && !ISNAN(ms_within_samples))
        {
            double variance_within_subject = (ms_unweighted_between_samples - ms_within_samples) / w3U;
            sigma_within_subject_estimate = (variance_within_subject >= 0.0) ? std::sqrt(variance_within_subject) : 0.0;
        }
    }

    // sigma_G (between-subject)
    if (!ISNAN(w2U) && w2U != 0.0 && !ISNAN(coeff_c2) && !ISNAN(coeff_c3) &&
        !ISNAN(ms_unweighted_between_subjects) && !ISNAN(ms_unweighted_between_samples) && !ISNAN(ms_within_samples))
    {
        double variance_between_subject = (ms_unweighted_between_subjects - coeff_c2 * ms_unweighted_between_samples + coeff_c3 * ms_within_samples) / w2U;
        sigma_between_subject_estimate = (variance_between_subject >= 0.0) ? std::sqrt(variance_between_subject) : 0.0;
    }

    // -----------------------------------------------------------------------
    // Output: "sigma" or "cv" (point estimates only)
    // -----------------------------------------------------------------------
    if (output_type == "sigma" || output_type == "cv")
    {
        bool is_cv = (output_type == "cv");

        if (is_cv && (ISNAN(grand_mean) || grand_mean == 0.0))
        {
            Rcpp::warning("Grand mean is NA or zero; CVs will be NA.");
            for (int i = 0; i < sigma_within_per_subject.size(); ++i)
            {
                sigma_within_per_subject[i] = NA_REAL;
                sigma_analytical_per_subject[i] = NA_REAL;
            }
            return Rcpp::List::create(
                Rcpp::Named("sigma_A") = NA_REAL,
                Rcpp::Named("sigma_I") = NA_REAL,
                Rcpp::Named("sigma_G") = NA_REAL,
                Rcpp::Named("sigma_i") = sigma_within_per_subject,
                Rcpp::Named("sigma_a") = sigma_analytical_per_subject,
                Rcpp::Named("HBHR") = HBHR,
                Rcpp::Named("beta") = grand_mean);
        }

        double scaling_denominator = is_cv ? grand_mean : 1.0;
        for (int i = 0; i < sigma_within_per_subject.size(); ++i)
        {
            if (!ISNAN(sigma_within_per_subject[i]))
                sigma_within_per_subject[i] = sigma_within_per_subject[i] / scaling_denominator * mult;
            if (!ISNAN(sigma_analytical_per_subject[i]))
                sigma_analytical_per_subject[i] = sigma_analytical_per_subject[i] / scaling_denominator * mult;
        }
        auto scale = [&](double x) -> double
        {
            return ISNAN(x) ? NA_REAL : x / scaling_denominator * mult;
        };
        return Rcpp::List::create(
            Rcpp::Named("sigma_A") = scale(sigma_analytical_estimate),
            Rcpp::Named("sigma_I") = scale(sigma_within_subject_estimate),
            Rcpp::Named("sigma_G") = scale(sigma_between_subject_estimate),
            Rcpp::Named("sigma_i") = sigma_within_per_subject,
            Rcpp::Named("sigma_a") = sigma_analytical_per_subject,
            Rcpp::Named("HBHR") = HBHR,
            Rcpp::Named("beta") = grand_mean);
    }

    // -----------------------------------------------------------------------
    // Output: "sigma_ci" or "cv_ci" (with confidence intervals)
    // -----------------------------------------------------------------------
    if (output_type != "sigma_ci" && output_type != "cv_ci")
        Rcpp::stop("output_type must be 'sigma', 'cv', 'sigma_ci', or 'cv_ci'.");

    double df_between_subjects = Rcpp::as<double>(anova["n1"]);
    double df_between_samples = Rcpp::as<double>(anova["n2"]);
    double df_within_samples_ci = Rcpp::as<double>(anova["n3"]);

    double alpha_half_tail = (1.0 - level) / 2.0;

    // Initialise CI vectors [estimate, lower, upper] -------------------------
    Rcpp::NumericVector ci_analytical = {sigma_analytical_estimate, NA_REAL, NA_REAL};
    Rcpp::NumericVector ci_within_subject = {sigma_within_subject_estimate, NA_REAL, NA_REAL};
    Rcpp::NumericVector ci_between_subject = {sigma_between_subject_estimate, NA_REAL, NA_REAL};

    // -- CI for sigma_A (chi-squared) ----------------------------------------
    if (df_within_samples_ci > 0 && !ISNAN(ms_within_samples) && ms_within_samples >= 0.0)
    {
        double chi_sq_lower_quantile = R::qchisq(alpha_half_tail, df_within_samples_ci, true, false);
        double chi_sq_upper_quantile = R::qchisq(1.0 - alpha_half_tail, df_within_samples_ci, true, false);
        if (chi_sq_lower_quantile > 0 && !ISNAN(chi_sq_lower_quantile))
            ci_analytical[2] = std::sqrt(df_within_samples_ci * ms_within_samples / chi_sq_lower_quantile); // upper
        if (chi_sq_upper_quantile > 0 && !ISNAN(chi_sq_upper_quantile))
            ci_analytical[1] = std::sqrt(df_within_samples_ci * ms_within_samples / chi_sq_upper_quantile); // lower
        if (!ISNAN(ci_analytical[1]) && !ISNAN(ci_analytical[2]) && ci_analytical[1] > ci_analytical[2])
            std::swap(ci_analytical[1], ci_analytical[2]);
        if (ISNAN(ci_analytical[1]))
            ci_analytical[1] = 0.0;
    }

    // -- G / H coefficients for sigma_I and sigma_G CIs ---------------------
    bool all_mean_squares_valid = (df_between_subjects > 0 && df_between_samples > 0 && df_within_samples_ci > 0 &&
                                   !ISNAN(ms_unweighted_between_subjects) && !ISNAN(ms_unweighted_between_samples) && !ISNAN(ms_within_samples) &&
                                   ms_unweighted_between_subjects >= 0 && ms_unweighted_between_samples >= 0 && ms_within_samples >= 0);

    if (all_mean_squares_valid)
    {
        // F quantiles involving infinity (1e12 proxy)
        double Fa_df1_inf = R::qf(alpha_half_tail, df_between_subjects, 1e12, true, false);
        double Fa_df2_inf = R::qf(alpha_half_tail, df_between_samples, 1e12, true, false);
        double Fa_df3_inf = R::qf(alpha_half_tail, df_within_samples_ci, 1e12, true, false);
        double F1a_df1_inf = R::qf(1.0 - alpha_half_tail, df_between_subjects, 1e12, true, false);
        double F1a_df2_inf = R::qf(1.0 - alpha_half_tail, df_between_samples, 1e12, true, false);
        double F1a_df3_inf = R::qf(1.0 - alpha_half_tail, df_within_samples_ci, 1e12, true, false);

        auto safe_G = [](double F_quantile)
        { return (F_quantile > 0 && !ISNAN(F_quantile)) ? 1.0 - 1.0 / F_quantile : 0.0; };
        auto safe_H = [](double F_quantile)
        { return (F_quantile > 0 && !ISNAN(F_quantile)) ? 1.0 / F_quantile - 1.0 : 0.0; };

        double G1 = safe_G(Fa_df1_inf), G2 = safe_G(Fa_df2_inf), G3 = safe_G(Fa_df3_inf);
        double H1 = safe_H(F1a_df1_inf), H2 = safe_H(F1a_df2_inf), H3 = safe_H(F1a_df3_inf);

        // Pairwise F quantiles
        double Fa_df1_df2 = R::qf(alpha_half_tail, df_between_subjects, df_between_samples, true, false);
        double Fa_df1_df3 = R::qf(alpha_half_tail, df_between_subjects, df_within_samples_ci, true, false);
        double Fa_df2_df3 = R::qf(alpha_half_tail, df_between_samples, df_within_samples_ci, true, false);
        double Fa_df3_df2 = R::qf(alpha_half_tail, df_within_samples_ci, df_between_samples, true, false);
        double F1a_df1_df2 = R::qf(1.0 - alpha_half_tail, df_between_subjects, df_between_samples, true, false);
        double F1a_df1_df3 = R::qf(1.0 - alpha_half_tail, df_between_subjects, df_within_samples_ci, true, false);
        double F1a_df2_df3 = R::qf(1.0 - alpha_half_tail, df_between_samples, df_within_samples_ci, true, false);
        double F1a_df3_df2 = R::qf(1.0 - alpha_half_tail, df_within_samples_ci, df_between_samples, true, false);
        double Fa_df13_inf = R::qf(alpha_half_tail, df_between_subjects + df_within_samples_ci, 1e12, true, false);
        double Fa_df23_inf = R::qf(alpha_half_tail, df_between_samples + df_within_samples_ci, 1e12, true, false);

        // Cross-term coefficients
        auto cross_lower = [](double F_quantile, double g_coeff, double h_coeff) -> double
        {
            if (ISNAN(F_quantile) || F_quantile <= 0)
                return NA_REAL;
            double d = F_quantile - 1.0;
            return (d * d - g_coeff * g_coeff * F_quantile * F_quantile - h_coeff * h_coeff) / F_quantile;
        };
        auto cross_upper = [](double F_quantile, double h_coeff, double g_coeff) -> double
        {
            if (ISNAN(F_quantile) || F_quantile <= 0)
                return NA_REAL;
            double d = 1.0 - F_quantile;
            return (d * d - h_coeff * h_coeff * F_quantile * F_quantile - g_coeff * g_coeff) / F_quantile;
        };

        double G12 = cross_lower(Fa_df1_df2, G1, H2);
        double G13 = cross_lower(Fa_df1_df3, G1, H3);
        double G23 = cross_lower(Fa_df2_df3, G2, H3);
        double G32 = cross_lower(Fa_df3_df2, G3, H2);
        double H12 = cross_upper(F1a_df1_df2, H1, G2);
        double H13 = cross_upper(F1a_df1_df3, H1, G3);
        double H23 = cross_upper(F1a_df2_df3, H2, G3);
        double H32 = cross_upper(F1a_df3_df2, H3, G2);

        // Star coefficients
        double G13_star = NA_REAL, H23_star = NA_REAL;
        if (!ISNAN(Fa_df13_inf) && Fa_df13_inf > 0 && df_between_subjects > 0 && df_within_samples_ci > 0)
        {
            double g13a = 1.0 - 1.0 / Fa_df13_inf;
            double g13b = df_between_subjects + df_within_samples_ci;
            G13_star = (g13a * g13a) * (g13b * g13b) / (df_between_subjects * df_within_samples_ci) - G1 * G1 * df_between_subjects / df_within_samples_ci - G3 * G3 * df_within_samples_ci / df_between_subjects;
        }
        if (!ISNAN(Fa_df23_inf) && Fa_df23_inf > 0 && df_between_samples > 0 && df_within_samples_ci > 0)
        {
            double h23a = 1.0 - 1.0 / Fa_df23_inf;
            double h23b = df_between_samples + df_within_samples_ci;
            H23_star = (h23a * h23a) * (h23b * h23b) / (df_between_samples * df_within_samples_ci) - G2 * G2 * df_between_samples / df_within_samples_ci - G3 * G3 * df_within_samples_ci / df_between_samples;
        }

        // -- CI for sigma_I (generalized) --------------------------------------
        if (!ISNAN(w3U) && w3U != 0.0 &&
            !ISNAN(G2) && !ISNAN(H3) && !ISNAN(G23) &&
            !ISNAN(H2) && !ISNAN(G3) && !ISNAN(H23))
        {
            double variance_ci_lower = G2 * G2 * ms_unweighted_between_samples * ms_unweighted_between_samples + H3 * H3 * ms_within_samples * ms_within_samples + G23 * ms_unweighted_between_samples * ms_within_samples;
            double variance_ci_upper = H2 * H2 * ms_unweighted_between_samples * ms_unweighted_between_samples + G3 * G3 * ms_within_samples * ms_within_samples + H23 * ms_unweighted_between_samples * ms_within_samples;
            if (variance_ci_lower >= 0.0 && variance_ci_upper >= 0.0)
            {
                double variance_estimate = (ms_unweighted_between_samples - ms_within_samples) / w3U;
                ci_within_subject[1] = std::sqrt(std::max(0.0, variance_estimate - std::sqrt(variance_ci_lower) / w3U));
                ci_within_subject[2] = std::sqrt(std::max(0.0, variance_estimate + std::sqrt(variance_ci_upper) / w3U));
                if (!ISNAN(ci_within_subject[1]) && !ISNAN(ci_within_subject[2]) && ci_within_subject[1] > ci_within_subject[2])
                    std::swap(ci_within_subject[1], ci_within_subject[2]);
            }
        }

        // -- CI for sigma_G (generalized) --------------------------------------
        if (!ISNAN(w2U) && w2U != 0.0 && !ISNAN(coeff_c2) && !ISNAN(coeff_c3) &&
            !ISNAN(G12) && !ISNAN(H12))
        {
            double variance_ci_lower_base = G1 * G1 * ms_unweighted_between_subjects * ms_unweighted_between_subjects + H2 * H2 * coeff_c2 * coeff_c2 * ms_unweighted_between_samples * ms_unweighted_between_samples + G12 * coeff_c2 * ms_unweighted_between_subjects * ms_unweighted_between_samples;
            double variance_ci_upper_base = H1 * H1 * ms_unweighted_between_subjects * ms_unweighted_between_subjects + G2 * G2 * coeff_c2 * coeff_c2 * ms_unweighted_between_samples * ms_unweighted_between_samples + H12 * coeff_c2 * ms_unweighted_between_subjects * ms_unweighted_between_samples;
            double variance_ci_lower = NA_REAL, variance_ci_upper = NA_REAL;

            if (coeff_c3 >= 0.0)
            {
                if (!ISNAN(G32) && !ISNAN(G13_star) && !ISNAN(H32) && !ISNAN(H13))
                {
                    variance_ci_lower = variance_ci_lower_base + G3 * G3 * coeff_c3 * coeff_c3 * ms_within_samples * ms_within_samples + G32 * coeff_c3 * coeff_c2 * ms_within_samples * ms_unweighted_between_samples + G13_star * coeff_c3 * ms_unweighted_between_subjects * ms_within_samples;
                    variance_ci_upper = variance_ci_upper_base + H3 * H3 * coeff_c3 * coeff_c3 * ms_within_samples * ms_within_samples + H32 * coeff_c3 * coeff_c2 * ms_within_samples * ms_unweighted_between_samples + H13 * coeff_c3 * ms_unweighted_between_subjects * ms_within_samples;
                }
            }
            else
            {
                if (!ISNAN(G13) && !ISNAN(H23_star) && !ISNAN(H13))
                {
                    double abs_coeff_c3 = std::abs(coeff_c3);
                    variance_ci_lower = variance_ci_lower_base + H3 * H3 * coeff_c3 * coeff_c3 * ms_within_samples * ms_within_samples + G13 * abs_coeff_c3 * ms_unweighted_between_subjects * ms_within_samples;
                    variance_ci_upper = variance_ci_upper_base + G3 * G3 * coeff_c3 * coeff_c3 * ms_within_samples * ms_within_samples + H13 * abs_coeff_c3 * ms_unweighted_between_subjects * ms_within_samples + H23_star * abs_coeff_c3 * coeff_c2 * ms_unweighted_between_samples * ms_within_samples;
                }
            }

            if (!ISNAN(variance_ci_lower) && !ISNAN(variance_ci_upper) && variance_ci_lower >= 0.0 && variance_ci_upper >= 0.0)
            {
                double variance_estimate = (ms_unweighted_between_subjects - coeff_c2 * ms_unweighted_between_samples + coeff_c3 * ms_within_samples) / w2U;
                ci_between_subject[1] = std::sqrt(std::max(0.0, variance_estimate - std::sqrt(variance_ci_lower) / w2U));
                ci_between_subject[2] = std::sqrt(std::max(0.0, variance_estimate + std::sqrt(variance_ci_upper) / w2U));
                if (!ISNAN(ci_between_subject[1]) && !ISNAN(ci_between_subject[2]) && ci_between_subject[1] > ci_between_subject[2])
                    std::swap(ci_between_subject[1], ci_between_subject[2]);
            }
        }
    } // end if (all_mean_squares_valid) – G/H coefficient block

    // --- Apply scaling (mult, grand_mean for CV) ----------------------------
    if (output_type == "cv_ci")
    {
        if (ISNAN(grand_mean) || grand_mean == 0.0)
        {
            Rcpp::warning("Grand mean is NA or zero; CV CIs will be NA.");
            for (int k = 0; k < 3; ++k)
            {
                ci_between_subject[k] = NA_REAL;
                ci_within_subject[k] = NA_REAL;
                ci_analytical[k] = NA_REAL;
            }
            for (int i = 0; i < sigma_within_per_subject.size(); ++i)
            {
                sigma_within_per_subject[i] = NA_REAL;
                sigma_analytical_per_subject[i] = NA_REAL;
            }
        }
        else
        {
            for (int k = 0; k < 3; ++k)
            {
                if (!ISNAN(ci_between_subject[k]))
                    ci_between_subject[k] = ci_between_subject[k] / grand_mean * mult;
                if (!ISNAN(ci_within_subject[k]))
                    ci_within_subject[k] = ci_within_subject[k] / grand_mean * mult;
                if (!ISNAN(ci_analytical[k]))
                    ci_analytical[k] = ci_analytical[k] / grand_mean * mult;
            }
            for (int i = 0; i < sigma_within_per_subject.size(); ++i)
            {
                if (!ISNAN(sigma_within_per_subject[i]))
                    sigma_within_per_subject[i] = sigma_within_per_subject[i] / grand_mean * mult;
                if (!ISNAN(sigma_analytical_per_subject[i]))
                    sigma_analytical_per_subject[i] = sigma_analytical_per_subject[i] / grand_mean * mult;
            }
        }
    }
    else
    { // "sigma_ci"
        for (int k = 0; k < 3; ++k)
        {
            if (!ISNAN(ci_between_subject[k]))
                ci_between_subject[k] *= mult;
            if (!ISNAN(ci_within_subject[k]))
                ci_within_subject[k] *= mult;
            if (!ISNAN(ci_analytical[k]))
                ci_analytical[k] *= mult;
        }
        for (int i = 0; i < sigma_within_per_subject.size(); ++i)
        {
            if (!ISNAN(sigma_within_per_subject[i]))
                sigma_within_per_subject[i] *= mult;
            if (!ISNAN(sigma_analytical_per_subject[i]))
                sigma_analytical_per_subject[i] *= mult;
        }
    }

    return Rcpp::List::create(
        Rcpp::Named("sigma_A") = ci_analytical,
        Rcpp::Named("sigma_I") = ci_within_subject,
        Rcpp::Named("sigma_G") = ci_between_subject,
        Rcpp::Named("sigma_i") = sigma_within_per_subject,
        Rcpp::Named("sigma_a") = sigma_analytical_per_subject,
        Rcpp::Named("HBHR") = HBHR,
        Rcpp::Named("beta") = grand_mean);
}

// ==========================================================================
// bv_anova_bootstrap_ci – Percentile bootstrap CIs
// ==========================================================================

//' Bootstrap Confidence Intervals for ANOVA
//' Variance Components
//'
//' @name bv_anova_bootstrap_ci
//'
//' @description
//' Estimates percentile bootstrap confidence intervals
//' for the variance-component standard deviations (or
//' CVs) obtained from \code{\link{variance_components}}.
//' Subjects are resampled with replacement; the sample
//' and replicate structure within each selected subject
//' is kept intact.
//'
//' @param data_orig A \code{list} (or \code{data.table})
//'   with the same structure required by
//'   \code{\link{bv_anova}}:
//'   \describe{
//'     \item{\code{SubjectID}}{\code{integer} vector
//'       of subject identifiers.}
//'     \item{\code{SampleID}}{\code{integer} vector
//'       of sample identifiers.}
//'     \item{\code{ReplicateID}}{\code{integer} vector
//'       of replicate identifiers.}
//'     \item{\code{y}}{\code{numeric} vector of
//'       measured values.}
//'   }
//' @param B \code{integer}.  Number of bootstrap
//'   replicates to draw.  Values of 1000--10 000 are
//'   typical.
//' @param level \code{numeric} confidence level,
//'   between 0 and 1.  Default is \code{0.95}.
//' @param output_type_for_point_est \code{character}.
//'   Either \code{"sigma"} (default) to report SDs
//'   or \code{"cv"} to report coefficients of
//'   variation.
//' @param mult \code{numeric} scaling factor applied
//'   to every estimate and CI bound (e.g. \code{100}
//'   for percentages).  Default is \code{1}.
//'
//' @details
//' At each bootstrap iteration the function:
//' \enumerate{
//'   \item Draws \eqn{I} subjects with replacement
//'     from the unique subject IDs (where \eqn{I} is
//'     the number of unique subjects in
//'     \code{data_orig}).
//'   \item Assembles the full sample-by-replicate
//'     data for the resampled subjects, remapping
//'     subject IDs to \eqn{1, \ldots, I}.
//'   \item Calls \code{\link{variance_components}} on
//'     the bootstrap dataset.
//' }
//'
//' Percentile CIs are taken as the \eqn{\alpha / 2}
//' and \eqn{1 - \alpha / 2} quantiles of the \eqn{B}
//' bootstrap estimates (using \code{type = 7}
//' quantile interpolation).  Iterations that fail or
//' produce \code{NA} are excluded from the quantile
//' calculation.
//'
//' @return A named \code{list} with two elements:
//'   \describe{
//'     \item{\code{point_estimates}}{A \code{list} of
//'       scalar point estimates from the original data:
//'       \code{sigma_A}, \code{sigma_I},
//'       \code{sigma_G}, \code{HBHR}, and
//'       \code{beta}.}
//'     \item{\code{conf_intervals}}{A \code{list} of
//'       length-2 \code{numeric} vectors
//'       \code{[lower, upper]} for each parameter:
//'       \code{sigma_A_CI}, \code{sigma_I_CI},
//'       \code{sigma_G_CI}, \code{HBHR_CI}, and
//'       \code{beta_CI}.}
//'   }
//'
//' @seealso \code{\link{variance_components}} for the
//'   parametric (Burdick--Graybill) CI approach;
//'   \code{\link{bv_anova}} for the underlying ANOVA.
//'
//' @examples
//' \dontrun{
//' dat <- simulate_bv_data(
//'   15, 10, 2, 10, 2, 50, 100
//' )
//' set.seed(42)
//' boot_ci <- bv_anova_bootstrap_ci(
//'   dat, B = 2000, level = 0.95,
//'   output_type_for_point_est = "cv",
//'   mult = 100
//' )
//' boot_ci$point_estimates$sigma_A
//' boot_ci$conf_intervals$sigma_A_CI
//' }
//'
//' @export
// [[Rcpp::export]]
List bv_anova_bootstrap_ci(List data_orig,
                           int B,
                           double level = 0.95,
                           std::string output_type_for_point_est = "sigma",
                           double mult = 1.0)
{

    IntegerVector all_subject_ids = data_orig["SubjectID"];
    IntegerVector all_sample_ids = data_orig["SampleID"];
    IntegerVector all_replicate_ids = data_orig["ReplicateID"];
    NumericVector all_values = data_orig["y"];

    IntegerVector unique_subject_ids = Rcpp::unique(all_subject_ids).sort();
    int num_unique_subjects = unique_subject_ids.size();
    int total_rows = all_subject_ids.size();

    // Pre-build subject-to-row index once: O(N) instead of O(I*N) per bootstrap
    std::vector<std::vector<int>> subject_row_indices(num_unique_subjects);
    {
        std::unordered_map<int, int> subject_id_to_index;
        subject_id_to_index.reserve(num_unique_subjects);
        for (int i = 0; i < num_unique_subjects; ++i)
            subject_id_to_index[unique_subject_ids[i]] = i;
        for (int k = 0; k < total_rows; ++k)
            subject_row_indices[subject_id_to_index[all_subject_ids[k]]].push_back(k);
    }

    // Storage for bootstrap estimates
    NumericMatrix bootstrap_sigma_analytical(B, 1);
    NumericMatrix bootstrap_sigma_within(B, 1);
    NumericMatrix bootstrap_sigma_between(B, 1);
    NumericMatrix bootstrap_hbhr(B, 1);
    NumericMatrix bootstrap_grand_mean(B, 1);

    // --- Bootstrap loop -----------------------------------------------------
    for (int boot_iter = 0; boot_iter < B; ++boot_iter)
    {
        Rcpp::checkUserInterrupt();

        // Resample subject indices with replacement
        IntegerVector resampled_indices(num_unique_subjects);
        for (int i = 0; i < num_unique_subjects; ++i)
            resampled_indices[i] = static_cast<int>(R::runif(0, num_unique_subjects));

        // Compute exact bootstrap dataset size
        int boot_size = 0;
        for (int i = 0; i < num_unique_subjects; ++i)
            boot_size += static_cast<int>(subject_row_indices[resampled_indices[i]].size());

        if (boot_size == 0)
        {
            bootstrap_sigma_analytical(boot_iter, 0) = bootstrap_sigma_within(boot_iter, 0) = bootstrap_sigma_between(boot_iter, 0) = NA_REAL;
            bootstrap_hbhr(boot_iter, 0) = bootstrap_grand_mean(boot_iter, 0) = NA_REAL;
            continue;
        }

        // Build bootstrap dataset using pre-built index (O(N) per iteration)
        IntegerVector boot_subject_ids(boot_size);
        IntegerVector boot_sample_ids(boot_size);
        IntegerVector boot_replicate_ids(boot_size);
        NumericVector boot_values(boot_size);

        int cursor = 0;
        for (int i = 0; i < num_unique_subjects; ++i)
        {
            const std::vector<int> &rows = subject_row_indices[resampled_indices[i]];
            int remapped_subject_id = i + 1;
            for (int k : rows)
            {
                boot_subject_ids[cursor] = remapped_subject_id;
                boot_sample_ids[cursor] = all_sample_ids[k];
                boot_replicate_ids[cursor] = all_replicate_ids[k];
                boot_values[cursor] = all_values[k];
                ++cursor;
            }
        }

        List bootstrap_dataset = List::create(
            Named("SubjectID") = boot_subject_ids,
            Named("SampleID") = boot_sample_ids,
            Named("ReplicateID") = boot_replicate_ids,
            Named("y") = boot_values);

        try
        {
            List bootstrap_estimates = variance_components(bootstrap_dataset, output_type_for_point_est, mult, 0.95);
            bootstrap_sigma_analytical(boot_iter, 0) = as<double>(bootstrap_estimates["sigma_A"]);
            bootstrap_sigma_within(boot_iter, 0) = as<double>(bootstrap_estimates["sigma_I"]);
            bootstrap_sigma_between(boot_iter, 0) = as<double>(bootstrap_estimates["sigma_G"]);
            bootstrap_hbhr(boot_iter, 0) = as<double>(bootstrap_estimates["HBHR"]);
            bootstrap_grand_mean(boot_iter, 0) = as<double>(bootstrap_estimates["beta"]);
        }
        catch (...)
        {
            bootstrap_sigma_analytical(boot_iter, 0) = bootstrap_sigma_within(boot_iter, 0) = bootstrap_sigma_between(boot_iter, 0) = NA_REAL;
            bootstrap_hbhr(boot_iter, 0) = bootstrap_grand_mean(boot_iter, 0) = NA_REAL;
        }
    }

    // --- Percentile CIs from bootstrap distributions ------------------------
    double lower_quantile_prob = (1.0 - level) / 2.0;
    double upper_quantile_prob = 1.0 - lower_quantile_prob;
    NumericVector probs = NumericVector::create(lower_quantile_prob, upper_quantile_prob);
    Function Rquantile("quantile");

    auto compute_percentile_ci = [&](NumericVector estimates) -> NumericVector
    {
        NumericVector valid_estimates;
        for (int i = 0; i < estimates.size(); ++i)
            if (!R_IsNA(estimates[i]) && R_finite(estimates[i]))
                valid_estimates.push_back(estimates[i]);
        if (valid_estimates.size() < 2)
            return NumericVector::create(NA_REAL, NA_REAL);
        return as<NumericVector>(
            Rquantile(valid_estimates, probs, Named("na.rm", true), Named("type", 7)));
    };

    NumericVector ci_sigma_analytical = compute_percentile_ci(bootstrap_sigma_analytical(_, 0));
    NumericVector ci_sigma_within = compute_percentile_ci(bootstrap_sigma_within(_, 0));
    NumericVector ci_sigma_between = compute_percentile_ci(bootstrap_sigma_between(_, 0));
    NumericVector ci_hbhr = compute_percentile_ci(bootstrap_hbhr(_, 0));
    NumericVector ci_grand_mean = compute_percentile_ci(bootstrap_grand_mean(_, 0));

    // Point estimates from original data
    List original_estimates = variance_components(data_orig, output_type_for_point_est, mult, level);

    return List::create(
        Named("point_estimates") = List::create(
            Named("sigma_A") = as<double>(original_estimates["sigma_A"]),
            Named("sigma_I") = as<double>(original_estimates["sigma_I"]),
            Named("sigma_G") = as<double>(original_estimates["sigma_G"]),
            Named("HBHR") = as<double>(original_estimates["HBHR"]),
            Named("beta") = as<double>(original_estimates["beta"])),
        Named("conf_intervals") = List::create(
            Named("sigma_A_CI") = ci_sigma_analytical,
            Named("sigma_I_CI") = ci_sigma_within,
            Named("sigma_G_CI") = ci_sigma_between,
            Named("HBHR_CI") = ci_hbhr,
            Named("beta_CI") = ci_grand_mean));
}

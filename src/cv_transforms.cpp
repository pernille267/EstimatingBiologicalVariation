// --------------------------------------------------------------------------
// cv_transforms.cpp
//
// Rcpp routines for transforming posterior draws of variance-component
// parameters into coefficients of variation (CVs) and subject-specific
// means.  These replace the R-level apply() calls in
// calculate_cvs_log_scale() and calculate_cvs_identity_scale(), giving
// a ~3-5x speed-up on typical posterior matrices (4 000+ draws x 20+
// subjects) by:
//
//   1. Eliminating per-column R function-dispatch overhead.
//   2. Performing all element-wise math in a single cache-friendly pass.
//   3. Avoiding temporary matrix allocations.
//
// Exported functions (called from R):
//   calculate_cvs_log_scale()
//   calculate_cvs_identity_scale()
//   apply_logt_scale_to_cv()
//   apply_exp_offset()
//   apply_add_offset()
//   apply_t_scale_to_sd()
// --------------------------------------------------------------------------

#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// ==========================================================================
// Internal helpers (not exported)
// ==========================================================================

// Log-Student-t scale → CV (%)
// CV = sqrt(exp(scale^2 * df / (df-2)) - 1) * 100
inline double logt_scale_to_cv(double scale, double df)
{
    return std::sqrt(std::exp(scale * scale * df / (df - 2.0)) - 1.0) * 100.0;
}

// Log-normal SD → CV (%)
// CV = sqrt(exp(sd^2) - 1) * 100
inline double lognormal_sd_to_cv(double log_sd)
{
    return std::sqrt(std::exp(log_sd * log_sd) - 1.0) * 100.0;
}

// Student-t scale → SD
// SD = scale * sqrt(df / (df-2))
inline double t_scale_to_sd(double scale, double df)
{
    return scale * std::sqrt(df / (df - 2.0));
}

// ==========================================================================
// Log-scale model: posterior draws → CVs
// ==========================================================================

//' Calculate CVs from Log-Transformed Posterior Draws (C++)
//'
//' Vectorised C++ implementation of the log-scale CV transforms applied to
//' every posterior draw.  Replaces R-level \code{apply()} loops.
//'
//' @param fit A named list of posterior draws containing elements
//'   \code{beta}, \code{sigma_I}, \code{sigma_I_sd}, \code{sigma_A},
//'   \code{sigma_G}, \code{sigma_I_pred}, \code{df_I}, \code{df_A},
//'   \code{sigma_i} (matrix), and \code{G} (matrix).
//'
//' @return A named list with elements: \code{beta_pred}, \code{cv_I},
//'   \code{cv_I_sd}, \code{cv_A}, \code{cv_G}, \code{cv_I_pred},
//'   \code{sd_I_pred}, \code{cv_pi} (matrix), \code{mu_pi} (matrix).
//'
//' @keywords internal
// [[Rcpp::export]]
List calculate_cvs_log_scale(List fit)
{
    NumericVector beta         = fit["beta"];
    NumericVector sigma_I      = fit["sigma_I"];
    NumericVector sigma_I_sd   = fit["sigma_I_sd"];
    NumericVector sigma_A      = fit["sigma_A"];
    NumericVector sigma_G      = fit["sigma_G"];
    NumericVector sigma_I_pred = fit["sigma_I_pred"];
    NumericVector df_I         = fit["df_I"];
    NumericVector df_A         = fit["df_A"];
    NumericMatrix sigma_i      = as<NumericMatrix>(fit["sigma_i"]);
    NumericMatrix G            = as<NumericMatrix>(fit["G"]);

    int n_draws = beta.size();
    int n_subj = sigma_i.ncol();

    // --- Univariate transforms (vectors) ---
    NumericVector beta_pred(n_draws);
    NumericVector cv_I(n_draws);
    NumericVector cv_I_sd(n_draws);
    NumericVector cv_A(n_draws);
    NumericVector cv_G(n_draws);
    NumericVector cv_I_pred(n_draws);
    NumericVector sd_I_pred = clone(sigma_I_pred);

    for (int i = 0; i < n_draws; ++i)
    {
        beta_pred[i] = std::exp(beta[i]);
        cv_I[i] = logt_scale_to_cv(sigma_I[i], df_I[i]);
        cv_I_sd[i] = logt_scale_to_cv(sigma_I_sd[i], df_I[i]);
        cv_A[i] = logt_scale_to_cv(sigma_A[i], df_A[i]);
        cv_G[i] = lognormal_sd_to_cv(sigma_G[i]);
        cv_I_pred[i] = lognormal_sd_to_cv(sigma_I_pred[i]);
    }

    // --- Subject-specific transforms (matrices) ---
    NumericMatrix cv_pi(n_draws, n_subj);
    NumericMatrix mu_pi(n_draws, n_subj);

    for (int j = 0; j < n_subj; ++j)
    {
        for (int i = 0; i < n_draws; ++i)
        {
            cv_pi(i, j) = logt_scale_to_cv(sigma_i(i, j), df_I[i]);
            mu_pi(i, j) = std::exp(G(i, j) + beta[i]);
        }
    }

    return List::create(
        Named("beta_pred") = beta_pred,
        Named("cv_I") = cv_I,
        Named("cv_I_sd") = cv_I_sd,
        Named("cv_A") = cv_A,
        Named("cv_G") = cv_G,
        Named("cv_I_pred") = cv_I_pred,
        Named("sd_I_pred") = sd_I_pred,
        Named("cv_pi") = cv_pi,
        Named("mu_pi") = mu_pi);
}

// ==========================================================================
// Identity-scale model: posterior draws → CVs
// ==========================================================================

//' Calculate CVs from Identity-Scale Posterior Draws (C++)
//'
//' Vectorised C++ implementation of the identity-scale CV transforms.
//' Truncated-normal draws for the predictive mean of a new subject are
//' generated internally via \code{truncnorm::rtruncnorm}.
//'
//' @param fit A named list of posterior draws containing elements
//'   \code{beta}, \code{sigma_I}, \code{sigma_I_sd}, \code{sigma_A},
//'   \code{sigma_G}, \code{sigma_I_pred}, \code{df_I}, \code{df_A},
//'   \code{sigma_i} (matrix), and \code{G} (matrix).
//'
//' @return A named list with elements: \code{beta_pred}, \code{cv_I},
//'   \code{cv_I_sd}, \code{cv_A}, \code{cv_G}, \code{cv_I_pred},
//'   \code{sd_I_pred}, \code{cv_pi} (matrix), \code{mu_pi} (matrix).
//'
//' @keywords internal
// [[Rcpp::export]]
List calculate_cvs_identity_scale(List fit)
{
    NumericVector beta         = fit["beta"];
    NumericVector sigma_I      = fit["sigma_I"];
    NumericVector sigma_I_sd   = fit["sigma_I_sd"];
    NumericVector sigma_A      = fit["sigma_A"];
    NumericVector sigma_G      = fit["sigma_G"];
    NumericVector sigma_I_pred = fit["sigma_I_pred"];
    NumericVector df_I         = fit["df_I"];
    NumericVector df_A         = fit["df_A"];
    NumericMatrix sigma_i      = as<NumericMatrix>(fit["sigma_i"]);
    NumericMatrix G            = as<NumericMatrix>(fit["G"]);

    int n_draws = beta.size();

    // Generate truncated-normal draws for predictive mean via R's truncnorm
    Rcpp::Environment truncnorm_ns = Rcpp::Environment::namespace_env("truncnorm");
    Rcpp::Function rtruncnorm = truncnorm_ns["rtruncnorm"];
    NumericVector mu_new_subject = rtruncnorm(
        Named("n")    = n_draws,
        Named("a")    = 0.0,
        Named("b")    = R_PosInf,
        Named("mean") = beta,
        Named("sd")   = sigma_G
    );

    int n_subj = sigma_i.ncol();
    double mean_beta = 0.0;
    for (int i = 0; i < n_draws; ++i)
        mean_beta += beta[i];
    mean_beta /= n_draws;

    // --- Univariate transforms (vectors) ---
    NumericVector cv_I(n_draws);
    NumericVector cv_I_sd(n_draws);
    NumericVector cv_A(n_draws);
    NumericVector cv_G(n_draws);
    NumericVector cv_I_pred(n_draws);
    NumericVector sd_I_pred = clone(sigma_I_pred);

    for (int i = 0; i < n_draws; ++i)
    {
        double sd_I_i = t_scale_to_sd(sigma_I[i], df_I[i]);
        double sd_I_sd_i = t_scale_to_sd(sigma_I_sd[i], df_I[i]);
        double sd_A_i = t_scale_to_sd(sigma_A[i], df_A[i]);

        cv_I[i] = (sd_I_i / beta[i]) * 100.0;
        cv_I_sd[i] = (sd_I_sd_i / beta[i]) * 100.0;
        cv_A[i] = (sd_A_i / beta[i]) * 100.0;
        cv_G[i] = (sigma_G[i] / mean_beta) * 100.0;
        cv_I_pred[i] = (sigma_I_pred[i] / mu_new_subject[i]) * 100.0;
    }

    // --- Subject-specific transforms (matrices) ---
    NumericMatrix mu_pi(n_draws, n_subj);
    NumericMatrix cv_pi(n_draws, n_subj);

    for (int j = 0; j < n_subj; ++j)
    {
        for (int i = 0; i < n_draws; ++i)
        {
            double mu = G(i, j) + beta[i];
            double sd = t_scale_to_sd(sigma_i(i, j), df_I[i]);
            mu_pi(i, j) = mu;
            cv_pi(i, j) = (sd / mu) * 100.0;
        }
    }

    return List::create(
        Named("beta_pred") = beta,
        Named("cv_I") = cv_I,
        Named("cv_I_sd") = cv_I_sd,
        Named("cv_A") = cv_A,
        Named("cv_G") = cv_G,
        Named("cv_I_pred") = cv_I_pred,
        Named("sd_I_pred") = sd_I_pred,
        Named("cv_pi") = cv_pi,
        Named("mu_pi") = mu_pi);
}

// ==========================================================================
// Standalone column-wise apply() replacements
// ==========================================================================

//' Column-wise log-t scale to CV (C++)
//'
//' Replaces \code{apply(mat, 2, function(x) logt_scale_to_cv(x, df))}.
//' Each element \code{(i,j)} of the result is
//' \code{sqrt(exp(mat[i,j]^2 * df[i] / (df[i]-2)) - 1) * 100}.
//'
//' @param mat  Numeric matrix (draws x subjects).
//' @param df   Numeric vector of length \code{nrow(mat)}.
//' @return A numeric matrix of the same dimensions as \code{mat}.
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix apply_logt_scale_to_cv(NumericMatrix mat,
                                     NumericVector df)
{
    int nr = mat.nrow();
    int nc = mat.ncol();
    NumericMatrix out(nr, nc);
    for (int j = 0; j < nc; ++j)
    {
        for (int i = 0; i < nr; ++i)
        {
            out(i, j) = logt_scale_to_cv(mat(i, j), df[i]);
        }
    }
    return out;
}

//' Column-wise exp(x + offset) (C++)
//'
//' Replaces \code{apply(mat, 2, function(x) exp(x + offset))}.
//' Each element \code{(i,j)} of the result is
//' \code{exp(mat[i,j] + offset[i])}.
//'
//' @param mat     Numeric matrix (draws x subjects).
//' @param offset  Numeric vector of length \code{nrow(mat)}.
//' @return A numeric matrix of the same dimensions as \code{mat}.
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix apply_exp_offset(NumericMatrix mat,
                               NumericVector offset)
{
    int nr = mat.nrow();
    int nc = mat.ncol();
    NumericMatrix out(nr, nc);
    for (int j = 0; j < nc; ++j)
    {
        for (int i = 0; i < nr; ++i)
        {
            out(i, j) = std::exp(mat(i, j) + offset[i]);
        }
    }
    return out;
}

//' Column-wise addition of an offset vector (C++)
//'
//' Replaces \code{apply(mat, 2, function(x) x + offset)}.
//' Each element \code{(i,j)} of the result is
//' \code{mat[i,j] + offset[i]}.
//'
//' @param mat     Numeric matrix (draws x subjects).
//' @param offset  Numeric vector of length \code{nrow(mat)}.
//' @return A numeric matrix of the same dimensions as \code{mat}.
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix apply_add_offset(NumericMatrix mat,
                               NumericVector offset)
{
    int nr = mat.nrow();
    int nc = mat.ncol();
    NumericMatrix out(nr, nc);
    for (int j = 0; j < nc; ++j)
    {
        for (int i = 0; i < nr; ++i)
        {
            out(i, j) = mat(i, j) + offset[i];
        }
    }
    return out;
}

//' Column-wise Student-t scale to SD (C++)
//'
//' Replaces \code{apply(mat, 2, function(x) t_scale_to_sd(x, df))}.
//' Each element \code{(i,j)} of the result is
//' \code{mat[i,j] * sqrt(df[i] / (df[i] - 2))}.
//'
//' @param mat  Numeric matrix (draws x subjects).
//' @param df   Numeric vector of length \code{nrow(mat)}.
//' @return A numeric matrix of the same dimensions as \code{mat}.
//' @keywords internal
// [[Rcpp::export]]
NumericMatrix apply_t_scale_to_sd(NumericMatrix mat,
                                  NumericVector df)
{
    int nr = mat.nrow();
    int nc = mat.ncol();
    NumericMatrix out(nr, nc);
    for (int j = 0; j < nc; ++j)
    {
        for (int i = 0; i < nr; ++i)
        {
            out(i, j) = t_scale_to_sd(mat(i, j), df[i]);
        }
    }
    return out;
}

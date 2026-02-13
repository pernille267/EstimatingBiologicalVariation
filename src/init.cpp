#include <Rcpp.h>
#include <R_ext/Rdynload.h>

/* ---------- forward declarations of wrappers from RcppExports.cpp -------- */
extern "C"
{
    SEXP _EstimatingBiologicalVariation_calculate_cvs_log_scale(SEXP);
    SEXP _EstimatingBiologicalVariation_calculate_cvs_identity_scale(SEXP);
    SEXP _EstimatingBiologicalVariation_apply_logt_scale_to_cv(SEXP, SEXP);
    SEXP _EstimatingBiologicalVariation_apply_exp_offset(SEXP, SEXP);
    SEXP _EstimatingBiologicalVariation_apply_add_offset(SEXP, SEXP);
    SEXP _EstimatingBiologicalVariation_apply_t_scale_to_sd(SEXP, SEXP);
    SEXP _EstimatingBiologicalVariation_process_stan_data_indexing(SEXP, SEXP, SEXP);
    SEXP _EstimatingBiologicalVariation_process_stan_data_priors(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
    SEXP _EstimatingBiologicalVariation_bv_anova(SEXP, SEXP);
    SEXP _EstimatingBiologicalVariation_variance_components(SEXP, SEXP, SEXP, SEXP, SEXP);
    SEXP _EstimatingBiologicalVariation_bv_anova_bootstrap_ci(SEXP, SEXP, SEXP, SEXP, SEXP);
}

/* ---------- .Call registration table ------------------------------------- */
static const R_CallMethodDef CallEntries[] = {
    {"_EstimatingBiologicalVariation_calculate_cvs_log_scale", (DL_FUNC)&_EstimatingBiologicalVariation_calculate_cvs_log_scale, 1},
    {"_EstimatingBiologicalVariation_calculate_cvs_identity_scale", (DL_FUNC)&_EstimatingBiologicalVariation_calculate_cvs_identity_scale, 1},
    {"_EstimatingBiologicalVariation_apply_logt_scale_to_cv", (DL_FUNC)&_EstimatingBiologicalVariation_apply_logt_scale_to_cv, 2},
    {"_EstimatingBiologicalVariation_apply_exp_offset", (DL_FUNC)&_EstimatingBiologicalVariation_apply_exp_offset, 2},
    {"_EstimatingBiologicalVariation_apply_add_offset", (DL_FUNC)&_EstimatingBiologicalVariation_apply_add_offset, 2},
    {"_EstimatingBiologicalVariation_apply_t_scale_to_sd", (DL_FUNC)&_EstimatingBiologicalVariation_apply_t_scale_to_sd, 2},
    {"_EstimatingBiologicalVariation_process_stan_data_indexing", (DL_FUNC)&_EstimatingBiologicalVariation_process_stan_data_indexing, 3},
    {"_EstimatingBiologicalVariation_process_stan_data_priors", (DL_FUNC)&_EstimatingBiologicalVariation_process_stan_data_priors, 9},
    {"_EstimatingBiologicalVariation_bv_anova", (DL_FUNC)&_EstimatingBiologicalVariation_bv_anova, 2},
    {"_EstimatingBiologicalVariation_variance_components", (DL_FUNC)&_EstimatingBiologicalVariation_variance_components, 5},
    {"_EstimatingBiologicalVariation_bv_anova_bootstrap_ci", (DL_FUNC)&_EstimatingBiologicalVariation_bv_anova_bootstrap_ci, 5},
    {NULL, NULL, 0}};

/* ---------- package init ------------------------------------------------- */
extern "C" void R_init_EstimatingBiologicalVariation(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

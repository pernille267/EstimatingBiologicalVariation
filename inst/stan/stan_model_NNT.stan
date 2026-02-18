// =============================================================================
// Stan Model: NNT (Normal-Normal-StudentT)
// =============================================================================
// Between-subject (G):  Normal
// Within-subject  (I):  Normal
// Analytical      (A):  Location-scale Student-t
//
// Only the analytical (residual) variation uses a location-scale
// t-distribution, allowing heavier tails for measurement error.
// The within-subject biological variation is Normal.
// =============================================================================

data {
  int<lower=1> N_obs;
  int<lower=1> N_subj;
  int<lower=1> N_samp_total;

  vector[N_obs] y;
  array[N_obs] int<lower=1, upper=N_subj> subj_idx;
  array[N_obs] int<lower=1, upper=N_samp_total> samp_idx;

  // Maps each unique sample (1..N_samp_total) to its subject (1..N_subj)
  array[N_samp_total] int<lower=1, upper=N_subj> sample_to_subj_map;

  // Priors for beta
  real prior_beta_mean;
  real<lower=0> prior_beta_sd;

  // Priors for sigma_G
  real<lower=0> prior_sigma_G_mean;
  real<lower=0> prior_sigma_G_sd;

  // Priors for E[sigma_i]
  real<lower=0> prior_sigma_i_mean_mean;
  real<lower=0> prior_sigma_i_mean_sd;

  // Priors for SD[sigma_i]
  real<lower=0> prior_sigma_i_sd_mean;
  real<lower=0> prior_sigma_i_sd_sd;

  // Priors for df_I (accepted for data-block compatibility, unused in NNT)
  real<lower=0> prior_df_I_mean;
  real<lower=0> prior_df_I_sd;

  // Priors for df_A (analytical t degrees of freedom)
  real<lower=0> prior_df_A_mean;
  real<lower=0> prior_df_A_sd;

  // Priors for sigma_A
  real<lower=0> prior_sigma_A_mean;
  real<lower=0> prior_sigma_A_sd;
}

parameters {
  real beta;
  real<lower=0> sigma_G;
  real<lower=0> sigma_A;
  vector[N_subj] G_raw;
  vector[N_samp_total] I_raw;
  real<lower=0.1> df_A_param_minus2;
  real<lower=0> sigma_I_mean;
  real<lower=0> sigma_I_sd;
  vector<lower=0>[N_subj] sigma_i;
}

transformed parameters {
  vector[N_subj] G = sigma_G * G_raw;

  real<lower=2.1> df_A_param = df_A_param_minus2 + 2.0;

  vector[N_samp_total] I;
  for (s_unique_idx in 1:N_samp_total) {
    int current_subj = sample_to_subj_map[s_unique_idx];
    I[s_unique_idx] = I_raw[s_unique_idx] * sigma_i[current_subj];
  }

  // Analytical scale parameter for location-scale t
  real lst_A_sd_scale;
  if (df_A_param_minus2 > 1e-9) {
    lst_A_sd_scale = sigma_A * sqrt(df_A_param_minus2 / df_A_param);
  } else {
    lst_A_sd_scale = sigma_A;
  }
  real lst_A_sd_scale_effective = (lst_A_sd_scale > 1e-9) ? lst_A_sd_scale : 1e-9;

  vector[N_obs] y_mean = beta + G[subj_idx] + I[samp_idx];
}

model {
  // Prior for beta
  beta ~ normal(prior_beta_mean, prior_beta_sd);

  // Priors for sigma_G and G
  sigma_G ~ normal(prior_sigma_G_mean, prior_sigma_G_sd);
  G_raw ~ std_normal();

  // Priors for sigma_i hierarchy and I (within-subject: Normal)
  sigma_I_mean ~ normal(prior_sigma_i_mean_mean, prior_sigma_i_mean_sd);
  sigma_I_sd ~ normal(prior_sigma_i_sd_mean, prior_sigma_i_sd_sd);
  sigma_i ~ normal(sigma_I_mean, sigma_I_sd);
  I_raw ~ std_normal();

  // Priors for sigma_A and df_A
  df_A_param_minus2 ~ normal(prior_df_A_mean, prior_df_A_sd);
  sigma_A ~ normal(prior_sigma_A_mean, prior_sigma_A_sd);

  // Likelihood (Student-t for analytical variation)
  y ~ student_t(df_A_param, y_mean, lst_A_sd_scale_effective);
}

generated quantities {
  real sigma_I_pred;
  {
    real sigma_I_pred_temp = -1.0;
    while (sigma_I_pred_temp < 0) {
      sigma_I_pred_temp = normal_rng(sigma_I_mean, sigma_I_sd);
    }
    sigma_I_pred = sigma_I_pred_temp;
  }
  // No t-distribution for I: df set to large constant for R compatibility
  real df_I = 9999.0;
  real df_A = df_A_param;
  real sigma_I = sigma_I_mean;
}

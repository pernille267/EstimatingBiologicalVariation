data {
  int<lower=1> N_obs;
  int<lower=1> N_subj;
  int<lower=1> N_samp_total;

  vector[N_obs] y;
  array[N_obs] int<lower=1, upper=N_subj> subj_idx;       
  array[N_obs] int<lower=1, upper=N_samp_total> samp_idx;

  // New data item: maps each unique sample (1..N_samp_total) to its subject (1..N_subj)
  array[N_samp_total] int<lower=1, upper=N_subj> sample_to_subj_map;

  // Priors for beta
  real prior_beta_mean;
  real<lower=0> prior_beta_sd;

  // Priors for sigma_G^2 (variance of G_i)
  real<lower=0> prior_sigma_G_mean;
  real<lower=0> prior_sigma_G_sd;

  // Priors for E[sigma_i]
  real<lower=0> prior_sigma_i_mean_mean;              
  real<lower=0> prior_sigma_i_mean_sd;
  
  // Priors for SD[sigma_i]
  real<lower=0> prior_sigma_i_sd_mean;              
  real<lower=0> prior_sigma_i_sd_sd;

  // Priors for dfi_minus_2
  real<lower=0> prior_df_I_mean;
  real<lower=0> prior_df_I_sd;

  // Priors for dfa_minus_2
  real<lower=0> prior_df_A_mean;
  real<lower=0> prior_df_A_sd;

  // Priors for sigma_A
  real<lower=0> prior_sigma_A_mean;
  real<lower=0> prior_sigma_A_sd;
}

transformed data {
  // Priors for dfi_minus_2
  real prior_df_I_alpha = square(prior_df_I_mean) / square(prior_df_I_sd);
  real prior_df_I_lambda = prior_df_I_mean / square(prior_df_I_sd);
  
  // Priors for dfa_minus_2
  real prior_df_A_alpha = square(prior_df_A_mean) / square(prior_df_A_sd);
  real prior_df_A_lambda = prior_df_A_mean / square(prior_df_A_sd);
}

parameters {
  real beta;
  real<lower=0> sigma_G;
  real<lower=0> sigma_A;
  vector[N_subj] G_raw;
  vector[N_samp_total] I_raw;
  real<lower=0.1> df_I_param_minus2;
  real<lower=0.1> df_A_param_minus2;
  real<lower=0> sigma_I_mean;     
  real<lower=0> sigma_I_sd;       
  vector<lower=0>[N_subj] sigma_i; 
}

transformed parameters {
  vector[N_subj] G = sigma_G * G_raw;

  real<lower=2.1> df_I_param = df_I_param_minus2 + 2.0;
  real<lower=2.1> df_A_param = df_A_param_minus2 + 2.0;
  
  vector[N_samp_total] I; 
  for (s_unique_idx in 1:N_samp_total) {
    int current_subj = sample_to_subj_map[s_unique_idx];
    real current_subj_sigma_i = sigma_i[current_subj];
    real lst_I_sd_scale;
    if (df_I_param_minus2 > 1e-9) {
      lst_I_sd_scale = current_subj_sigma_i * sqrt(df_I_param_minus2 / df_I_param);
    } else {
      lst_I_sd_scale = current_subj_sigma_i;
    }
    real lst_I_sd_scale_effective = (lst_I_sd_scale > 1e-9) ? lst_I_sd_scale : 1e-9;
    I[s_unique_idx] = I_raw[s_unique_idx] * lst_I_sd_scale_effective;
  }

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

  // Priors for sigma_i, sigma_I, and I
  sigma_I_mean ~ normal(prior_sigma_i_mean_mean, prior_sigma_i_mean_sd);
  sigma_I_sd ~ normal(prior_sigma_i_sd_mean, prior_sigma_i_sd_sd);
  sigma_i ~ normal(sigma_I_mean, sigma_I_sd);
  df_I_param_minus2 ~ gamma(prior_df_I_alpha, prior_df_I_lambda);
  I_raw ~ student_t(df_I_param, 0, 1);

  // Priors for sigma_A
  df_A_param_minus2 ~ gamma(prior_df_A_alpha, prior_df_A_lambda);
  sigma_A ~ normal(prior_sigma_A_mean, prior_sigma_A_sd);

  // Likelihood
  y ~ student_t(df_A_param, y_mean, lst_A_sd_scale_effective);
}

generated quantities {
  real sigma_I_pred_raw;
  real sigma_I_pred;
  {
    real sigma_I_pred_temp = -1.0;
    while (sigma_I_pred_temp < 0) {
      sigma_I_pred_temp = normal_rng(sigma_I_mean, sigma_I_sd);
    }
    sigma_I_pred_raw = sigma_I_pred_temp;
  }
  sigma_I_pred = sigma_I_pred_raw * sqrt(df_I_param_minus2 / df_I_param);
  real df_I = df_I_param;
  real df_A = df_A_param;
  real sigma_I = sigma_I_mean;
}
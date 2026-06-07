data{
  int N;   // number of games
  int<lower=0> N_prev;
  array[N,2] int y;
  int nteams;
  array[N] int team1;
  array[N] int team2;
  array[N_prev] int team1_prev;
  array[N_prev] int team2_prev;
  int ntimes;                 // dynamic periods
  array[ntimes] int time;
  array[N] int instants;
  array[N_prev] int instants_prev;
  array[N] int instants_rank;
  int ntimes_rank;                 // dynamic periods for ranking
  matrix[ntimes_rank,nteams] ranking;      // eventual ranking
  int<lower=0, upper=1> ind_home;
  int<lower=0, upper=1> ind_common_sigma;
  real mean_home;              // Mean for home effect
  real<lower=0> sd_home;      // Standard deviation for home effect

  // priors part
  int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
  int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

  real<lower=0> hyper_df;
  real hyper_location;

  real<lower=0> hyper_sd_df;
  real hyper_sd_location;
  real<lower=0> hyper_sd_scale;


  // commensurate prior
  int<lower=0, upper=1> ind_comm_prior;
  real mu_spike;
  real<lower=0> sd_spike;
  real mu_slab;
  real<lower=0> sd_slab;

  // Koopman & Lit (2015) break variance inflation
  int<lower=0, upper=1> ind_kl_sd;  // 1 to use K&L approach
  array[ntimes] int<lower=0, upper=1> is_summer_break;  // 1 if period follows summer break
}

transformed data {
  real lognc_spike = normal_lccdf(0 | mu_spike, sd_spike);
  real lognc_slab  = normal_lccdf(0 | mu_slab, sd_slab);
}

parameters{
  // Non-centered parameterization for commensurate prior
  matrix[ind_comm_prior ? ntimes : 0, ind_comm_prior ? nteams : 0] att_raw_std;  // standard normal
  matrix[ind_comm_prior ? ntimes : 0, ind_comm_prior ? nteams : 0] def_raw_std;  // standard normal

  // Centered parameterization for non-commensurate models
  matrix[ind_comm_prior ? 0 : ntimes, ind_comm_prior ? 0 : nteams] att_raw_centered;
  matrix[ind_comm_prior ? 0 : ntimes, ind_comm_prior ? 0 : nteams] def_raw_centered;

  vector[ntimes] home;
  real gamma;
  array[ind_comm_prior ? nteams : 0] real<lower=0, upper=1> prob_spike;

  // Evolution variance - Egidi (2018) approach
  array[(ind_comm_prior || ind_common_sigma || ind_kl_sd) ? 0 : 1] real<lower=0> sigma_att;
  array[(ind_comm_prior || ind_common_sigma || ind_kl_sd) ? 0 : 1] real<lower=0> sigma_def;

  // Evolution variance - Owen (2011) approach
  array[(ind_comm_prior || !ind_common_sigma || ind_kl_sd) ? 0 : 1] real<lower=0> sigma_common;

  // Koopman & Lit (2015) approach: base variance + break variance inflation
  array[ind_kl_sd ? 1 : 0] real<lower=0> sigma_att_kl;      // base attack variance
  array[ind_kl_sd ? 1 : 0] real<lower=0> sigma_def_kl;      // base defense variance
  array[ind_kl_sd ? 1 : 0] real<lower=0> sigma_break;       // additional variance at summer breaks

  // Commensurate prior parameters
  array[ind_comm_prior ? ntimes : 0, ind_comm_prior ? nteams : 0] real<lower=0> comm_prec_att;
  array[ind_comm_prior ? ntimes : 0, ind_comm_prior ? nteams : 0] real<lower=0> comm_prec_def;
}

transformed parameters {
  vector[ntimes] adj_h_eff;
  matrix[ntimes, nteams] att;            // attack abilities
  matrix[ntimes, nteams] def;            // defense abilities
  matrix[ntimes, nteams] att_raw;        // attack raw (before sum-to-zero)
  matrix[ntimes, nteams] def_raw;        // defense raw (before sum-to-zero)
  matrix[ntimes, nteams] mu_att;         // attack hyperparameter
  matrix[ntimes, nteams] mu_def;         // defense hyperparameter
  vector[N] theta_home;                  // exponentiated linear pred.
  vector[N] theta_away;

  // K&L time-varying standard deviations
  array[ind_kl_sd ? ntimes : 0] real<lower=0> sigma_att_t;
  array[ind_kl_sd ? ntimes : 0] real<lower=0> sigma_def_t;

  // Commensurate SD (derived from precision)
  array[ind_comm_prior ? ntimes : 0, ind_comm_prior ? nteams : 0] real<lower=0> comm_sd_att;
  array[ind_comm_prior ? ntimes : 0, ind_comm_prior ? nteams : 0] real<lower=0> comm_sd_def;

  // Compute commensurate SDs from precision
  if (ind_comm_prior == 1) {
    for (i in 1:ntimes) {
      for (h in 1:nteams) {
        comm_sd_att[i,h] = inv(comm_prec_att[i,h]);
        comm_sd_def[i,h] = inv(comm_prec_def[i,h]);
      }
    }
  }

  // Compute K&L time-varying standard deviations
  if (ind_kl_sd == 1) {
    for (t in 1:ntimes) {
      sigma_att_t[t] = sqrt(square(sigma_att_kl[1]) + square(sigma_break[1]) * is_summer_break[t]);
      sigma_def_t[t] = sqrt(square(sigma_def_kl[1]) + square(sigma_break[1]) * is_summer_break[t]);
    }
  }

  // Initialize mu_att and mu_def for first time period
  mu_att[1] = rep_row_vector(hyper_location, nteams);
  mu_def[1] = rep_row_vector(hyper_location, nteams);

  // ========================================
  // Non-centered parameterization for commensurate prior
  // ========================================
  if (ind_comm_prior == 1) {
    // First time period: att_raw = mu + sd * z
    for (h in 1:nteams) {
      att_raw[1, h] = mu_att[1, h] + comm_sd_att[1, h] * att_raw_std[1, h];
      def_raw[1, h] = mu_def[1, h] + comm_sd_def[1, h] * def_raw_std[1, h];
    }

    // Apply sum-to-zero constraint
    att[1] = att_raw[1] - mean(att_raw[1]);
    def[1] = def_raw[1] - mean(def_raw[1]);

    // Subsequent time periods
    for (t in 2:ntimes) {
      // Update mu based on previous constrained values
      mu_att[t] = att[t-1];
      mu_def[t] = def[t-1];

      // Non-centered: att_raw = mu + sd * z
      for (h in 1:nteams) {
        att_raw[t, h] = mu_att[t, h] + comm_sd_att[t, h] * att_raw_std[t, h];
        def_raw[t, h] = mu_def[t, h] + comm_sd_def[t, h] * def_raw_std[t, h];
      }

      // Apply sum-to-zero constraint
      att[t] = att_raw[t] - mean(att_raw[t]);
      def[t] = def_raw[t] - mean(def_raw[t]);
    }
  }
  // ========================================
  // Centered parameterization for other models
  // ========================================
  else {
    // Use centered parameterization
    att_raw = att_raw_centered;
    def_raw = def_raw_centered;

    // Sum-to-zero constraint
    att[1] = att_raw[1] - mean(att_raw[1]);
    def[1] = def_raw[1] - mean(def_raw[1]);

    for (t in 2:ntimes) {
      mu_att[t] = att[t-1];
      mu_def[t] = def[t-1];
      att[t] = att_raw[t] - mean(att_raw[t]);
      def[t] = def_raw[t] - mean(def_raw[t]);
    }
  }

  adj_h_eff = home * ind_home;

  for (n in 1:N) {
    theta_home[n] = exp(adj_h_eff[instants[n]] + att[instants[n], team1[n]] + def[instants[n], team2[n]] +
                     (gamma/2)*(ranking[instants_rank[n], team1[n]] - ranking[instants_rank[n], team2[n]]));
    theta_away[n] = exp(att[instants[n], team2[n]] + def[instants[n], team1[n]] -
                     (gamma/2)*(ranking[instants_rank[n], team1[n]] - ranking[instants_rank[n], team2[n]]));
  }
}

model{
  // ========================================
  // Commensurate Prior Approach (Weighted Dynamic) - Non-centered
  // ========================================
  if (ind_comm_prior == 1) {
    // Prior on spike probability
    target += beta_lpdf(prob_spike | 1, 1);

    // Spike-and-slab prior on precision
    for (h in 1:nteams) {
      for (i in 1:ntimes) {
        target += log_mix(
          prob_spike[h],
          normal_lpdf(comm_prec_att[i,h] | mu_spike, sd_spike) - lognc_spike,
          normal_lpdf(comm_prec_att[i,h] | mu_slab, sd_slab) - lognc_slab
        );
        target += log_mix(
          prob_spike[h],
          normal_lpdf(comm_prec_def[i,h] | mu_spike, sd_spike) - lognc_spike,
          normal_lpdf(comm_prec_def[i,h] | mu_slab, sd_slab) - lognc_slab
        );
      }
    }

    // Standard normal prior on standardized parameters
    // Non-centered parameterization
    if (prior_dist_num == 1) {
      // Normal case: z ~ N(0,1)
      target += std_normal_lpdf(to_vector(att_raw_std));
      target += std_normal_lpdf(to_vector(def_raw_std));
    }
    else if (prior_dist_num == 2) {
      // Student-t case: need to handle differently
      // For non-centered t: x = mu + sigma * z where z ~ t(df, 0, 1)
      for (h in 1:nteams) {
        for (i in 1:ntimes) {
          target += student_t_lpdf(att_raw_std[i,h] | hyper_df, 0, 1);
          target += student_t_lpdf(def_raw_std[i,h] | hyper_df, 0, 1);
        }
      }
    }
    else if (prior_dist_num == 3) {
      // Cauchy case (t with df=1)
      for (h in 1:nteams) {
        for (i in 1:ntimes) {
          target += student_t_lpdf(att_raw_std[i,h] | 1, 0, 1);
          target += student_t_lpdf(def_raw_std[i,h] | 1, 0, 1);
        }
      }
    }
  }
  // ========================================
  // Koopman & Lit (2015) Approach
  // ========================================
  else if (ind_kl_sd == 1) {
    for (h in 1:nteams) {
      for (i in 1:ntimes) {
        if (prior_dist_num == 1) {
          target += normal_lpdf(att_raw[i,h] | mu_att[i,h], sigma_att_t[i]);
          target += normal_lpdf(def_raw[i,h] | mu_def[i,h], sigma_def_t[i]);
        }
        else if (prior_dist_num == 2) {
          target += student_t_lpdf(att_raw[i,h] | hyper_df, mu_att[i,h], sigma_att_t[i]);
          target += student_t_lpdf(def_raw[i,h] | hyper_df, mu_def[i,h], sigma_def_t[i]);
        }
        else if (prior_dist_num == 3) {
          target += student_t_lpdf(att_raw[i,h] | 1, mu_att[i,h], sigma_att_t[i]);
          target += student_t_lpdf(def_raw[i,h] | 1, mu_def[i,h], sigma_def_t[i]);
        }
      }
    }
    // Hyperpriors for K&L variance parameters
    if (prior_dist_sd_num == 1) {
      target += normal_lpdf(sigma_att_kl[1] | hyper_sd_location, hyper_sd_scale);
      target += normal_lpdf(sigma_def_kl[1] | hyper_sd_location, hyper_sd_scale);
      target += normal_lpdf(sigma_break[1] | hyper_sd_location, hyper_sd_scale);
    }
    else if (prior_dist_sd_num == 2) {
      target += student_t_lpdf(sigma_att_kl[1] | hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      target += student_t_lpdf(sigma_def_kl[1] | hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      target += student_t_lpdf(sigma_break[1] | hyper_sd_df, hyper_sd_location, hyper_sd_scale);
    }
    else if (prior_dist_sd_num == 3) {
      target += cauchy_lpdf(sigma_att_kl[1] | hyper_sd_location, hyper_sd_scale);
      target += cauchy_lpdf(sigma_def_kl[1] | hyper_sd_location, hyper_sd_scale);
      target += cauchy_lpdf(sigma_break[1] | hyper_sd_location, hyper_sd_scale);
    }
    else if (prior_dist_sd_num == 4) {
      target += double_exponential_lpdf(sigma_att_kl[1] | hyper_sd_location, hyper_sd_scale);
      target += double_exponential_lpdf(sigma_def_kl[1] | hyper_sd_location, hyper_sd_scale);
      target += double_exponential_lpdf(sigma_break[1] | hyper_sd_location, hyper_sd_scale);
    }
  }
  // ========================================
  // Owen (2011) and Egidi et al. (2018) Approaches
  // ========================================
  else {
    for (h in 1:nteams) {
      for (i in 1:ntimes) {
        if (prior_dist_num == 1) {
          if (ind_common_sigma == 0) {
            target += normal_lpdf(att_raw[i,h] | mu_att[i,h], sigma_att[1]);
            target += normal_lpdf(def_raw[i,h] | mu_def[i,h], sigma_def[1]);
          } else {
            target += normal_lpdf(att_raw[i,h] | mu_att[i,h], sigma_common[1]);
            target += normal_lpdf(def_raw[i,h] | mu_def[i,h], sigma_common[1]);
          }
        }
        else if (prior_dist_num == 2) {
          if (ind_common_sigma == 0) {
            target += student_t_lpdf(att_raw[i,h] | hyper_df, mu_att[i,h], sigma_att[1]);
            target += student_t_lpdf(def_raw[i,h] | hyper_df, mu_def[i,h], sigma_def[1]);
          } else {
            target += student_t_lpdf(att_raw[i,h] | hyper_df, mu_att[i,h], sigma_common[1]);
            target += student_t_lpdf(def_raw[i,h] | hyper_df, mu_def[i,h], sigma_common[1]);
          }
        }
        else if (prior_dist_num == 3) {
          if (ind_common_sigma == 0) {
            target += student_t_lpdf(att_raw[i,h] | 1, mu_att[i,h], sigma_att[1]);
            target += student_t_lpdf(def_raw[i,h] | 1, mu_def[i,h], sigma_def[1]);
          } else {
            target += student_t_lpdf(att_raw[i,h] | 1, mu_att[i,h], sigma_common[1]);
            target += student_t_lpdf(def_raw[i,h] | 1, mu_def[i,h], sigma_common[1]);
          }
        }
      }
    }
    // Hyperpriors for sd parameters
    if (prior_dist_sd_num == 1) {
      if (ind_common_sigma == 0) {
        target += normal_lpdf(sigma_att[1] | hyper_sd_location, hyper_sd_scale);
        target += normal_lpdf(sigma_def[1] | hyper_sd_location, hyper_sd_scale);
      } else {
        target += normal_lpdf(sigma_common[1] | hyper_sd_location, hyper_sd_scale);
      }
    }
    else if (prior_dist_sd_num == 2) {
      if (ind_common_sigma == 0) {
        target += student_t_lpdf(sigma_att[1] | hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target += student_t_lpdf(sigma_def[1] | hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      } else {
        target += student_t_lpdf(sigma_common[1] | hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
    }
    else if (prior_dist_sd_num == 3) {
      if (ind_common_sigma == 0) {
        target += cauchy_lpdf(sigma_att[1] | hyper_sd_location, hyper_sd_scale);
        target += cauchy_lpdf(sigma_def[1] | hyper_sd_location, hyper_sd_scale);
      } else {
        target += cauchy_lpdf(sigma_common[1] | hyper_sd_location, hyper_sd_scale);
      }
    }
    else if (prior_dist_sd_num == 4) {
      if (ind_common_sigma == 0) {
        target += double_exponential_lpdf(sigma_att[1] | hyper_sd_location, hyper_sd_scale);
        target += double_exponential_lpdf(sigma_def[1] | hyper_sd_location, hyper_sd_scale);
      } else {
        target += double_exponential_lpdf(sigma_common[1] | hyper_sd_location, hyper_sd_scale);
      }
    }
  }

  // Priors for home effect
  for (t in 1:ntimes) {
    target += normal_lpdf(home[t] | mean_home, sd_home);
  }
  target += normal_lpdf(gamma | 0, 1);

  // Likelihood
  target += poisson_lpmf(y[,1] | theta_home);
  target += poisson_lpmf(y[,2] | theta_away);
}

generated quantities {
  array[N,2] int y_rep;
  vector[N] log_lik;
  array[N] int diff_y_rep;
  array[N_prev,2] int y_prev;
  real max_rate = 1e9;
  vector[N_prev] theta_home_prev;
  vector[N_prev] theta_away_prev;

  // In-sample replications
  for (n in 1:N) {
    y_rep[n,1] = poisson_rng(fmin(theta_home[n], max_rate));
    y_rep[n,2] = poisson_rng(fmin(theta_away[n], max_rate));
    diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
    log_lik[n] = poisson_lpmf(y[n,1] | theta_home[n]) +
                 poisson_lpmf(y[n,2] | theta_away[n]);
  }

  // Out-of-sample predictions
  if (N_prev > 0) {
    for (n in 1:N_prev) {
      theta_home_prev[n] = exp(adj_h_eff[instants_prev[n]] + att[instants_prev[n], team1_prev[n]] +
                               def[instants_prev[n], team2_prev[n]] +
                               (gamma/2)*(ranking[instants_rank[N], team1_prev[n]] -
                                          ranking[instants_rank[N], team2_prev[n]]));
      theta_away_prev[n] = exp(att[instants_prev[n], team2_prev[n]] +
                               def[instants_prev[n], team1_prev[n]] -
                               (gamma/2)*(ranking[instants_rank[N], team1_prev[n]] -
                                          ranking[instants_rank[N], team2_prev[n]]));

      y_prev[n,1] = poisson_rng(fmin(theta_home_prev[n], max_rate));
      y_prev[n,2] = poisson_rng(fmin(theta_away_prev[n], max_rate));
    }
  }
}

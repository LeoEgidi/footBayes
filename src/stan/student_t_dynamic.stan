data {
  int N;                       // number of matches
  int<lower=0> N_prev;
  int nteams;                  // number of teams
  int ntimes;                  // number of dynamic periods for abilities
  int ntimes_rank;             // number of dynamic periods for rankings
  array[N] int team1;
  array[N] int team2;
  array[N_prev] int team1_prev;
  array[N_prev] int team2_prev;
  matrix[N, 2] y;
  real nu;
  array[ntimes] int time;
  array[N] int instants;             // time indices for abilities
  array[N_prev] int instants_prev;
  array[N] int instants_rank;        // time indices for rankings
  matrix[ntimes_rank, nteams] ranking; // rankings over time
  int<lower=0, upper=1> ind_home;
  real mean_home;              // Mean for home effect
  real<lower=0> sd_home;      // Standard deviation for home effect


  // priors part
  int<lower=1, upper=4> prior_dist_num;
  int<lower=1, upper=4> prior_dist_sd_num;

  real hyper_df;
  real hyper_location;

  real hyper_sd_df;
  real hyper_sd_location;
  real hyper_sd_scale;
}
transformed data {
  vector[N] diff_y = y[,1] - y[,2];  // modeled data
}
parameters {
  real beta;                        // common intercept
  matrix[ntimes, nteams] alpha;     // per-team weights over time
  real<lower=0> sigma_a;            // common variance
  real<lower=0> sigma_y;            // noise term
  real<lower=0> sigma_alpha;
  real home;                        // home effect
}
transformed parameters {
  array[ntimes, ntimes_rank, nteams] real ability;
  matrix[ntimes, nteams] mu_alpha;
  real adj_h_eff;                   // Adjusted home effect

  adj_h_eff = home * ind_home;

  for (t in 1:ntimes) {
    for(tr in 1:ntimes_rank) {
      for (h in 1:nteams) {
        ability[t, tr, h] = beta * ranking[tr, h] + alpha[t, h] * sigma_a;
      }
    }
  }

  mu_alpha[1] = rep_row_vector(0, nteams);
  for (t in 2:ntimes) {
    mu_alpha[t] = alpha[t - 1];
  }
}
model {
  // Priors for team-specific abilities
  for (h in 1:nteams) {
    if (prior_dist_num == 1) {
      alpha[, h] ~ multi_normal(mu_alpha[, h], diag_matrix(rep_vector(square(sigma_alpha), ntimes)));
    } else if (prior_dist_num == 2) {
      alpha[, h] ~ multi_student_t(hyper_df, mu_alpha[, h], diag_matrix(rep_vector(square(sigma_alpha), ntimes)));
    } else if (prior_dist_num == 3) {
      alpha[, h] ~ multi_student_t(1, mu_alpha[, h], diag_matrix(rep_vector(square(sigma_alpha), ntimes)));
    }
  }

  // Hyperpriors for standard deviations
  if (prior_dist_sd_num == 1) {
    sigma_a ~ normal(hyper_sd_location, hyper_sd_scale);
    sigma_alpha ~ normal(hyper_sd_location, hyper_sd_scale);
  } else if (prior_dist_sd_num == 2) {
    sigma_a ~ student_t(hyper_sd_df, hyper_sd_location, hyper_sd_scale);
    sigma_alpha ~ student_t(hyper_sd_df, hyper_sd_location, hyper_sd_scale);
  } else if (prior_dist_sd_num == 3) {
    sigma_a ~ cauchy(hyper_sd_location, hyper_sd_scale);
    sigma_alpha ~ cauchy(hyper_sd_location, hyper_sd_scale);
  } else if (prior_dist_sd_num == 4) {
    sigma_a ~ double_exponential(hyper_sd_location, hyper_sd_scale);
    sigma_alpha ~ double_exponential(hyper_sd_location, hyper_sd_scale);
  }

  // Priors for other parameters
  beta ~ normal(0, 2.5);
  sigma_y ~ normal(0, 2.5);
  target+=normal_lpdf(home|mean_home,sd_home);

  // Likelihood
  for (n in 1:N) {
    diff_y[n] ~ student_t(
    nu,
    adj_h_eff+
    ability[instants[n], instants_rank[n], team1[n]] - ability[instants[n], instants_rank[n], team2[n]],
    sigma_y);

  }
}
generated quantities {
  vector[N] diff_y_rep;
  vector[N] log_lik;
  vector[N_prev] diff_y_prev;

  for (n in 1:N) {
    diff_y_rep[n] = student_t_rng(nu, adj_h_eff+ability[instants[n], instants_rank[n], team1[n]] - ability[instants[n], instants_rank[n], team2[n]],
    sigma_y);
    log_lik[n] = student_t_lpdf(diff_y[n] | nu, adj_h_eff+ability[instants[n], instants_rank[n], team1[n]] - ability[instants[n], instants_rank[n], team2[n]],
    sigma_y);
  }
  //out-of-sample predictions
  if (N_prev > 0) {
    for (n in 1:N_prev) {
      diff_y_prev[n] = student_t_rng( nu, adj_h_eff+ability[instants_prev[n], instants_rank[N], team1_prev[n]] - ability[instants_prev[n], instants_rank[N], team2_prev[n]],
      sigma_y);
    }
  }
}


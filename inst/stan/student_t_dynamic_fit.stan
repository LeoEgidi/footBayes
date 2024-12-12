data {
      int N;                       // number of matches
      int nteams;                  // number of teams
      array[N] int team1;                // team 1 indices
      array[N] int team2;                // team 2 indices
      matrix[N, 2] y;              // scores: column 1 is team1, column 2 is team2
      int ntimes;                  // number of dynamic periods for abilities
      int ntimes_rank;             // number of dynamic periods for rankings
      array[N] int instants;             // time indices for abilities
      array[N] int instants_rank;
      matrix[ntimes_rank, nteams] ranking; // rankings over time
      real nu;                     // degrees of freedom for the Student's t-distribution


      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

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
  real beta;
  matrix[ntimes, nteams] alpha;
  real<lower=0> sigma_a;
  real<lower=0> sigma_y;
  real<lower=0> sigma_alpha;
}
transformed parameters {
  array[ntimes, ntimes_rank, nteams] real ability;
  matrix[ntimes, nteams] mu_alpha;

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
      alpha[,h] ~ multi_normal(mu_alpha[,h], diag_matrix(rep_vector(square(sigma_alpha), ntimes)));
    } else if (prior_dist_num == 2) {
      alpha[,h] ~ multi_student_t(hyper_df, mu_alpha[,h], diag_matrix(rep_vector(square(sigma_alpha), ntimes)));
    } else if (prior_dist_num == 3) {
      alpha[,h] ~ multi_student_t(1, mu_alpha[,h], diag_matrix(rep_vector(square(sigma_alpha), ntimes)));
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

  // Likelihood
  for (n in 1:N) {
    diff_y[n] ~ student_t(
    nu,
    ability[instants[n], instants_rank[n], team1[n]] - ability[instants[n], instants_rank[n], team2[n]],
    sigma_y);

  }
}
generated quantities {
  vector[N] diff_y_rep;
  vector[N] log_lik;
  for (n in 1:N) {
    diff_y_rep[n] = student_t_rng(nu, ability[instants[n], instants_rank[n], team1[n]] - ability[instants[n], instants_rank[n], team2[n]],
    sigma_y);
    log_lik[n] = student_t_lpdf(diff_y[n] | nu, ability[instants[n], instants_rank[n], team1[n]] - ability[instants[n], instants_rank[n], team2[n]],
    sigma_y);
  }
}

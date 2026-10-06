functions {
  real bipois_lpmf(array[] int r, real mu1, real mu2, real mu3) {
    int miny = min(r[1], r[2]);
    real ss = poisson_lpmf(r[1] | mu1) + poisson_lpmf(r[2] | mu2) - mu3;
    if (miny > 0) {
      real mus = log(mu3) - log(mu1) - log(mu2);
      real log_s = ss;
      for (k in 1:miny) {
        log_s += log(r[1] - k + 1) + log(r[2] - k + 1) - log(k) + mus;
        ss = log_sum_exp(ss, log_s);
      }
    }
    return ss;
  }

  // Diagonal-inflated bivariate Poisson (Karlis and Ntzoufras, 2003):
  // with probability p the result is a draw j-j, with j = 0, ..., J drawn from
  // the discrete distribution draw_dist (Pr(j-j) = draw_dist[j + 1]);
  // with probability 1 - p the result follows a bivariate Poisson.
  real diag_infl_bipois_lpmf(array[] int r, real mu1, real mu2, real mu3,
                             real p, vector draw_dist) {
    real lp_bp = log1m(p) + bipois_lpmf(r | mu1, mu2, mu3);
    if (r[1] == r[2] && r[1] < num_elements(draw_dist)) {
      return log_sum_exp(log(p) + log(draw_dist[r[1] + 1]), lp_bp);
    }
    return lp_bp;
  }

  array[] int diag_infl_bipois_rng(real mu1, real mu2, real mu3,
                                   real p, vector draw_dist) {
    array[2] int r;
    if (bernoulli_rng(p)) {
      int j = categorical_rng(draw_dist) - 1;
      r[1] = j;
      r[2] = j;
    } else {
      int x3 = poisson_rng(mu3);
      r[1] = poisson_rng(mu1) + x3;
      r[2] = poisson_rng(mu2) + x3;
    }
    return r;
  }
}
data {
  int N;                             // number of games
  int<lower=0> N_prev;
  array[N, 2] int y;
  int nteams;
  array[N] int team1;
  array[N] int team2;
  array[N_prev] int team1_prev;
  array[N_prev] int team2_prev;
  array[N] int instants_rank;
  int ntimes_rank;                   // dynamic periods for ranking
  matrix[ntimes_rank, nteams] ranking;
  int<lower=0, upper=1> ind_home;
  real mean_home;                    // Mean for home effect
  real<lower=1e-8> sd_home;          // Standard deviation for home effect

  // priors part
  int<lower=1, upper=4> prior_dist_num;     // 1 gaussian, 2 t, 3 cauchy, 4 laplace
  int<lower=1, upper=4> prior_dist_sd_num;  // 1 gaussian, 2 t, 3 cauchy, 4 laplace

  real<lower=0> hyper_df;
  real hyper_location;

  real<lower=0> hyper_sd_df;
  real hyper_sd_location;
  real<lower=1e-8> hyper_sd_scale;
}
transformed data {
  int J = 3;                         // inflated draws: 0-0, 1-1, 2-2, 3-3
}
parameters {
  vector[nteams] att_raw;
  vector[nteams] def_raw;
  real<lower=1e-8> sigma_att;
  real<lower=1e-8> sigma_def;
  real home;
  real rho;
  real gamma;
  real<lower=0, upper=1> prob_of_draws;  // probability of the inflation component
  simplex[J + 1] draw_dist;              // Pr(j-j | inflation), j = 0, ..., J
}
transformed parameters {
  real adj_h_eff = home * ind_home;           // Adjusted home effect
  vector[nteams] att = att_raw - mean(att_raw);
  vector[nteams] def = def_raw - mean(def_raw);
  array[N] vector[3] theta;

  for (n in 1:N) {
    real rank_diff = ranking[instants_rank[n], team1[n]]
                     - ranking[instants_rank[n], team2[n]];
    theta[n, 1] = exp(adj_h_eff + att[team1[n]] + def[team2[n]] + (gamma / 2) * rank_diff);
    theta[n, 2] = exp(att[team2[n]] + def[team1[n]] - (gamma / 2) * rank_diff);
    theta[n, 3] = exp(rho);
  }
}
model {
  // log-priors for team-specific abilities
  if (prior_dist_num == 1) {
    target += normal_lpdf(att_raw | hyper_location, sigma_att);
    target += normal_lpdf(def_raw | hyper_location, sigma_def);
  } else if (prior_dist_num == 2) {
    target += student_t_lpdf(att_raw | hyper_df, hyper_location, sigma_att);
    target += student_t_lpdf(def_raw | hyper_df, hyper_location, sigma_def);
  } else if (prior_dist_num == 3) {
    target += cauchy_lpdf(att_raw | hyper_location, sigma_att);
    target += cauchy_lpdf(def_raw | hyper_location, sigma_def);
  } else if (prior_dist_num == 4) {
    target += double_exponential_lpdf(att_raw | hyper_location, sigma_att);
    target += double_exponential_lpdf(def_raw | hyper_location, sigma_def);
  }

  // log-hyperpriors for sd parameters
  if (prior_dist_sd_num == 1) {
    target += normal_lpdf(sigma_att | hyper_sd_location, hyper_sd_scale);
    target += normal_lpdf(sigma_def | hyper_sd_location, hyper_sd_scale);
  } else if (prior_dist_sd_num == 2) {
    target += student_t_lpdf(sigma_att | hyper_sd_df, hyper_sd_location, hyper_sd_scale);
    target += student_t_lpdf(sigma_def | hyper_sd_df, hyper_sd_location, hyper_sd_scale);
  } else if (prior_dist_sd_num == 3) {
    target += cauchy_lpdf(sigma_att | hyper_sd_location, hyper_sd_scale);
    target += cauchy_lpdf(sigma_def | hyper_sd_location, hyper_sd_scale);
  } else if (prior_dist_sd_num == 4) {
    target += double_exponential_lpdf(sigma_att | hyper_sd_location, hyper_sd_scale);
    target += double_exponential_lpdf(sigma_def | hyper_sd_location, hyper_sd_scale);
  }

  // log-priors fixed effects
  target += normal_lpdf(home | mean_home, sd_home);
  target += normal_lpdf(rho | 0, 1);
  target += normal_lpdf(gamma | 0, 1);
  target += uniform_lpdf(prob_of_draws | 0, 1);
  target += dirichlet_lpdf(draw_dist | rep_vector(1, J + 1));

  // diagonal-inflated bivariate Poisson likelihood
  for (n in 1:N) {
    target += diag_infl_bipois_lpmf(y[n] | theta[n, 1], theta[n, 2], theta[n, 3],
                                    prob_of_draws, draw_dist);
  }
}
generated quantities {
  array[N, 2] int y_rep;
  array[N] int diff_y_rep;
  vector[N] log_lik;
  array[N_prev, 2] int y_prev;
  array[N_prev] vector[3] theta_prev;
  array[N_prev] int diff_y_prev;

  // max_rate
  {
    real max_rate = 1e9;

    // in-sample replications
    for (n in 1:N) {
      y_rep[n] = diag_infl_bipois_rng(fmin(theta[n, 1], max_rate),
                                      fmin(theta[n, 2], max_rate),
                                      fmin(theta[n, 3], max_rate),
                                      prob_of_draws, draw_dist);
      diff_y_rep[n] = y_rep[n, 1] - y_rep[n, 2];
      log_lik[n] = diag_infl_bipois_lpmf(y[n] | theta[n, 1], theta[n, 2], theta[n, 3],
                                         prob_of_draws, draw_dist);
    }

    // out-of-sample predictions
    if (N_prev > 0) {
      int t_last = max(instants_rank);
      for (n in 1:N_prev) {
        real rank_diff = ranking[t_last, team1_prev[n]]
                         - ranking[t_last, team2_prev[n]];
        theta_prev[n, 1] = exp(adj_h_eff + att[team1_prev[n]] + def[team2_prev[n]]
                               + (gamma / 2) * rank_diff);
        theta_prev[n, 2] = exp(att[team2_prev[n]] + def[team1_prev[n]]
                               - (gamma / 2) * rank_diff);
        theta_prev[n, 3] = exp(rho);
        y_prev[n] = diag_infl_bipois_rng(fmin(theta_prev[n, 1], max_rate),
                                         fmin(theta_prev[n, 2], max_rate),
                                         fmin(theta_prev[n, 3], max_rate),
                                         prob_of_draws, draw_dist);
        diff_y_prev[n] = y_prev[n, 1] - y_prev[n, 2];
      }
    }
  }
}

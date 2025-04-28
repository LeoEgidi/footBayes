data {
  int<lower=1> N;                          // number of games
  int<lower=0> N_prev;
  array[N,2] int y;                        // observed scores
  int<lower=1> nteams;
  array[N] int instants_rank;
  int<lower=1> ntimes_rank;                // number of ranking periods
  array[N] int team1;
  array[N] int team2;
  array[N_prev]int team1_prev;
  array[N_prev] int team2_prev;
  matrix[ntimes_rank, nteams] ranking;
  int<lower=0, upper=1> ind_home;
  real mean_home;                          // prior mean for home effect
  real<lower=0> sd_home;                  // prior sd for home effect

  // choice of prior distributions for att/def
  int<lower=1,upper=4> prior_dist_num;     // 1=normal,2=t,3=cauchy,4=laplace
  int<lower=1,upper=4> prior_dist_sd_num;  // same for sd parameters

  real<lower=0> hyper_df;                  // d.f. for t‑prior on abilities
  real hyper_location;                     // location for ability priors

  real<lower=0> hyper_sd_df;               // d.f. for t‑prior on sds
  real hyper_sd_location;                  // location for sd priors
  real<lower=0> hyper_sd_scale;            // scale for sd priors
}

parameters {
  vector[nteams] att_raw;
  vector[nteams] def_raw;
  real<lower=0> sigma_att;
  real<lower=0> sigma_def;
  real home;
  real gamma;

  // Negative Binomial dispersion parameters
  real<lower=0> phi1;
  real<lower=0> phi2;
}

transformed parameters {
  vector[nteams] att = att_raw - mean(att_raw);
  vector[nteams] def = def_raw - mean(def_raw);
  real adj_h_eff;
  array[N] vector[2] theta;

  adj_h_eff = home * ind_home;

  for (n in 1:N) {
    theta[n,1] = exp(
      adj_h_eff
      + att[team1[n]] + def[team2[n]]
      + (gamma/2) * (ranking[instants_rank[n], team1[n]]
                     - ranking[instants_rank[n], team2[n]])
    );
    theta[n,2] = exp(
      att[team2[n]] + def[team1[n]]
      - (gamma/2) * (ranking[instants_rank[n], team1[n]]
                     - ranking[instants_rank[n], team2[n]])
    );
  }
}

model {
  // 1) Priors on team abilities (via target+=)
  for (t in 1:nteams) {
    if (prior_dist_num == 1) {
      target += normal_lpdf(att_raw[t]   | hyper_location, sigma_att);
      target += normal_lpdf(def_raw[t]   | hyper_location, sigma_def);
    } else if (prior_dist_num == 2) {
      target += student_t_lpdf(att_raw[t] | hyper_df, hyper_location, sigma_att);
      target += student_t_lpdf(def_raw[t] | hyper_df, hyper_location, sigma_def);
    } else if (prior_dist_num == 3) {
      target += cauchy_lpdf(att_raw[t]   | hyper_location, sigma_att);
      target += cauchy_lpdf(def_raw[t]   | hyper_location, sigma_def);
    } else {
      target += double_exponential_lpdf(att_raw[t] | hyper_location, sigma_att);
      target += double_exponential_lpdf(def_raw[t] | hyper_location, sigma_def);
    }
  }

  // 2) Priors on sigma_att, sigma_def
  if (prior_dist_sd_num == 1) {
    target += normal_lpdf(sigma_att | hyper_sd_location, hyper_sd_scale);
    target += normal_lpdf(sigma_def | hyper_sd_location, hyper_sd_scale);
  } else if (prior_dist_sd_num == 2) {
    target += student_t_lpdf(sigma_att | hyper_sd_df, hyper_sd_location, hyper_sd_scale);
    target += student_t_lpdf(sigma_def | hyper_sd_df, hyper_sd_location, hyper_sd_scale);
  } else if (prior_dist_sd_num == 3) {
    target += cauchy_lpdf(sigma_att | hyper_sd_location, hyper_sd_scale);
    target += cauchy_lpdf(sigma_def | hyper_sd_location, hyper_sd_scale);
  } else {
    target += double_exponential_lpdf(sigma_att | hyper_sd_location, hyper_sd_scale);
    target += double_exponential_lpdf(sigma_def | hyper_sd_location, hyper_sd_scale);
  }

  // 3) Priors on fixed effects
  target += normal_lpdf(home  | mean_home, sd_home);
  target += normal_lpdf(gamma | 0, 1);

  // 4) Priors on NB dispersion
  target += normal_lpdf(phi1 | 0, 5);
  target += normal_lpdf(phi2 | 0, 5);

  // 5) Likelihood: marginal NB2 for each margin + shared latent component
  for (n in 1:N) {
    target += neg_binomial_2_lpmf(y[n,1] | theta[n,1], phi1);
    target += neg_binomial_2_lpmf(y[n,2] | theta[n,2], phi2);
  }
}

generated quantities {
  array[N,2] int y_rep;
  array[N_prev,2] int y_prev;
  array[N_prev] vector[2] theta_prev;
  vector[N] log_lik;
  array[N] int diff_y_rep;

  for (n in 1:N) {
    y_rep[n,1]    = neg_binomial_2_rng(theta[n,1], phi1);
    y_rep[n,2]    = neg_binomial_2_rng(theta[n,2], phi2);
    diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];

    log_lik[n] = neg_binomial_2_lpmf(y[n,1] | theta[n,1], phi1)
               + neg_binomial_2_lpmf(y[n,2] | theta[n,2], phi2);
  }

  //out-of-sample predictions
  if (N_prev > 0) {
    for (n in 1:N_prev){
      theta_prev[n,1] = exp(adj_h_eff+att[team1_prev[n]]+
                            def[team2_prev[n]]+
                           (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
      theta_prev[n,2] = exp(att[team2_prev[n]]+
                            def[team1_prev[n]]-
                           (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
      y_prev[n,1] = neg_binomial_2_rng(theta_prev[n,1], phi1);
      y_prev[n,2] = neg_binomial_2_rng(theta_prev[n,2], phi2);
    }
  }
}

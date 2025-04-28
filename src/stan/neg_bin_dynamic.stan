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
      matrix[ntimes_rank,nteams] ranking;      // eventual fifa/uefa ranking
      int<lower=0, upper=1> ind_home;
      real mean_home;              // Mean for home effect
      real<lower=0> sd_home;      // Standard deviation for home effect


      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }

parameters {
  matrix[ntimes, nteams] att_raw;        // raw attack ability
  matrix[ntimes, nteams] def_raw;        // raw defense ability
  real<lower=0> sigma_att;
  real<lower=0> sigma_def;
  real home;
  real gamma;

  // Negative Binomial dispersion parameters
  real<lower=0> phi1;
  real<lower=0> phi2;
}

transformed parameters {
  matrix[ntimes, nteams] att;            // attack abilities
  matrix[ntimes, nteams] def;            // defense abilities
  matrix[ntimes, nteams] mu_att;         // attack hyperparameter
  matrix[ntimes, nteams] mu_def;         // defense hyperparameter
  real adj_h_eff;
  array[N] vector[2] theta;

  // Sum-to-zero constraint for attack/defense parameters
  att[1]=att_raw[1]-mean(att_raw[1]);
  def[1]=def_raw[1]-mean(def_raw[1]);
  for (t in 2:ntimes){
    att[t]=att_raw[t]-mean(att_raw[t]);
    def[t]=def_raw[t]-mean(def_raw[t]);
  }
  // Lagged prior mean for attack/defense parameters
  for (t in 2:(ntimes)){
    mu_att[1]=rep_row_vector(hyper_location,nteams);
    mu_att[t]=att[t-1];
    //rep_row_vector(0,nteams);

    mu_def[1]=rep_row_vector(hyper_location,nteams);
    mu_def[t]=def[t-1];
    //rep_row_vector(0,nteams);
    }

  adj_h_eff = home * ind_home;

  for (n in 1:N){
    theta[n,1] = exp(adj_h_eff+att[instants[n], team1[n]]+def[instants[n], team2[n]]+
                     (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
    theta[n,2] = exp(att[instants[n], team2[n]]+def[instants[n], team1[n]]-
                     (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
  }
}

model {
  // log-priors for team-specific abilities
  for (h in 1:(nteams)){
    if (prior_dist_num == 1 ){
      att_raw[,h]~multi_normal(mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
      def_raw[,h]~multi_normal(mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
    }
    else if (prior_dist_num == 2 ){
      att_raw[,h]~multi_student_t(hyper_df, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
      def_raw[,h]~multi_student_t(hyper_df, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
    }
    else if (prior_dist_num == 3 ){
      att_raw[,h]~multi_student_t(1, mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
      def_raw[,h]~multi_student_t(1, mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
    }
  }

  // log-hyperpriors for sd parameters
  if (prior_dist_sd_num == 1 ){
    target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
    target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
  }
  else if (prior_dist_sd_num == 2){
    target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
    target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
  }
  else if (prior_dist_sd_num == 3){
    target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
    target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
  }
  else if (prior_dist_sd_num == 4){
    target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
    target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
  }

  // log-priors fixed effects
  target+=normal_lpdf(home|mean_home,sd_home);
  target+=normal_lpdf(gamma|0,1);

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
      theta_prev[n,1] = exp(adj_h_eff+att[instants_prev[n],team1_prev[n]]+
                            def[instants_prev[n], team2_prev[n]]+
                           (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
      theta_prev[n,2] = exp(att[instants_prev[n],team2_prev[n]]+
                            def[instants_prev[n], team1_prev[n]]-
                           (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
      y_prev[n,1] = neg_binomial_2_rng(theta_prev[n,1], phi1);
      y_prev[n,2] = neg_binomial_2_rng(theta_prev[n,2], phi2);
    }
  }
}


data {
      int N;                       // number of matches
      int N_prev;                  // number of predicted matches
      int nteams;                  // number of teams
      int ntimes_rank;             // number of dynamic periods for rankings
      int instants_rank[N];        // time indices for rankings for each match
      int instants_rank_prev[N_prev]; // time indices for rankings for predicted matches
      matrix[ntimes_rank, nteams] ranking; // rankings over time

      int team1[N];
      int team2[N];
      int team1_prev[N_prev];
      int team2_prev[N_prev];
      matrix[N, 2] y;
      real nu;

      // Priors part
      int<lower=1, upper=4> prior_dist_num;    // 1 Gaussian, 2 Student's t, 3 Cauchy, 4 Laplace
      int<lower=1, upper=4> prior_dist_sd_num; // 1 Gaussian, 2 Student's t, 3 Cauchy, 4 Laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }

    transformed data {
      vector[N] diff_y = y[, 1] - y[, 2];  // Modeled data
    }

    parameters {
      real beta;                           // Coefficient for the ranking
      vector[nteams] alpha;                // Team-specific abilities
      real<lower=0> sigma_a;               // Scaling parameter for abilities
      real<lower=0> sigma_y;               // Noise term in our estimate
      real<lower=0> sigma_alpha;
    }

    transformed parameters {
      // mixed effects model - common intercept + random effects
      vector[N] ability_team1;
      vector[N] ability_team2;
      vector[N_prev] ability_team1_prev;
      vector[N_prev] ability_team2_prev;

      // Compute abilities for each match at the data point level
      for (n in 1:N) {
        ability_team1[n] = beta * ranking[instants_rank[n], team1[n]] + alpha[team1[n]] * sigma_a;
        ability_team2[n] = beta * ranking[instants_rank[n], team2[n]] + alpha[team2[n]] * sigma_a;
      }

      // Compute abilities for predicted matches
      for (n in 1:N_prev) {
        ability_team1_prev[n] = beta * ranking[instants_rank[N], team1_prev[n]] + alpha[team1_prev[n]] * sigma_a;
        ability_team2_prev[n] = beta * ranking[instants_rank[N], team2_prev[n]] + alpha[team2_prev[n]] * sigma_a;
      }
  }

    model {
      // log-priors for team-specific abilities
      for (t in 1:(nteams)){
        if (prior_dist_num == 1){
          target+= normal_lpdf(alpha[t]|hyper_location, sigma_alpha);
        }
        else if (prior_dist_num == 2){
          target+= student_t_lpdf(alpha[t]|hyper_df, hyper_location, sigma_alpha);

        }
        else if (prior_dist_num == 3){
          target+= cauchy_lpdf(alpha[t]|hyper_location, sigma_alpha);

        }
        else if (prior_dist_num == 4){
          target+= double_exponential_lpdf(alpha[t]|hyper_location, sigma_alpha);

        }
      }


      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_a|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_alpha|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_a|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_alpha|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_a|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_alpha|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_a|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_alpha|hyper_sd_location, hyper_sd_scale);
      }

      beta ~ normal(0, 2.5);
      sigma_y ~ normal(0, 2.5);

      // Likelihood
      for (n in 1:N) {
        diff_y[n] ~ student_t(nu, ability_team1[n] - ability_team2[n], sigma_y);
      }
    }

    generated quantities {
     // posterior predictive check - carry along uncertainty!!!
     // now estimate a whole season's worth of games
     // based on the current estimate of our parameters
    vector[N] diff_y_rep;
    vector[N] log_lik;
    vector[N_prev] diff_y_prev;

    for (n in 1:N) {
      diff_y_rep[n] = student_t_rng(nu, ability_team1[n] - ability_team2[n], sigma_y);
      log_lik[n] = student_t_lpdf(diff_y[n]| nu, ability_team1[n] - ability_team2[n], sigma_y);
    }

    for (n in 1:N_prev) {
      diff_y_prev[n] = student_t_rng(nu, ability_team1_prev[n] - ability_team2_prev[n], sigma_y);
    }
}

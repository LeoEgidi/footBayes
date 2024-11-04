data {
      int N;                       // number of matches
      int nteams;                  // number of teams
      int ntimes_rank;             // number of dynamic periods for rankings
      int instants_rank[N];        // time indices for rankings for each match
      matrix[ntimes_rank, nteams] ranking; // rankings over time

      int team1[N];                // team 1 indices for N matches
      int team2[N];                // team 2 indices for N matches
      matrix[N, 2] y;              // observed scores: column 1 is team1, column 2 is team2
      real nu;                     // degrees of freedom for the Student's t-distribution

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
      vector[N] diff_y = y[, 1] - y[, 2]; // Difference in scores
    }

    parameters {
      real beta;                           // Coefficient for the ranking
      vector[nteams] alpha;                // Team-specific abilities
      real<lower=0> sigma_a;               // Scaling parameter for abilities
      real<lower=0> sigma_y;               // Noise term in the model
      real<lower=0> sigma_alpha;           // Standard deviation for alpha's prior
    }

    transformed parameters {
      // mixed effects model - common intercept + random effects
      vector[N] ability_team1;
      vector[N] ability_team2;

      for (n in 1:N) {
        ability_team1[n] = beta * ranking[instants_rank[n], team1[n]] + alpha[team1[n]] * sigma_a;
        ability_team2[n] = beta * ranking[instants_rank[n], team2[n]] + alpha[team2[n]] * sigma_a;
      }
    }

    model {
    // log-priors for team-specific abilities
        for (t in 1:nteams) {
          if (prior_dist_num == 1) {
            alpha[t] ~ normal(hyper_location, sigma_alpha);
          } else if (prior_dist_num == 2) {
            alpha[t] ~ student_t(hyper_df, hyper_location, sigma_alpha);
          } else if (prior_dist_num == 3) {
            alpha[t] ~ cauchy(hyper_location, sigma_alpha);
          } else if (prior_dist_num == 4) {
            alpha[t] ~ double_exponential(hyper_location, sigma_alpha);
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
     // Priors for beta and sigma_y
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
      vector[N] diff_y_rep; // Replicated differences for posterior predictive checks
      vector[N] log_lik;    // Log-likelihood for each observation

      for (n in 1:N) {
        diff_y_rep[n] = student_t_rng(nu, ability_team1[n] - ability_team2[n], sigma_y);
        log_lik[n] = student_t_lpdf(diff_y[n] | nu, ability_team1[n] - ability_team2[n], sigma_y);
      }
    }

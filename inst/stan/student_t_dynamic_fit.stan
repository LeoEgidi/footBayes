
data {
      int N;                       // number of matches
      int nteams;                  // number of teams
      int team1[N];                // team 1 indices
      int team2[N];                // team 2 indices
      matrix[N, 2] y;              // scores: column 1 is team1, column 2 is team2
      int ntimes;                  // number of dynamic periods for abilities
      int ntimes_rank;             // number of dynamic periods for rankings
      int instants[N];             // time indices for abilities
      int instants_rank[N];        // time indices for rankings
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
      real beta;                           // Coefficient for the ranking
      matrix[ntimes, nteams] alpha;        // Team-specific abilities over time
      real<lower=0> sigma_a;               // Scaling parameter for abilities
      real<lower=0> sigma_y;               // Noise term in the model
      real<lower=0> sigma_alpha;           // Standard deviation for alpha's prior
    }
    transformed parameters {
      //cov_matrix[ntimes] Sigma_alpha;
      // mixed effects model - common intercept + random effects
      vector[N] ability_team1;
      vector[N] ability_team2;
      matrix[ntimes, nteams] mu_alpha;

      // Compute abilities for each match at the data point level
        for (n in 1:N) {
          ability_team1[n] = beta * ranking[instants_rank[n], team1[n]] + alpha[instants[n], team1[n]] * sigma_a;
          ability_team2[n] = beta * ranking[instants_rank[n], team2[n]] + alpha[instants[n], team2[n]] * sigma_a;
        }

      // Gaussian process covariance functions
      // for (i in 1:(ntimes)){
        //   for (j in 1:(ntimes)){
          //     Sigma_alpha[i, j] = exp(-pow(time[i] - time[j], 2))
          //     + (i == j ? 0.1 : 0.0);
          //   }}

      // Lagged prior mean for attack/defense parameters
      mu_alpha[1, ] = rep_row_vector(0, nteams);
      for (t in 2:ntimes) {
        mu_alpha[t, ] = alpha[t - 1, ];
      }

    }
    model {

      // log-priors for team-specific abilities
      for (h in 1:(nteams)){
        if (prior_dist_num == 1){
          alpha[,h]~multi_normal(mu_alpha[,h], diag_matrix(rep_vector(square(sigma_alpha), ntimes)));
        } else if (prior_dist_num == 2){
          alpha[,h]~multi_student_t(hyper_df, mu_alpha[,h], diag_matrix(rep_vector(square(sigma_alpha), ntimes)));
        } else if (prior_dist_num == 3){
          alpha[,h]~multi_student_t(1, mu_alpha[,h], diag_matrix(rep_vector(square(sigma_alpha), ntimes)));
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

        vector[N] diff_y_rep;
        vector[N] log_lik;

        for (n in 1:N) {
          diff_y_rep[n] = student_t_rng(nu, ability_team1[n] - ability_team2[n], sigma_y);
          log_lik[n] = student_t_lpdf(diff_y[n] | nu, ability_team1[n] - ability_team2[n], sigma_y);
        }

}

data {
  int N;   // number of matches
  int N_prev; // number of predictedmatched
  int nteams;   // number of teams
  vector[nteams] spi_std;  // per-team ranking
  // this is a 4-column data table of per-game outcomes
  int team1[N];
  int team2[N];
  int team1_prev[N_prev];
  int team2_prev[N_prev];
  matrix[N,2] y;
}
transformed data {
  vector[N] diff_y = y[,1] - y[,2];  // "modeled" data
}
parameters {
  real beta;            // common intercept
  vector[nteams] alpha;    // vector of per-team weights
  real<lower=0> sigma_a;   // common variance
  real<lower=0> sigma_y;   // noise term in our estimate
}
transformed parameters {
  // "mixed effects" model - common intercept + random effects
  vector[nteams] ability = beta * spi_std + alpha * sigma_a;
}
model {
  alpha ~ normal(0, 1); // priors on all parameters
  beta ~ normal(0, 2.5);
  sigma_a ~ normal(0, 2.5);
  sigma_y ~ normal(0, 2.5);

  diff_y ~ student_t(7, ability[team1] - ability[team2], sigma_y);
}
generated quantities {
  // posterior predictive check - carry along uncertainty!!!
  // now estimate a whole season's worth of games
  // based on the current estimate of our parameters
  vector[N] diff_y_rep;
  vector[N] log_lik;
  vector[N_prev] diff_y_prev;

  for (n in 1:N) {
    diff_y_rep[n] = student_t_rng(7, ability[team1[n]] - ability[team2[n]], sigma_y);
    log_lik[n] = student_t_lpdf(diff_y[n]| 7, ability[team1] - ability[team2], sigma_y);
  }

  for (n in 1:N_prev) {
    diff_y_prev[n] = student_t_rng(7, ability[team1_prev[n]] - ability[team2_prev[n]], sigma_y);
  }



}

data {
  int N;   // number of matches
  int nteams;   // number of teams
  vector[nteams] spi_std;  // per-team ranking
  // this is a 4-column data table of per-game outcomes
  int team1[N];
  int team2[N];
  matrix[N,2] y;
  int ntimes;                 // dynamic periods
  int time[ntimes];
  int instants[N];
}
transformed data {
  vector[N] diff_y = y[,1] - y[,2];  // "modeled" data
}
parameters {
  real beta;            // common intercept
  matrix[ntimes, nteams] alpha;    // vector of per-team weights
  real<lower=0> sigma_a;   // common variance
  real<lower=0> sigma_y;   // noise term in our estimate
  real<lower=0> sigma_alpha;
}
transformed parameters {
  //cov_matrix[ntimes] Sigma_alpha;
  // "mixed effects" model - common intercept + random effects
  matrix[ntimes, nteams] ability;
  matrix[ntimes, nteams] mu_alpha;

  for (t in 1: ntimes){
    ability[t]= to_row_vector(beta*spi_std) + alpha[t]*sigma_a;
  }

   // Gaussian process covariance functions
   // for (i in 1:(ntimes)){
   //   for (j in 1:(ntimes)){
   //     Sigma_alpha[i, j] = exp(-pow(time[i] - time[j], 2))
   //     + (i == j ? 0.1 : 0.0);
   //   }}

     // Lagged prior mean for attack/defense parameters
   for (t in 2:(ntimes)){
     mu_alpha[1]=rep_row_vector(0,nteams);
     mu_alpha[t]=alpha[t-1];
     //rep_row_vector(0,nteams);
     }

}
model {
  for (h in 1:(nteams)){
     alpha[,h]~multi_normal(mu_alpha[,h], diag_matrix(rep_vector(square(sigma_alpha), ntimes)));
  }
  beta ~ normal(0, 2.5);
  sigma_a ~ normal(0, 2.5);
  sigma_y ~ normal(0, 2.5);
  sigma_alpha ~ normal(0, 2.5);

  for (n in 1:N)
  diff_y[n] ~ student_t(7, ability[instants[n], team1[n]] - ability[instants[n], team2[n]], sigma_y);
}
generated quantities {
  // posterior predictive check - carry along uncertainty!!!
  // now estimate a whole season's worth of games
  // based on the current estimate of our parameters
  vector[N] diff_y_rep;
  vector[N] log_lik;
  for (n in 1:N) {
    diff_y_rep[n] = student_t_rng(7, ability[instants[n],team1[n]] - ability[instants[n],team2[n]], sigma_y);
    log_lik[n] = student_t_lpdf(diff_y[n]| 7, ability[instants[n],team1[n]] - ability[instants[n],team2[n]], sigma_y);
  }

}

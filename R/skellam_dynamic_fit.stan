functions{
  real skellam_lpmf(int k, real lambda1, real lambda2) {
    real r = k;
    return -(lambda1 + lambda2) + (r/2) * log(lambda1/lambda2) +
      log(modified_bessel_first_kind(k, 2 * sqrt(lambda1 * lambda2)));
  }
}
data{
  int N;
  int diff_y[N];
  int nteams;
  int team1[N];
  int team2[N];
  int ntimes;                 // dynamic periods
  int time[ntimes];
  int instants[N];
}
parameters{
  matrix[ntimes, nteams] att_raw;        // raw attack ability
  matrix[ntimes, nteams] def_raw;        // raw defense ability
  real home;
}
transformed parameters{
  matrix[ntimes, nteams] att;            // attack abilities
  matrix[ntimes, nteams] def;            // defense abilities
  cov_matrix[ntimes] Sigma_att;          // Gaussian process attack cov. funct.
  cov_matrix[ntimes] Sigma_def;          // Gaussian process defense cov.funct.
  matrix[ntimes, nteams] mu_att;         // attack hyperparameter
  matrix[ntimes, nteams] mu_def;         // defense hyperparameter
  vector[N] theta_home;                    // exponentiated linear pred.
  vector[N] theta_away;

 for (i in 1:(ntimes)){
     for (j in 1:(ntimes)){
       Sigma_att[i, j] = exp(-pow(time[i] - time[j], 2))
       + (i == j ? 0.1 : 0.0);
       Sigma_def[i, j] = exp(-pow(time[i] - time[j], 2))
                   + (i == j ? 0.1 : 0.0);
     }}

  // Sum-to-zero constraint for attack/defense parameters
  att[1]=att_raw[1]-mean(att_raw[1]);
  def[1]=def_raw[1]-mean(def_raw[1]);
   for (t in 2:ntimes){
      att[t]=att_raw[t]-mean(att_raw[t]);
      def[t]=def_raw[t]-mean(def_raw[t]);
     }

  // Lagged prior mean for attack/defense parameters
   for (t in 2:(ntimes)){
     mu_att[1]=rep_row_vector(0,nteams);
     mu_att[t]=rep_row_vector(0,nteams);

     mu_def[1]=rep_row_vector(0,nteams);
     mu_def[t]=rep_row_vector(0,nteams);

     }

  for (n in 1:N){
    theta_home[n] = exp(home+att[instants[n], team1[n]]+def[instants[n], team2[n]]);
    theta_away[n] = exp(att[instants[n],team2[n]]+def[instants[n], team1[n]]);
   }
}
model{
  // priors
  for (h in 1:(nteams)){
     att_raw[,h]~multi_normal(mu_att[,h], Sigma_att);
     def_raw[,h]~multi_normal(mu_def[,h], Sigma_def);
   }
  target+=normal_lpdf(home|0,5);

  // likelihood
  for (n in 1:N){
    target+=skellam_lpmf(diff_y[n]| theta_home[n],
                                    theta_away[n]);
    }
}
generated quantities{
  int y_rep[N,2];
  int diff_y_rep[N];
  vector[N] log_lik;

  //in-sample replications
  for (n in 1:N){
    y_rep[n,1] = poisson_rng(theta_home[n]);
    y_rep[n,2] = poisson_rng(theta_away[n]);
    diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
    log_lik[n] =skellam_lpmf(diff_y[n]| theta_home[n], theta_away[n]);
  }
}

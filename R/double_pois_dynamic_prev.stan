data{
  int N;   // number of games
  int N_prev;
  int y[N,2];
  int nteams;
  int team1[N];
  int team2[N];
  int team1_prev[N_prev];
  int team2_prev[N_prev];
  int ntimes;                 // dynamic periods
  int time[ntimes];
  int instants[N];
  int instants_prev[N_prev];
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

    // Gaussian process covariance functions
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
    theta_away[n] = exp(att[instants[n], team2[n]]+def[instants[n], team1[n]]);
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
    target+=poisson_lpmf(y[n,1]| theta_home[n]);
    target+=poisson_lpmf(y[n,2]| theta_away[n]);
  }
}
generated quantities{
  int y_rep[N,2];
  vector[N] log_lik;
  int y_prev[N_prev,2];
  vector[N_prev] theta_home_prev;                    // exponentiated linear pred.
  vector[N_prev] theta_away_prev;


  //in-sample replications
  for (n in 1:N){
    y_rep[n,1] = poisson_rng(theta_home[n]);
    y_rep[n,2] = poisson_rng(theta_away[n]);
    log_lik[n] =poisson_lpmf(y[n,1]| theta_home[n])+
                poisson_lpmf(y[n,2]| theta_away[n]);
  }
  //out-of-sample predictions
  for (n in 1:N_prev){
    theta_home_prev[n] = exp(home+att[instants_prev[n],team1_prev[n]]+
                          def[instants_prev[n], team2_prev[n]]);
    theta_away_prev[n] = exp(att[instants_prev[n],team2_prev[n]]+
                          def[instants_prev[n], team1_prev[n]]);

  y_prev[n,1] = poisson_rng(theta_home_prev[n]);
  y_prev[n,2] = poisson_rng(theta_away_prev[n]);
        }
}

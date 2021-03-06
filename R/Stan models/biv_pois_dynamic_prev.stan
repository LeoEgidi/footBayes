functions{

real bipois_lpmf(int[] r , real mu1,real mu2,real mu3) {
     real ss;
     real log_s;
     real mus;
     int  miny;

     miny = min(r[1], r[2]);

     ss = poisson_lpmf(r[1] | mu1) + poisson_lpmf(r[2] | mu2) -
            exp(mu3);
     if(miny > 0) {
       mus = -mu1-mu2+mu3;
       log_s = ss;

       for(k in 1:miny) {
            log_s = log_s + log(r[1] - k + 1) + mus
                           + log(r[2] - k + 1)
                           - log(k);
            ss = log_sum_exp(ss, log_s);
       }
     }
     return(ss);
   }
}
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
  real<lower=0> rho;
  real home;
  real<lower=0> sigma_att;
  real<lower=0> sigma_def;
}
transformed parameters{
  matrix[ntimes, nteams] att;            // attack abilities
  matrix[ntimes, nteams] def;            // defense abilities
  //cov_matrix[ntimes] Sigma_att;          // Gaussian process attack cov. funct.
  //cov_matrix[ntimes] Sigma_def;          // Gaussian process defense cov.funct.
  matrix[ntimes, nteams] mu_att;         // attack hyperparameter
  matrix[ntimes, nteams] mu_def;         // defense hyperparameter
  vector[N] theta_home;                    // exponentiated linear pred.
  vector[N] theta_away;
  vector[N] theta_corr;

    // Gaussian process covariance functions
   // for (i in 1:(ntimes)){
   //   for (j in 1:(ntimes)){
   //     Sigma_att[i, j] = exp(-pow(time[i] - time[j], 2))
   //     + (i == j ? 0.1 : 0.0);
   //     Sigma_def[i, j] = exp(-pow(time[i] - time[j], 2))
   //                 + (i == j ? 0.1 : 0.0);
   //   }}

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
     mu_att[t]=att[t-1];
     //rep_row_vector(0,nteams);

     mu_def[1]=rep_row_vector(0,nteams);
     mu_def[t]=def[t-1];
     //rep_row_vector(0,nteams);

     }


  for (n in 1:N){
    theta_home[n] = exp(home+att[instants[n], team1[n]]+def[instants[n], team2[n]]);
    theta_away[n] = exp(att[instants[n], team2[n]]+def[instants[n], team1[n]]);
    theta_corr[n] = rho;
   }
}
model{
  // priors
  for (h in 1:(nteams)){
     att_raw[,h]~multi_normal(mu_att[,h], diag_matrix(rep_vector(square(sigma_att), ntimes)));
     def_raw[,h]~multi_normal(mu_def[,h], diag_matrix(rep_vector(square(sigma_def), ntimes)));
   }
  target+=normal_lpdf(home|0,5);
  target+=normal_lpdf(rho|0,5);
  target+=cauchy_lpdf(sigma_att| 0,2.5);
  target+=cauchy_lpdf(sigma_def| 0,2.5);
  // likelihood

  for (n in 1:N){
    target+=bipois_lpmf(y[n,]| theta_home[n],
                        theta_away[n], theta_corr[n]);
    }
}
generated quantities{
  int y_rep[N,2];
  vector[N] log_lik;
  int diff_y_rep[N];
  int y_prev[N_prev,2];
  vector[N_prev] theta_home_prev;                    // exponentiated linear pred.
  vector[N_prev] theta_away_prev;
  vector[N_prev] theta_corr_prev;


  //in-sample replications
  for (n in 1:N){
    y_rep[n,1] = poisson_rng(theta_home[n]+theta_corr[n]);
    y_rep[n,2] = poisson_rng(theta_away[n]+theta_corr[n]);
    diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
    log_lik[n] =bipois_lpmf(y[n,]| theta_home[n],
                            theta_away[n], theta_corr[n]);
  }

  for (n in 1:N_prev){
    theta_home_prev[n] = exp(home+att[instants_prev[n], team1_prev[n]]+
                          def[instants_prev[n], team2_prev[n]]);
    theta_away_prev[n] = exp(att[instants_prev[n], team2_prev[n]]+
                          def[instants_prev[n], team1_prev[n]]);
    theta_corr_prev[n] = rho;
  y_prev[n,1] = poisson_rng(theta_home_prev[n]+theta_corr_prev[n]);
  y_prev[n,2] = poisson_rng(theta_away_prev[n]+theta_corr_prev[n]);
  }
}

functions{
  real skellam_lpmf(int k, real lambda1, real lambda2) {
    real r = k;
    return -(lambda1 + lambda2) + (r/2) * log(lambda1/lambda2) +
      log(modified_bessel_first_kind(k, 2 * sqrt(lambda1 * lambda2)));
  }
}
data{
  int N;
  int N_prev;
  int diff_y[N];
  int nteams;
  int team1[N];
  int team2[N];
  int team1_prev[N_prev];
  int team2_prev[N_prev];
}
parameters{
  vector[nteams] att_raw;
  vector[nteams] def_raw;
  real<lower=0> sigma_att;
  real<lower=0> sigma_def;
  real<lower=0> rho;
  real home;
}
transformed parameters{
  vector[nteams] att;
  vector[nteams] def;
  real theta[N,2];

  for (t in 1:nteams){
    att[t] = att_raw[t]-mean(att_raw);
    def[t] = def_raw[t]-mean(def_raw);
   }

  for (n in 1:N){
    theta[n,1] = exp(home+att[team1[n]]+def[team2[n]]);
    theta[n,2] = exp(att[team2[n]]+def[team1[n]]);
   }
}
model{
  // priors
  for (t in 1:(nteams)){
    target+=normal_lpdf(att_raw[t]|0, sigma_att);
    target+=normal_lpdf(def_raw[t]|0, sigma_def);
  }
  target+=cauchy_lpdf(sigma_att|0, 5);
  target+=cauchy_lpdf(sigma_def|0, 5);
  target+=normal_lpdf(rho|0,5);
  target+=normal_lpdf(home|0,5);
  // likelihood
  for (n in 1:N){
    target+=skellam_lpmf(diff_y[n]| theta[n,1],theta[n,2]);
    }
}
generated quantities{
  int y_rep[N,2];
  int diff_y_rep[N];
  vector[N] log_lik;
  int y_prev[N_prev,2];
  vector[2] theta_prev[N_prev];
  int diff_y_prev[N_prev];

  //in-sample replications
  for (n in 1:N){
    y_rep[n,1] = poisson_rng(theta[n,1]);
    y_rep[n,2] = poisson_rng(theta[n,2]);
    diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
    log_lik[n] =skellam_lpmf(diff_y[n]| theta[n,1], theta[n,2]);
  }
  //out-of-sample predictions
  for (n in 1:N_prev){
    theta_prev[n,1] = exp(home+att[team1_prev[n]]+
                          def[team2_prev[n]]);
    theta_prev[n,2] = exp(att[team2_prev[n]]+
                          def[team1_prev[n]]);
    y_prev[n,1] = poisson_rng(theta_prev[n,1]);
    y_prev[n,2] = poisson_rng(theta_prev[n,2]);
    diff_y_prev[n] = y_prev[n,1] - y_prev[n,2];
        }
}


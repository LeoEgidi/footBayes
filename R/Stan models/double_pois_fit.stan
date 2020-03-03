data{
  int N;                      // number of games
  int y[N,2];                 // scores
  int nteams;                 // number of teams
  int team1[N];               // home team index
  int team2[N];               // away team index
}
parameters{
  vector[nteams] att_raw;
  vector[nteams] def_raw;
  real<lower=0> sigma_att;
  real<lower=0> sigma_def;
  real home;
}
transformed parameters{
  vector[nteams] att;        // attack parameters
  vector[nteams] def;        // defence parameters
  vector[2] theta[N];        // exponentiated linear pred.

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
  target+=normal_lpdf(home|0,5);
  // likelihood
  for (n in 1:N){
    target+=poisson_lpmf(y[n,1]| theta[n,1]);
    target+=poisson_lpmf(y[n,2]| theta[n,2]);
  }
}
generated quantities{
  int y_rep[N,2];
  vector[N] log_lik;
  int diff_y_rep[N];

  //in-sample replications
  for (n in 1:N){
    y_rep[n,1] = poisson_rng(theta[n,1]);
    y_rep[n,2] = poisson_rng(theta[n,2]);
    diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
    log_lik[n] =poisson_lpmf(y[n,1]| theta[n,1])+
                poisson_lpmf(y[n,2]| theta[n,2]);
  }
}

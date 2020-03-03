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
  int y[N,2];
  int nteams;
  int team1[N];
  int team2[N];
}
parameters{
  vector[nteams] att_raw;
  vector[nteams] def_raw;
  real<lower=0> sigma_att;
  real<lower=0> sigma_def;
  real beta;
  real<lower=0> rho;
  real home;
}
transformed parameters{
  vector[nteams] att;
  vector[nteams] def;
  vector[3] theta[N];

  for (t in 1:nteams){
    att[t] = att_raw[t]-mean(att_raw);
    def[t] = def_raw[t]-mean(def_raw);
   }

  for (n in 1:N){
    theta[n,1] = exp(home+att[team1[n]]+def[team2[n]]);
    theta[n,2] = exp(att[team2[n]]+def[team1[n]]);
    theta[n,3] = rho;
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
    target+=bipois_lpmf(y[n,]| theta[n,1],
                        theta[n,2], theta[n,3]);
    }
}
generated quantities{
  int y_rep[N,2];
  vector[N] log_lik;
  int diff_y_rep[N];

  //in-sample replications
  for (n in 1:N){
    y_rep[n,1] = poisson_rng(theta[n,1]+theta[n,3]);
    y_rep[n,2] = poisson_rng(theta[n,2]+theta[n,3]);
    diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
    log_lik[n] =bipois_lpmf(y[n,]| theta[n,1],
                            theta[n,2], theta[n,3]);
  }
}

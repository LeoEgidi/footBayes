functions{

    real bipois_lpmf(array[] int r , real mu1,real mu2,real mu3) {
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
    real diag_infl_bipois_lpmf(array[] int r , real mu1,real mu2,real mu3, real p) {
    // This way is the easiest and proposed by https://github.com/Torvaney/karlis-ntzoufras-reproduction.
    // However, within model block, we propose in a comment the alternative way that Stan proposes in their documentation for zero inflated models
      real base_prob;
      real prob;
      real log_prob;

      base_prob = exp(bipois_lpmf(r| mu1, mu2,mu3));

      if (r[1] == r[2])
        prob = p + (1 - p) * base_prob;
      else
        prob = (1 - p) * base_prob;

      log_prob = log(prob);

      return log_prob;
    }

  }
data{
      int N;   // number of games
      int<lower=0> N_prev;
      array[N,2] int y;
      int nteams;
      array[N] int team1;
      array[N] int team2;
      array[N_prev] int team1_prev;
      array[N_prev] int team2_prev;
      array[N] int instants_rank;
      int ntimes_rank;                 // dynamic periods for ranking
      matrix[ntimes_rank,nteams] ranking;
      int<lower=0, upper=1> ind_home;
      real mean_home;              // Mean for home effect
      real<lower=0> sd_home;      // Standard deviation for home effect

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real hyper_df;
      real hyper_location;

      real hyper_sd_df;
      real hyper_sd_location;
      real hyper_sd_scale;
    }
    parameters{
      vector[nteams] att_raw;
      vector[nteams] def_raw;
      real<lower=0> sigma_att;
      real<lower=0> sigma_def;
      real home;
      real rho;
      real gamma;
      real <lower=0,upper=1> prob_of_draws;// excessive probability of draws

    }
    transformed parameters{
      real adj_h_eff;                   // Adjusted home effect
      vector[nteams] att;
      vector[nteams] def;
      array[N] vector[3] theta;

      for (t in 1:nteams){
        att[t] = att_raw[t]-mean(att_raw);
        def[t] = def_raw[t]-mean(def_raw);
      }

      adj_h_eff = home * ind_home;

      for (n in 1:N){
        theta[n,1] = exp(adj_h_eff+att[team1[n]]+def[team2[n]]+
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
        theta[n,2] = exp(att[team2[n]]+def[team1[n]]-
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
        theta[n,3] = exp(rho);
      }
}
model{
      // log-priors for team-specific abilities
      for (t in 1:(nteams)){
        if (prior_dist_num == 1){
          target+= normal_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= normal_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
        else if (prior_dist_num == 2){
          target+= student_t_lpdf(att_raw[t]|hyper_df, hyper_location, sigma_att);
          target+= student_t_lpdf(def_raw[t]|hyper_df, hyper_location, sigma_def);
        }
        else if (prior_dist_num == 3){
          target+= cauchy_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= cauchy_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
        else if (prior_dist_num == 4){
          target+= double_exponential_lpdf(att_raw[t]|hyper_location, sigma_att);
          target+= double_exponential_lpdf(def_raw[t]|hyper_location, sigma_def);
        }
      }


      // log-hyperpriors for sd parameters
      if (prior_dist_sd_num == 1 ){
        target+=normal_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=normal_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 2){
        target+=student_t_lpdf(sigma_att|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        target+=student_t_lpdf(sigma_def|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 3){
        target+=cauchy_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=cauchy_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }
      else if (prior_dist_sd_num == 4){
        target+=double_exponential_lpdf(sigma_att|hyper_sd_location, hyper_sd_scale);
        target+=double_exponential_lpdf(sigma_def|hyper_sd_location, hyper_sd_scale);
      }

      // log-priors fixed effects
      target+=normal_lpdf(home|mean_home,sd_home);
      target+=normal_lpdf(rho|0,1);
      target+=normal_lpdf(gamma|0,1);
      target+=uniform_lpdf(prob_of_draws|0,1);

      // likelihood

      for (n in 1:N){
         target+=diag_infl_bipois_lpmf(y[n,]| theta[n,1],
                   theta[n,2], theta[n,3],prob_of_draws);

   // if (y[n,1] == y[n,2]){// Alternative way as proposed by Stan manual
  //      target += log_sum_exp(bernoulli_lpmf(1 |prob_of_draws),
   //                         bernoulli_lpmf(0 | prob_of_draws)
     //                    + bipois_lpmf(y[n,] | theta[n,1],
                 //  theta[n,2], theta[n,3]) );
     // } else {
       //  target += bernoulli_lpmf(0 |prob_of_draws)
         //            + bipois_lpmf(y[n,] |theta[n,1],
                 //  theta[n,2], theta[n,3]);
    // }
 }
}

generated quantities{
      array[N,2] int y_rep;
      array[N_prev,2] int y_prev;
      vector[N] log_lik;
      array[N] int diff_y_rep;
      vector[N_prev] theta_home_prev;                    // exponentiated linear pred.
      vector[N_prev] theta_away_prev;
      vector[N_prev] theta_corr_prev;

      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta[n,1]+theta[n,3]);
        y_rep[n,2] = poisson_rng(theta[n,2]+theta[n,3]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] =diag_infl_bipois_lpmf(y[n,]| theta[n,1],
                   theta[n,2], theta[n,3],prob_of_draws);
           //    if (y[n,1] == y[n,2]){// Alternative way proposed by Stan documentation
    //  log_lik[n] = log_sum_exp(bernoulli_lpmf(1 |prob_of_draws),
          //                  bernoulli_lpmf(0 |prob_of_draws)
           //                   + bipois_lpmf(y[n,] | theta[n,1],
                 //  theta[n,2], theta[n,3]));
   //} else {
   //   log_lik[n] = bernoulli_lpmf(0 |prob_of_draws)
              //    + bipois_lpmf(y[n,] | theta[n,1],
                 //  theta[n,2], theta[n,3]);
  //}
}

    //out-of-sample predictions
      if (N_prev > 0) {
        for (n in 1:N_prev){
          theta_home_prev[n] = exp(adj_h_eff+att[team1_prev[n]]+
                                  def[team2_prev[n]]+
                           (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
          theta_away_prev[n] = exp(att[team2_prev[n]]+
                                  def[team1_prev[n]]-
                           (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
          theta_corr_prev[n] = exp(rho);
          y_prev[n,1] = poisson_rng(theta_home_prev[n]+theta_corr_prev[n]);
          y_prev[n,2] = poisson_rng(theta_away_prev[n]+theta_corr_prev[n]);
        }
      }
}

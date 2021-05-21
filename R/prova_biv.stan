
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

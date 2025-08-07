data{
      int N;   // number of games
      int<lower=0> N_prev;
      array[N,2] int y;
      int nteams;
      array[N] int team1;
      array[N] int team2;
      array[N_prev] int team1_prev;
      array[N_prev] int team2_prev;
      int ntimes;                 // dynamic periods
      array[ntimes] int time;
      array[N] int instants;
      array[N_prev] int instants_prev;
      array[N] int instants_rank;
      int ntimes_rank;                 // dynamic periods for ranking
      matrix[ntimes_rank,nteams] ranking;      // eventual ranking
      int<lower=0, upper=1> ind_home;
      int<lower=0, upper=1> ind_common_sigma;
      real mean_home;              // Mean for home effect
      real<lower=1e-8> sd_home;      // Standard deviation for home effect

      // priors part
      int<lower=1,upper=4> prior_dist_num;    // 1 gaussian, 2 t, 3 cauchy, 4 laplace
      int<lower=1,upper=4> prior_dist_sd_num; // 1 gaussian, 2 t, 3 cauchy, 4 laplace

      real<lower=0> hyper_df;
      real hyper_location;

      real<lower=0> hyper_sd_df;
      real hyper_sd_location;
      real<lower=1e-8> hyper_sd_scale;


      // commensurate prior
      int<lower=0, upper=1> ind_comm_prior;
      real mu_spike;
      real<lower=0> sd_spike;
      real mu_slab;
      real<lower=0> sd_slab;
      real<lower=0, upper=1> p_spike;
    }

    transformed data {
      real lognc_spike = normal_lccdf(0 | mu_spike, sd_spike); // \Phi(mu_spike / sd_spike)
      real lognc_slab  = normal_lccdf(0 | mu_slab, sd_slab); // \Phi(mu_slab / sd_slab)
    }

    parameters{
      matrix[ntimes, nteams] att_raw;        // Raw attack ability
      matrix[ntimes, nteams] def_raw;        // Raw defense ability
      real home;
      real gamma;
      array[(ind_comm_prior|| ind_common_sigma) ? 0 : 1] real<lower=1e-8> sigma_att; // Evolution variance Egidi (2018)
      array[(ind_comm_prior|| ind_common_sigma) ? 0 : 1] real<lower=1e-8> sigma_def; // Evolution variance Egidi (2018)
      array[(ind_comm_prior|| !ind_common_sigma) ? 0 : 1] real<lower=1e-8> sigma_common; // Evolution variance Owen (2011)
      vector<lower=0>[ind_comm_prior ? ntimes : 0] comm_prec_att;  // Commensurability parameter
      vector<lower=0>[ind_comm_prior ? ntimes : 0] comm_prec_def;  // Commensurability parameter
    }

    transformed parameters {
      real adj_h_eff;                   // Adjusted home effect
      matrix[ntimes, nteams] att;            // attack abilities
      matrix[ntimes, nteams] def;            // defense abilities
      // cov_matrix[ntimes] Sigma_att;          // Gaussian process attack cov. funct.
      // cov_matrix[ntimes] Sigma_def;          // Gaussian process defense cov.funct.
      matrix[ntimes, nteams] mu_att;         // attack hyperparameter
      matrix[ntimes, nteams] mu_def;         // defense hyperparameter
      vector[N] theta_home;                    // exponentiated linear pred.
      vector[N] theta_away;

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
        mu_att[1]=rep_row_vector(hyper_location,nteams);
        mu_att[t]= att[t-1];
        //rep_row_vector(0,nteams);

        mu_def[1]=rep_row_vector(hyper_location,nteams);
        mu_def[t]=def[t-1];
        //rep_row_vector(0,nteams);

      }

      adj_h_eff = home * ind_home;

      for (n in 1:N){
        theta_home[n] = exp(adj_h_eff+att[instants[n], team1[n]]+def[instants[n], team2[n]]+
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
        theta_away[n] = exp(att[instants[n], team2[n]]+def[instants[n], team1[n]]-
                         (gamma/2)*(ranking[instants_rank[n],team1[n]]-ranking[instants_rank[n],team2[n]]));
      }

      vector[ind_comm_prior ? ntimes : 0] comm_sd_att = inv_sqrt(comm_prec_att);   // SD for commensurate prior
      vector[ind_comm_prior ? ntimes : 0] comm_sd_def = inv_sqrt(comm_prec_def);   // SD for commensurate prior

    }



    model{
      if(ind_comm_prior == 1){
        for (i in 1:ntimes) {
          // Att
          target += log_mix(
            p_spike
            , normal_lpdf(comm_prec_att[i] | mu_spike, sd_spike) - lognc_spike
            , normal_lpdf(comm_prec_att[i] | mu_slab, sd_slab) - lognc_slab
          );
          // Def
          target += log_mix(
            p_spike
            , normal_lpdf(comm_prec_def[i] | mu_spike, sd_spike) - lognc_spike
            , normal_lpdf(comm_prec_def[i] | mu_slab, sd_slab) - lognc_slab
         );
        }

        // log-priors for team-specific abilities
        for (h in 1:(nteams)){
          for (i in 1:ntimes){
            if (prior_dist_num == 1 ){
              target+= normal_lpdf(att_raw[i,h]| mu_att[i,h], comm_sd_att[i]);
              target+= normal_lpdf(def_raw[i,h]| mu_def[i,h], comm_sd_def[i]);
            }
            else if (prior_dist_num == 2 ){
              target+= student_t_lpdf(att_raw[i,h]| hyper_df, mu_att[i,h], comm_sd_att[i]);
              target+= student_t_lpdf(def_raw[i,h]| hyper_df, mu_def[i,h], comm_sd_def[i]);
            }
            else if (prior_dist_num == 3 ){
              target+= student_t_lpdf(att_raw[i,h]| 1, mu_att[i,h], comm_sd_att[i]);
              target+= student_t_lpdf(def_raw[i,h]| 1, mu_def[i,h], comm_sd_def[i]);
            }
          }
        }

      }
      else{
        // First order autoregressive model Owen (2011)
        // log-priors for team-specific abilities
        for (h in 1:(nteams)){
          for (i in 1:ntimes){
            if (prior_dist_num == 1 ){
              if(ind_common_sigma == 0) {
              target+= normal_lpdf(att_raw[i,h]| mu_att[i,h], sigma_att[1]);
              target+= normal_lpdf(def_raw[i,h]| mu_def[i,h], sigma_def[1]);
              } else {
              target+= normal_lpdf(att_raw[i,h]| mu_att[i,h], sigma_common[1]);
              target+= normal_lpdf(def_raw[i,h]| mu_def[i,h], sigma_common[1]);
              }
            }
            else if (prior_dist_num == 2 ){
              if(ind_common_sigma == 0) {
              target+= student_t_lpdf(att_raw[i,h]| hyper_df, mu_att[i,h], sigma_att[1]);
              target+= student_t_lpdf(def_raw[i,h]| hyper_df, mu_def[i,h], sigma_def[1]);
              } else{
              target+= student_t_lpdf(att_raw[i,h]| hyper_df, mu_att[i,h], sigma_common[1]);
              target+= student_t_lpdf(def_raw[i,h]| hyper_df, mu_def[i,h], sigma_common[1]);
              }
            }
            else if (prior_dist_num == 3 ){
              if(ind_common_sigma == 0) {
              target+= student_t_lpdf(att_raw[i,h]| 1, mu_att[i,h], sigma_att[1]);
              target+= student_t_lpdf(def_raw[i,h]| 1, mu_def[i,h], sigma_def[1]);
              } else{
              target+= student_t_lpdf(att_raw[i,h]| 1, mu_att[i,h], sigma_common[1]);
              target+= student_t_lpdf(def_raw[i,h]| 1, mu_def[i,h], sigma_common[1]);
              }
            }
          }
        }
        // log-hyperpriors for sd parameters
          if (prior_dist_sd_num == 1 ){
            if(ind_common_sigma == 0) {
              target+=normal_lpdf(sigma_att[1]|hyper_sd_location, hyper_sd_scale);
              target+=normal_lpdf(sigma_def[1]|hyper_sd_location, hyper_sd_scale);
            } else {
                target+=normal_lpdf(sigma_common[1]|hyper_sd_location, hyper_sd_scale);
              }
          }
          else if (prior_dist_sd_num == 2){
            if(ind_common_sigma == 0) {
              target+=student_t_lpdf(sigma_att[1]|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
              target+=student_t_lpdf(sigma_def[1]|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
            } else {
                target+=student_t_lpdf(sigma_common[1]|hyper_sd_df, hyper_sd_location, hyper_sd_scale);
              }
          }
          else if (prior_dist_sd_num == 3){
            if(ind_common_sigma == 0) {
              target+=cauchy_lpdf(sigma_att[1]|hyper_sd_location, hyper_sd_scale);
              target+=cauchy_lpdf(sigma_def[1]|hyper_sd_location, hyper_sd_scale);
            } else {
              target+=cauchy_lpdf(sigma_common[1]|hyper_sd_location, hyper_sd_scale);
              }
          }
          else if (prior_dist_sd_num == 4){
            if(ind_common_sigma == 0) {
              target+=double_exponential_lpdf(sigma_att[1]|hyper_sd_location, hyper_sd_scale);
              target+=double_exponential_lpdf(sigma_def[1]|hyper_sd_location, hyper_sd_scale);
            }
            else {
              target+=double_exponential_lpdf(sigma_common[1]|hyper_sd_location, hyper_sd_scale);
            }
          }
      }
      // log-priors fixed effects
      target+=normal_lpdf(home|mean_home,sd_home);
      target+=normal_lpdf(gamma|0,1);


      // likelihood
      target+=poisson_lpmf(y[,1]| theta_home);
      target+=poisson_lpmf(y[,2]| theta_away);

    }

    generated quantities {
      array[N,2] int y_rep;
      vector[N] log_lik;
      array[N] int diff_y_rep;
      array[N_prev,2] int y_prev;
      vector[N_prev] theta_home_prev;                    // exponentiated linear pred.
      vector[N_prev] theta_away_prev;


      //in-sample replications
      for (n in 1:N){
        y_rep[n,1] = poisson_rng(theta_home[n]);
        y_rep[n,2] = poisson_rng(theta_away[n]);
        diff_y_rep[n] = y_rep[n,1] - y_rep[n,2];
        log_lik[n] = poisson_lpmf(y[n,1]| theta_home[n])+
          poisson_lpmf(y[n,2]| theta_away[n]);
      }
    //out-of-sample predictions
      if (N_prev > 0) {
        for (n in 1:N_prev){
          theta_home_prev[n] = exp(adj_h_eff+att[instants_prev[n],team1_prev[n]]+
                                     def[instants_prev[n], team2_prev[n]]+
                           (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));
          theta_away_prev[n] = exp(att[instants_prev[n],team2_prev[n]]+
                                     def[instants_prev[n], team1_prev[n]]-
                           (gamma/2)*(ranking[instants_rank[N],team1_prev[n]]-ranking[instants_rank[N],team2_prev[n]]));

          y_prev[n,1] = poisson_rng(theta_home_prev[n]);
          y_prev[n,2] = poisson_rng(theta_away_prev[n]);
        }
      }
    }

data {
    int N;                       // number of matches
    int nteams;                  // number of teams
    int ntimes_rank;             // number of dynamic periods for rankings
    matrix[ntimes_rank, nteams] ranking; // rankings over time
    array[N] int instants_rank;        // time indices for rankings

    array[N] int team1;                // team 1 indices
    array[N] int team2;                // team 2 indices
    matrix[N, 2] y;              // scores: column 1 is team1, column 2 is team2
    real nu;                     // degrees of freedom for the Student's t-distribution
    int<lower=0, upper=1> ind_home;
    real mean_home;              // Mean for home effect
    real<lower=0> sd_home;      // Standard deviation for home effect


    // Priors part
    int<lower=1,upper=4> prior_dist_num;    // 1: Gaussian, 2: t, 3: Cauchy, 4: Laplace
    int<lower=1,upper=4> prior_dist_sd_num; // 1: Gaussian, 2: t, 3: Cauchy, 4: Laplace

    real hyper_df;
    real hyper_location;

    real hyper_sd_df;
    real hyper_sd_location;
    real hyper_sd_scale;
}
transformed data {
    vector[N] diff_y = y[,1] - y[,2];  // modeled data
}
parameters {
    real beta;                       // common coefficient for ranking
    vector[nteams] alpha;            // vector of per-team random effects
    real<lower=0> sigma_a;           // standard deviation for random effects
    real<lower=0> sigma_y;           // noise term
    real<lower=0> sigma_alpha;       // standard deviation for alpha prior
    real home;                      // home effect
}
transformed parameters {
    // Mixed effects model - common coefficient * dynamic ranking + random effects
    matrix[ntimes_rank, nteams] ability;
    real adj_h_eff;                   // Adjusted home effect

    adj_h_eff = home * ind_home;

    for (t in 1:ntimes_rank) {
        ability[t] = beta * ranking[t] + (alpha * sigma_a)';
    }
}
model {
    // Priors for team-specific abilities (alpha)
    if (prior_dist_num == 1) {
        alpha ~ normal(hyper_location, sigma_alpha);
    } else if (prior_dist_num == 2) {
        alpha ~ student_t(hyper_df, hyper_location, sigma_alpha);
    } else if (prior_dist_num == 3) {
        alpha ~ cauchy(hyper_location, sigma_alpha);
    } else if (prior_dist_num == 4) {
        alpha ~ double_exponential(hyper_location, sigma_alpha);
    }

    // Priors for standard deviations
    if (prior_dist_sd_num == 1) {
        sigma_a ~ normal(hyper_sd_location, hyper_sd_scale);
        sigma_alpha ~ normal(hyper_sd_location, hyper_sd_scale);
    } else if (prior_dist_sd_num == 2) {
        sigma_a ~ student_t(hyper_sd_df, hyper_sd_location, hyper_sd_scale);
        sigma_alpha ~ student_t(hyper_sd_df, hyper_sd_location, hyper_sd_scale);
    } else if (prior_dist_sd_num == 3) {
        sigma_a ~ cauchy(hyper_sd_location, hyper_sd_scale);
        sigma_alpha ~ cauchy(hyper_sd_location, hyper_sd_scale);
    } else if (prior_dist_sd_num == 4) {
        sigma_a ~ double_exponential(hyper_sd_location, hyper_sd_scale);
        sigma_alpha ~ double_exponential(hyper_sd_location, hyper_sd_scale);
    }

    beta ~ normal(0, 2.5);
    sigma_y ~ normal(0, 2.5);
    target+=normal_lpdf(home|mean_home,sd_home);

    // Likelihood
    for (n in 1:N) {
        int rank_time = instants_rank[n];
        diff_y[n] ~ student_t(nu, adj_h_eff+ability[rank_time, team1[n]] - ability[rank_time, team2[n]], sigma_y);
    }
}
generated quantities {
    // Posterior predictive checks
    vector[N] diff_y_rep;
    vector[N] log_lik;
    for (n in 1:N) {
        int rank_time = instants_rank[n];
        diff_y_rep[n] = student_t_rng(nu, adj_h_eff+ability[rank_time, team1[n]] - ability[rank_time, team2[n]], sigma_y);
        log_lik[n] = student_t_lpdf(diff_y[n] | nu, adj_h_eff+ability[rank_time, team1[n]] - ability[rank_time, team2[n]], sigma_y);
    }
}

data {
      int<lower=1> N;          // Number of observations
      int<lower=1> nteams;          // Number of teams
      int<lower=1> ntimes_rank;      // Number of time points
      int<lower=1, upper=ntimes_rank> instants_rank[N];  // Time point of each observation
      int<lower=1, upper=nteams> team1[N];  // Index of team1 in each observation
      int<lower=1, upper=nteams> team2[N];  // Index of team2 in each observation
      real mean_psi;                // Initial mean for psi
      real<lower=0> sd_psi;         // Standard deviation of the AR(1) process
      real mean_gamma;
      real<lower=0> sd_gamma;
      int<lower=1, upper=3> y[N];      // Outcome: 1 if team1 beats team2, 3 if team2 beats team1, 2 for tie
      int<lower=0, upper=1> ind_home;        // Home effect indicator
      real mean_home;              // Mean for home effect
      real<lower=0> sd_home;      // Standard deviation for home effect
  }

  parameters {
      matrix[nteams, ntimes_rank] psi;     // Log strength parameters for each team over time
      real gamma;               // Log tie parameter
      real home_effect;                  // Home team effect parameter
  }

  transformed parameters {
      real adj_home_eff;
      adj_home_eff = home_effect * ind_home;
  }

  model {
      // Priors for initial strengths
      for (k in 1:nteams) {
          psi[k, 1] ~ normal(mean_psi, sd_psi);
      }

      // Prior for tie parameter
      gamma ~ normal(mean_gamma, sd_gamma);

      // AR(1) process for strength parameters
      for (t_idx in 2:ntimes_rank) {
          for (k in 1:nteams) {
              psi[k, t_idx] ~ normal(psi[k, t_idx - 1], sd_psi);
          }
      }

      // Prior for the home effect
      home_effect ~ normal(mean_home, sd_home);

      // Likelihood
      for (n in 1:N) {
          real delta_team1 = exp(psi[team1[n], instants_rank[n]] + adj_home_eff);
          real delta_team2 = exp(psi[team2[n], instants_rank[n]]);
          real nu = exp(gamma);
          real denom = delta_team1 + delta_team2 + (nu * sqrt(delta_team1 * delta_team2));
          real p_i_win = delta_team1 / denom;
          real p_j_win = delta_team2 / denom;
          real p_tie = (nu * sqrt(delta_team1 * delta_team2)) / denom;
          if (y[n] == 1) {
              target += log(p_i_win);
          } else if (y[n] == 3) {
              target += log(p_j_win);
          } else if (y[n] == 2) {
              target += log(p_tie);
          }
      }
  }

  generated quantities {
    // Log-likelihood vector
    vector[N] log_lik;

    // Posterior predictive vector
    int y_rep[N];

    for (n in 1:N) {
        // Delta values
        real delta_team1 = exp(psi[team1[n], instants_rank[n]] + adj_home_eff);
        real delta_team2 = exp(psi[team2[n], instants_rank[n]]);
        real nu = exp(gamma);
        real denom = delta_team1 + delta_team2 + (nu * sqrt(delta_team1 * delta_team2));

        // Probabilities
        real p_i_win = delta_team1 / denom;
        real p_j_win = delta_team2 / denom;
        real p_tie = (nu * sqrt(delta_team1 * delta_team2)) / denom;

        // Log-likelihood
        if (y[n] == 1) {
            log_lik[n] = log(p_i_win);
        } else if (y[n] == 3) {
            log_lik[n] = log(p_j_win);
        } else if (y[n] == 2) {
            log_lik[n] = log(p_tie);
        }

        // Posterior predictive samples
        vector[3] probs;
        probs[1] = p_i_win;
        probs[2] = p_tie;
        probs[3] = p_j_win;

        y_rep[n] = categorical_rng(probs);
    }
}

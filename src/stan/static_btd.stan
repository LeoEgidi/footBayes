data {
      int<lower=1> N;          // Number of observations
      int<lower=1> nteams;          // Number of teams
      array[N] int<lower=1, upper=nteams> team1;  // Index of team1 in each observation
      array[N] int<lower=1, upper=nteams> team2;  // Index of team2 in each observation
      real mean_logStrength;                // Initial mean for logStrength
      real<lower=1e-8> sd_logStrength;         // Standard deviation for logStrength
      real mean_logTie;
      real<lower=1e-8> sd_logTie;
      array[N] int<lower=1, upper=3> y;      // Outcome: 1 if team1 beats team2, 3 if team2 beats team1, 2 for tie
      int<lower=0, upper=1> ind_home;        // Home effect indicator
      real mean_home;              // Mean for home effect
      real<lower=1e-8> sd_home;      // Standard deviation for home effect
    }
    parameters {
      vector[nteams] logStrength_raw;          // Log strength parameters for each team (static)
      real logTie;             // Log tie parameter
      real home;                  // Home team effect parameter
    }

    transformed parameters {
      real adj_h_eff;
      vector[nteams] logStrength;

      adj_h_eff = home * ind_home;

      // Sum-to-zero constraint for log-strength parameters
      logStrength = logStrength_raw - mean(logStrength_raw);
    }

    model {
      // Priors for strengths
      logStrength_raw ~ normal(mean_logStrength, sd_logStrength);

      // Prior for tie parameter
      logTie ~ normal(mean_logTie, sd_logTie);

      // Prior for the home effect

      home ~ normal(mean_home, sd_home);

      // Likelihood
      for (n in 1:N) {
        real delta_team1 = exp(logStrength[team1[n]] + adj_h_eff);
        real delta_team2 = exp(logStrength[team2[n]]);
        real nu = exp(logTie);
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
    array[N] int y_rep;

    for (n in 1:N) {
        // Delta values
        real delta_team1 = exp(logStrength[team1[n]] + adj_h_eff);
        real delta_team2 = exp(logStrength[team2[n]]);
        real nu = exp(logTie);
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

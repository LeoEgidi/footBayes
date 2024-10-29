#' Bayesian Bradley-Terry-Davidson Model
#'
#' This function fits a Bayesian Bradley-Terry-Davidson model using Stan. It supports both static and dynamic ranking models, allowing for the estimation of team strengths over time. Teams are specified by their names (character strings), and the function internally maps these names to integer indices required by the Stan model.
#'
#' @param data A data frame containing the observations with columns:
#'   \itemize{
#'     \item \code{periods}: Time point of each observation (integer >= 1).
#'     \item \code{team1}: Name of team 1 in each observation (character string).
#'     \item \code{team2}: Name of team 2 in each observation (character string).
#'     \item \code{match_outcome}: Outcome (\code{1} if team1 beats team2, \code{2} for tie, and \code{3} if team2 beats team1).
#'   }
#'   The data frame must not contain missing values.
#' @param dynamic_rank Logical; if \code{TRUE}, uses a dynamic ranking model (default is \code{FALSE}).
#' @param priors A list containing prior parameters with elements:
#'   \itemize{
#'     \item \code{mean_psi}: Initial mean for psi (numeric, default is 0).
#'     \item \code{std_psi}: Standard deviation for psi or the AR(1) process (positive numeric, default is 3).
#'     \item \code{mean_gamma}: Mean for gamma (numeric, default is 0).
#'     \item \code{std_gamma}: Standard deviation for gamma (positive numeric, default is 0.3).
#'   }
#' @param rank_measure A character string specifying the method used to summarize the posterior distributions of the team strengths (\code{psi}). Options are:
#'   \itemize{
#'     \item \code{"median"}: Uses the median of the posterior samples (default).
#'     \item \code{"mean"}: Uses the mean of the posterior samples.
#'     \item \code{"MAP"}: Uses the Maximum A Posteriori estimate, calculated as the mode of the posterior distribution.
#'   }
#' @param ... Additional arguments passed to \code{\link[rstan]{stan}} (e.g., \code{iter}, \code{chains}, \code{control}).
#'
#' @return A list of class \code{"btdFoot"} containing:
#'   \itemize{
#'     \item \code{fit}: The fitted \code{stanfit} object returned by \code{\link[rstan]{stan}}.
#'     \item \code{rank}: A data frame with the rankings, including columns:
#'       \itemize{
#'         \item \code{periods}: The time period (for dynamic models).
#'         \item \code{team}: The team name.
#'         \item \code{rank_points}: The estimated strength of the team based on the chosen \code{rank_measure}.
#'       }
#'     \item \code{data}: The original input data.
#'     \item \code{stan_data}: The data prepared for Stan.
#'     \item \code{stan_code}: The Stan code used for the model.
#'     \item \code{priors}: A list of the prior values used.
#'     \item \code{rank_measure}: The method used to compute the rankings.
#'   }
#'
#' @examples
#' \dontrun{
#'
#'#   Dynamic Ranking example                                               ####
#'
#'data <- data.frame(
#'  periods = c(1, 1, 2, 2, 3, 3, 4, 4),
#'  team1 = c("AC Milan", "Inter", "AC Milan", "Juventus", "Roma", "Inter", "Lecce", "Roma"),
#'  team2 = c("Inter", "Juventus", "Roma", "Roma", "Juventus", "AC Milan", "Juventus", "Lecce"),
#'  match_outcome = c(1, 3, 2, 3, 2, 3, 1, 2) # 1 = team1 wins, 2 = draw, 3 = team2 wins
#')
#'
#'# Fit the dynamic model using the median as rank measure
#'fit_result <- btd_foot(
#'  data,
#'  dynamic_rank = TRUE,
#'  priors = list(
#'    mean_psi = 0,
#'    std_psi = 1,
#'    mean_gamma = 0,
#'    std_gamma = 1
#'  ),
#'  rank_measure = "median",
#'  iter = 1000,
#'  chains = 2
#')
#'
#'print(fit_result$rank)
#'
#' #   Static Ranking example                                               ####
#'
#' data <- data.frame(
#'   periods = rep(1,6),
#'   team1 = c("AC Milan", "Roma", "Juventus", "Inter", "Roma", "AC Milan"),
#'   team2 = c("Juventus", "Inter", "AC Milan", "Roma", "AC Milan", "Juventus"),
#'   match_outcome = c(1, 2, 3, 1, 2, 1) # 1 = team1 wins, 2 = draw, 3 = team2 wins
#' )
#'
#'# Fit the static model using the MAP as rank measure
#' fit_result_static <- btd_foot(
#'   data,
#'   dynamic_rank = FALSE,
#'   priors = list(
#'     mean_psi = 0,
#'     std_psi = 1,
#'     mean_gamma = 0,
#'     std_gamma = 1
#'   ),
#'   rank_measure = "MAP",
#'   iter = 1000,
#'   chains = 2
#' )
#'
#' print(fit_result_static$rank)
#' }
#' @import rstan
#' @import dplyr
#' @export

btd_foot <- function(data,
                     dynamic_rank = FALSE,
                     priors = list(),
                     rank_measure = c("median", "mean", "MAP"),
                     ...) {

  # Validate rank_measure
  rank_measure <- match.arg(rank_measure)

  # Default values for priors if not provided
  default_mean_psi <- 0
  default_std_psi <- 3
  default_mean_gamma <- 0
  default_std_gamma <- 0.3

  # Extract prior parameters from the priors list or assign defaults
  mean_psi <- if (is.null(priors$mean_psi)) default_mean_psi else priors$mean_psi
  std_psi <- if (is.null(priors$std_psi)) default_std_psi else priors$std_psi
  mean_gamma <- if (is.null(priors$mean_gamma)) default_mean_gamma else priors$mean_gamma
  std_gamma <- if (is.null(priors$std_gamma)) default_std_gamma else priors$std_gamma

  # Check that data is a data frame
  if (!is.data.frame(data)) {
    stop("Data must be a data frame.")
  }

  # Check that required columns are present
  required_cols <- c("periods", "team1", "team2", "match_outcome")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Data is missing required columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  # Extract variables
  instants_rank <- data$periods
  team1 <- data$team1
  team2 <- data$team2
  match_outcome <- data$match_outcome

  N <- nrow(data)

  # Create teams vector and map team names to integer indices
  teams <- unique(c(team1, team2))
  nteams <- length(teams)
  ntimes_rank <- max(instants_rank)

  # Map team names to integer indices
  team1_idx <- match(team1, teams)
  team2_idx <- match(team2, teams)

  # Check for NAs in team1_idx and team2_idx
  if (any(is.na(team1_idx))) {
    stop("Some values in 'team1' do not match any known teams.")
  }
  if (any(is.na(team2_idx))) {
    stop("Some values in 'team2' do not match any known teams.")
  }

  # NAs
  if (any(is.na(data))) {
    stop("Data contains missing values. Please handle them before fitting the model.")
  }

  # Check periods
  if (!is.numeric(instants_rank) || any(instants_rank != as.integer(instants_rank))) {
    stop("Column 'periods' must contain integer values.")
  }
  if (any(instants_rank < 1 | instants_rank > ntimes_rank)) {
    stop(paste("Values in 'periods' must be between 1 and", ntimes_rank, "."))
  }

  # Check match_outcome
  if (!is.numeric(match_outcome) || any(match_outcome != as.integer(match_outcome))) {
    stop("Column 'match_outcome' must contain integer values.")
  }
  if (any(!match_outcome %in% c(1, 2, 3))) {
    stop("Values in 'match_outcome' must be 1, 2, or 3.")
  }

  # Check priors
  if (!is.numeric(mean_psi) || length(mean_psi) != 1) {
    stop("'mean_psi' must be a numeric value.")
  }
  if (!is.numeric(std_psi) || length(std_psi) != 1 || std_psi <= 0) {
    stop("'std_psi' must be a positive numeric value.")
  }
  if (!is.numeric(mean_gamma) || length(mean_gamma) != 1) {
    stop("'mean_gamma' must be a numeric value.")
  }
  if (!is.numeric(std_gamma) || length(std_gamma) != 1 || std_gamma <= 0) {
    stop("'std_gamma' must be a positive numeric value.")
  }

  # Prepare data for Stan based on dynamic_rank
  if (!dynamic_rank) {
    stan_data <- list(
      N = N,
      nteams = nteams,
      team1 = as.integer(team1_idx),
      team2 = as.integer(team2_idx),
      mu_psi_init = mean_psi,
      sigma_psi = std_psi,
      mu_gamma = mean_gamma,
      sigma_gamma = std_gamma,
      y = as.integer(match_outcome)
    )
  } else {
    stan_data <- list(
      N = N,
      nteams = nteams,
      ntimes_rank = ntimes_rank,
      t = as.integer(instants_rank),
      team1 = as.integer(team1_idx),
      team2 = as.integer(team2_idx),
      mu_psi_init = mean_psi,
      sigma_psi = std_psi,
      mu_gamma = mean_gamma,
      sigma_gamma = std_gamma,
      y = as.integer(match_outcome)
    )
  }

  # Define Stan model codes
  statBTD <- "
  data {
      int<lower=1> N;          // Number of observations
      int<lower=1> nteams;          // Number of teams
      int<lower=1, upper=nteams> team1[N];  // Index of team1 in each observation
      int<lower=1, upper=nteams> team2[N];  // Index of team2 in each observation
      real mu_psi_init;                // Initial mean for psi
      real<lower=0> sigma_psi;         // Standard deviation for psi
      real mu_gamma;
      real<lower=0> sigma_gamma;
      int<lower=1, upper=3> y[N];      // Outcome: 1 if team1 beats team2, 3 if team2 beats team1, 2 for tie
    }
    parameters {
      vector[nteams] psi;          // Log strength parameters for each team (static)
      real gamma;             // Log tie parameter
    }
    model {
      // Priors for strengths
      psi ~ normal(mu_psi_init, sigma_psi);

      // Prior for tie parameter
      gamma ~ normal(mu_gamma, sigma_gamma);

      // Likelihood
      for (n in 1:N) {
        real delta_team1 = exp(psi[team1[n]]);
        real delta_team2 = exp(psi[team2[n]]);
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
  "

  dynBTD <- "
  data {
      int<lower=1> N;          // Number of observations
      int<lower=1> nteams;          // Number of teams
      int<lower=1> ntimes_rank;      // Number of time points
      int<lower=1, upper=ntimes_rank> t[N];  // Time point of each observation
      int<lower=1, upper=nteams> team1[N];  // Index of team1 in each observation
      int<lower=1, upper=nteams> team2[N];  // Index of team2 in each observation
      real mu_psi_init;                // Initial mean for psi
      real<lower=0> sigma_psi;         // Standard deviation of the AR(1) process
      real mu_gamma;
      real<lower=0> sigma_gamma;
      int<lower=1, upper=3> y[N];      // Outcome: 1 if team1 beats team2, 3 if team2 beats team1, 2 for tie
  }
  parameters {
      matrix[nteams, ntimes_rank] psi;     // Log strength parameters for each team over time
      real gamma;               // Log tie parameter
  }
  model {
      // Priors for initial strengths
      for (k in 1:nteams) {
          psi[k, 1] ~ normal(mu_psi_init, sigma_psi);
      }

      // Prior for tie parameter
      gamma ~ normal(mu_gamma, sigma_gamma);

      // AR(1) process for strength parameters
      for (t_idx in 2:ntimes_rank) {
          for (k in 1:nteams) {
              psi[k, t_idx] ~ normal(psi[k, t_idx - 1], sigma_psi);
          }
      }
      // Likelihood
      for (n in 1:N) {
          real delta_team1 = exp(psi[team1[n], t[n]]);
          real delta_team2 = exp(psi[team2[n], t[n]]);
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
  "

  # Select the appropriate Stan code based on dynamic_rank
  if (!dynamic_rank) {
    stan_code <- statBTD
  } else {
    stan_code <- dynBTD
  }

  # Fit the model using Stan
  fit <- rstan::stan(model_code = stan_code, data = stan_data, ...)

  # Extract parameters
  BTDparameters <- rstan::extract(fit)

  results_df <- data.frame()

  # Function to compute MAP estimate from samples
  compute_MAP <- function(samples) {
    dens <- stats::density(samples)
    MAP <- dens$x[which.max(dens$y)]
    return(MAP)
  }

  if (!dynamic_rank) {
    # Static model
    for (team in teams) {
      team_index <- which(teams == team)

      # Get the samples for this team
      psi_samples <- BTDparameters[["psi"]][, team_index]

      # Compute the summary statistic based on rank_measure
      rank_point <- switch(rank_measure,
                           median = median(psi_samples),
                           mean = mean(psi_samples),
                           MAP = compute_MAP(psi_samples))

      df <- data.frame(
        periods = 1,  # Set periods to 1 for static model
        team = team,  # Use team name
        rank_points = rank_point,
        stringsAsFactors = FALSE
      )

      results_df <- rbind(results_df, df)
    }
    BTDrank <- results_df
  } else {
    # Dynamic model
    for (team in teams) {
      team_index <- which(teams == team)

      # Compute the summary statistic for each date
      rank_point <- sapply(1:ntimes_rank, function(k) {
        psi_samples <- BTDparameters[["psi"]][, team_index, k]
        switch(rank_measure,
               median = median(psi_samples),
               mean = mean(psi_samples),
               MAP = compute_MAP(psi_samples))
      })

      df <- data.frame(
        periods = 1:ntimes_rank,
        team = team,  # Use team name
        rank_points = rank_point,
        stringsAsFactors = FALSE
      )

      results_df <- rbind(results_df, df)
    }
    BTDrank <- results_df[, c("periods", "team", "rank_points")]
  }

  # Output
  output <- list(
    fit = fit,
    rank = BTDrank,
    data = data,
    stan_data = stan_data,
    stan_code = stan_code,
    priors = list(
      mean_psi = mean_psi,
      std_psi = std_psi,
      mean_gamma = mean_gamma,
      std_gamma = std_gamma
    ),
    rank_measure = rank_measure)
  class(output) <- "btdFoot"
  return(output)
}

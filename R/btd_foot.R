#' Bayesian Bradley-Terry-Davidson Model
#'
#' Fits a Bayesian Bradley-Terry-Davidson model using Stan. Supports both static and dynamic ranking models, allowing for the estimation of team strengths over time.
#'
#' @param data A data frame containing the observations with columns:
#'   \itemize{
#'     \item \code{periods}: Time point of each observation (integer >= 1).
#'     \item \code{home_team}: Home team's name (character string).
#'     \item \code{away_team}: Away team's name (character string).
#'     \item \code{match_outcome}: Outcome (1 if home team beats away team, 2 for tie, and 3 if away team beats home team).
#'   }
#'   The data frame must not contain missing values.
#' @param dynamic_rank Logical; if \code{TRUE}, uses a dynamic ranking model (default is \code{FALSE}).
#' @param home_effect Logical; if \code{TRUE}, includes a home effect in the model (default is \code{FALSE}).
#' @param prior_par A list specifying the prior distributions for the parameters of interest, using the \code{normal} function:
#'   \itemize{
#'     \item \code{psi}: Prior for the team strengths (\code{psi}). Default is \code{normal(0, 3)}.
#'     \item \code{gamma}: Prior for the tie parameter (\code{gamma}). Default is \code{normal(0, 0.3)}.
#'     \item \code{home}: Prior for the home effect (\code{home}). Applicable only if \code{home_effect = TRUE}. Default is \code{normal(0, 5)}.
#'   }
#'   Only normal priors are allowed for this model.
#' @param rank_measure A character string specifying the method used to summarize the posterior distributions of the team strengths. Options are:
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
#'         \item \code{periods}: The time period.
#'         \item \code{team}: The team name.
#'         \item \code{rank_points}: The estimated strength of the team based on the chosen \code{rank_measure}.
#'       }
#'     \item \code{data}: The original input data.
#'     \item \code{stan_data}: The data list prepared for Stan.
#'     \item \code{stan_code}: The path to the Stan model code used.
#'     \item \code{prior_par}: A list of the prior distributions used.
#'     \item \code{rank_measure}: The method used to compute the rankings.
#'   }
#'
#' @examples
#' \dontrun{
#'
#' # Dynamic Ranking example ####
#'
#' data <- data.frame(
#'   periods = c(1, 1, 2, 2, 3, 3, 4, 4),
#'   home_team = c("AC Milan", "Inter", "AC Milan", "Juventus", "Roma", "Inter", "Lecce", "Roma"),
#'   away_team = c("Inter", "Juventus", "Roma", "Roma", "Juventus", "AC Milan", "Juventus", "Lecce"),
#'   match_outcome = c(1, 3, 2, 3, 2, 3, 1, 2) # 1 = home team wins, 2 = draw, 3 = away team wins
#' )
#'
#' # Fit the dynamic model using the median as rank measure
#' fit_result <- btd_foot(
#'   data = data,
#'   dynamic_rank = TRUE,
#'   prior_par = list(
#'     psi = normal(0, 10),
#'     gamma = normal(0, 5)
#'   ),
#'   rank_measure = "median",
#'   iter = 1000,
#'   chains = 2
#' )
#'
#' print(fit_result$rank)
#'
#' # Static Ranking example ####
#'
#' data_static <- data.frame(
#'   periods = rep(1, 6),
#'   home_team = c("AC Milan", "Roma", "Juventus", "Inter", "Roma", "AC Milan"),
#'   away_team = c("Juventus", "Inter", "AC Milan", "Roma", "AC Milan", "Juventus"),
#'   match_outcome = c(1, 2, 3, 1, 2, 1) # 1 = home team wins, 2 = draw, 3 = away team wins
#' )
#'
#' # Fit the static model using the MAP as rank measure
#' fit_result_static <- btd_foot(
#'   data = data_static,
#'   dynamic_rank = FALSE,
#'   home_effect = TRUE,
#'   prior_par = list(
#'     psi = normal(0, 1),
#'     gamma = normal(0, 1),
#'     home = normal(0, 2)
#'   ),
#'   rank_measure = "MAP",
#'   iter = 1000,
#'   chains = 2
#' )
#'
#' print(fit_result_static$rank)
#' }
#' @importFrom rstan stan extract
#' @importFrom dplyr filter count
#' @export


btd_foot <- function(data,
                     dynamic_rank = FALSE,
                     home_effect = FALSE,
                     prior_par = list(
                        psi = normal(0, 3),
                        gamma = normal(0, 0.3),
                        home = normal(0, 5)
                     ),
                     rank_measure = "median",
                     ...) {


  # Validate prior names
  allowed_prior_names <- c("psi", "gamma", "home")

  # Check that prior_par contains only allowed elements
  if (!is.null(prior_par)) {
    if (!is.list(prior_par)) {
      stop("'prior_par' must be a list.")
    }
    unknown_prior_names <- setdiff(names(prior_par), allowed_prior_names)
    if (length(unknown_prior_names) > 0) {
      stop(
        paste(
          "Unknown elements in 'prior_par':",
          paste(unknown_prior_names, collapse = ", ")
        )
      )
    }
  }


  # Validate rank_measure
  allowed_rank_measures <- c("median", "mean", "MAP")

  rank_measure <- match.arg(rank_measure, allowed_rank_measures)

  # Extract prior parameters from the priors list
  psi_prior <- prior_par$psi
  gamma_prior <- prior_par$gamma
  home_prior <- prior_par$home

  # Only normal prior

  if (psi_prior$dist != "normal" || gamma_prior$dist != "normal" ||
      (home_effect && home_prior$dist != "normal")) {
    stop("Prior distributions must be 'normal'.")
  }


  mean_psi <- psi_prior$location
  sd_psi <- psi_prior$scale
  mean_gamma <- gamma_prior$location
  sd_gamma <- gamma_prior$scale

#   ____________________________________________________________________________
#   Home Effect Check                                                       ####


  # Check that home_effect is logical
  if (!is.logical(home_effect) || length(home_effect) != 1) {
    stop("'home_effect' must be a single logical value (TRUE or FALSE).")
  }


  if (home_effect) {
    ind_home <- 1
    mean_home <- home_prior$location
    sd_home <- home_prior$scale
  } else {
    ind_home <- 0
    mean_home <- 0
    sd_home <- 1  # Default value (can be adjusted)
  }


  # # Check prior_par' value
  #
  # check_prior(mean_psi, "mean_psi")
  # check_prior(sd_psi, "sd_psi", positive = TRUE)
  # check_prior(mean_gamma, "mean_gamma")
  # check_prior(sd_gamma, "sd_gamma", positive = TRUE)
  #
  # if (home_effect) {
  #   check_prior(mean_home, "mean_home")
  #   check_prior(sd_home, "sd_home", positive = TRUE)
  # }


  # Check that data is a data frame
  if (!is.data.frame(data)) {
    stop("Data must be a data frame.")
  }

  # Check that required columns are present
  required_cols <- c("periods", "home", "away_team", "match_outcome")
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
  home_team <- data$home_team
  away_team <- data$away_team
  match_outcome <- data$match_outcome

  N <- nrow(data)

  # Create teams vector and map team names to integer indices
  teams <- unique(c(home_team, away_team))
  nteams <- length(teams)
  ntimes_rank <- max(instants_rank)

  # Map team names to integer indices
  home_team_idx <- match(home_team, teams)
  away_team_idx <- match(away_team, teams)

  # Check for NAs in home_team_idx and away_team_idx
  if (any(is.na(home_team_idx))) {
    stop("Some values in 'home_team' do not match any known teams.")
  }
  if (any(is.na(away_team_idx))) {
    stop("Some values in 'away_team' do not match any known teams.")
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

#
#   # Check prior_par
#   if (!is.numeric(mean_psi) || length(mean_psi) != 1) {
#     stop("'mean_psi' must be a numeric value.")
#   }
#   if (!is.numeric(sd_psi) || length(sd_psi) != 1 || sd_psi <= 0) {
#     stop("'sd_psi' must be a positive numeric value.")
#   }
#   if (!is.numeric(mean_gamma) || length(mean_gamma) != 1) {
#     stop("'mean_gamma' must be a numeric value.")
#   }
#   if (!is.numeric(sd_gamma) || length(sd_gamma) != 1 || sd_gamma <= 0) {
#     stop("'sd_gamma' must be a positive numeric value.")
#   }





  # Prepare data for Stan based on dynamic_rank
  if (!dynamic_rank) {
    stan_data <- list(
      N = N,
      nteams = nteams,
      team1 = as.integer(home_team_idx),
      team2 = as.integer(away_team_idx),
      mean_psi = mean_psi,
      sd_psi = sd_psi,
      mean_gamma = mean_gamma,
      sd_gamma = sd_gamma,
      mean_home = mean_home,
      sd_home = sd_home,
      ind_home = ind_home,
      y = as.integer(match_outcome)
    )
  } else {
    stan_data <- list(
      N = N,
      nteams = nteams,
      ntimes_rank = ntimes_rank,
      instants_rank = as.integer(instants_rank),
      team1 = as.integer(home_team_idx),
      team2 = as.integer(away_team_idx),
      mean_psi = mean_psi,
      sd_psi = sd_psi,
      mean_gamma = mean_gamma,
      sd_gamma = sd_gamma,
      mean_home = mean_home,
      sd_home = sd_home,
      ind_home = ind_home,
      y = as.integer(match_outcome)
    )
  }

#   ____________________________________________________________________________
#   Static Bradley-Terry-Davidson model                                     ####

  # statBTD <- "
  # data {
  #     int<lower=1> N;          // Number of observations
  #     int<lower=1> nteams;          // Number of teams
  #     int<lower=1, upper=nteams> home_team[N];  // Index of home_team in each observation
  #     int<lower=1, upper=nteams> away_team[N];  // Index of away_team in each observation
  #     real mean_psi;                // Initial mean for psi
  #     real<lower=0> sd_psi;         // Standard deviation for psi
  #     real mean_gamma;
  #     real<lower=0> sd_gamma;
  #     int<lower=1, upper=3> y[N];      // Outcome: 1 if home_team beats away_team, 3 if away_team beats home_team, 2 for tie
  #     int<lower=0, upper=1> ind_home;        // Home effect indicator
  #     real mean_home;              // Mean for home effect
  #     real<lower=0> sd_home;      // Standard deviation for home effect
  #   }
  #   parameters {
  #     vector[nteams] psi;          // Log strength parameters for each team (static)
  #     real gamma;             // Log tie parameter
  #     real home_effect;                  // Home team effect parameter
  #   }
  #
  #   transformed parameters {
  #     real adjusted_home_effect;
  #     adjusted_home_effect = home_effect * ind_home;
  #   }
  #
  #   model {
  #     // Priors for strengths
  #     psi ~ normal(mean_psi, sd_psi);
  #
  #     // Prior for tie parameter
  #     gamma ~ normal(mean_gamma, sd_gamma);
  #
  #     // Prior for the home effect
  #
  #     home_effect ~ normal(mean_home, sd_home);
  #
  #     // Likelihood
  #     for (n in 1:N) {
  #       real delta_home_team = exp(psi[home_team[n]] + adjusted_home_effect);
  #       real delta_away_team = exp(psi[away_team[n]]);
  #       real nu = exp(gamma);
  #       real denom = delta_home_team + delta_away_team + (nu * sqrt(delta_home_team * delta_away_team));
  #       real p_i_win = delta_home_team / denom;
  #       real p_j_win = delta_away_team / denom;
  #       real p_tie = (nu * sqrt(delta_home_team * delta_away_team)) / denom;
  #       if (y[n] == 1) {
  #         target += log(p_i_win);
  #       } else if (y[n] == 3) {
  #         target += log(p_j_win);
  #       } else if (y[n] == 2) {
  #         target += log(p_tie);
  #       }
  #     }
  #   }
  # "


#   ____________________________________________________________________________
#   Dynamic Bradley-Terry-Davidson Model                                    ####

  # dynBTD <- "
  # data {
  #     int<lower=1> N;          // Number of observations
  #     int<lower=1> nteams;          // Number of teams
  #     int<lower=1> ntimes_rank;      // Number of time points
  #     int<lower=1, upper=ntimes_rank> instants_rank[N];  // Time point of each observation
  #     int<lower=1, upper=nteams> home_team[N];  // Index of home_team in each observation
  #     int<lower=1, upper=nteams> away_team[N];  // Index of away_team in each observation
  #     real mean_psi;                // Initial mean for psi
  #     real<lower=0> sd_psi;         // Standard deviation of the AR(1) process
  #     real mean_gamma;
  #     real<lower=0> sd_gamma;
  #     int<lower=1, upper=3> y[N];      // Outcome: 1 if home_team beats away_team, 3 if away_team beats home_team, 2 for tie
  #     int<lower=0, upper=1> ind_home;        // Home effect indicator
  #     real mean_home;              // Mean for home effect
  #     real<lower=0> sd_home;      // Standard deviation for home effect
  # }
  #
  # parameters {
  #     matrix[nteams, ntimes_rank] psi;     // Log strength parameters for each team over time
  #     real gamma;               // Log tie parameter
  #     real home_effect;                  // Home team effect parameter
  # }
  #
  # transformed parameters {
  #     real adjusted_home_effect;
  #     adjusted_home_effect = home_effect * ind_home;
  # }
  #
  # model {
  #     // Priors for initial strengths
  #     for (k in 1:nteams) {
  #         psi[k, 1] ~ normal(mean_psi, sd_psi);
  #     }
  #
  #     // Prior for tie parameter
  #     gamma ~ normal(mean_gamma, sd_gamma);
  #
  #     // AR(1) process for strength parameters
  #     for (t_idx in 2:ntimes_rank) {
  #         for (k in 1:nteams) {
  #             psi[k, t_idx] ~ normal(psi[k, t_idx - 1], sd_psi);
  #         }
  #     }
  #
  #     // Prior for the home effect
  #     home_effect ~ normal(mean_home, sd_home);
  #
  #     // Likelihood
  #     for (n in 1:N) {
  #         real delta_home_team = exp(psi[home_team[n], instants_rank[n]] + adjusted_home_effect);
  #         real delta_away_team = exp(psi[away_team[n], instants_rank[n]]);
  #         real nu = exp(gamma);
  #         real denom = delta_home_team + delta_away_team + (nu * sqrt(delta_home_team * delta_away_team));
  #         real p_i_win = delta_home_team / denom;
  #         real p_j_win = delta_away_team / denom;
  #         real p_tie = (nu * sqrt(delta_home_team * delta_away_team)) / denom;
  #         if (y[n] == 1) {
  #             target += log(p_i_win);
  #         } else if (y[n] == 3) {
  #             target += log(p_j_win);
  #         } else if (y[n] == 2) {
  #             target += log(p_tie);
  #         }
  #     }
  # }
  # "

  # The Stan code based on dynamic_rank
  # if (!dynamic_rank) {
  #   stan_code <- statBTD
  # } else {
  #   stan_code <- dynBTD
  # }

  #
  # # Fit the model
  # fit <- rstan::stan(model_code = stan_code, data = stan_data, ...)

  stan_model_path <- if (!dynamic_rank) {
    system.file("stan", "static_btd.stan", package = "footBayes")
  } else {
    system.file("stan", "dynamic_btd.stan", package = "footBayes")
  }

  fit <- rstan::stan(file = stan_model_path,
                     data = stan_data, ...)





  # Extract parameters
  BTDparameters <- rstan::extract(fit)

  results_df <- data.frame()

  if (!dynamic_rank) {
    # Static model
    for (team in teams) {
      team_index <- which(teams == team)

      # Get the samples for this team
      psi_samples <- BTDparameters[["psi"]][, team_index]

      # Summary statistic based on rank_measure
      rank_point <- switch(rank_measure,
                           median = stats::median(psi_samples),
                           mean = mean(psi_samples),
                           MAP = compute_MAP(psi_samples))

      df <- data.frame(
        periods = 1,  # Set periods to 1 for static model
        team = team,
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
               median = stats::median(psi_samples),
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


  # Priors list for output
  priors_output <- list(
    mean_psi = mean_psi,
    sd_psi = sd_psi,
    mean_gamma = mean_gamma,
    sd_gamma = sd_gamma
  )

  # Add home effect priors if home_effect is TRUE
  if (home_effect) {
    priors_output$mean_home <- mean_home
    priors_output$sd_home <- sd_home
  }

  # Final Output
  output <- list(
    fit = fit,
    rank = BTDrank,
    data = data,
    stan_data = stan_data,
    stan_code = stan_model_path,
    prior_par = priors_output,
    rank_measure = rank_measure
  )

  class(output) <- "btdFoot"
  return(output)
}

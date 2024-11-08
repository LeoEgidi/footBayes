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
#'     \item \code{logStrength}: Prior for the team log-strengths. Default is \code{normal(0, 3)}.
#'     \item \code{logTie}: Prior for the tie parameter. Default is \code{normal(0, 0.3)}.
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
#'@author Roberto Macrì Demartino \email{roberto.macridemartino@phd.unipd.it}.
#'
#' @examples
#' \dontrun{
#'
#' require(dplyr)
#'
#' data("italy")
#'
#' italy_2020_2021 <- italy %>%
#'   dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
#'   dplyr::filter(Season == "2020" | Season == "2021") %>%
#'   dplyr::mutate(match_outcome = dplyr::case_when(
#'     hgoal > vgoal ~ 1,        # Home team wins
#'     hgoal == vgoal ~ 2,       # Draw
#'     hgoal < vgoal ~ 3         # Away team wins
#'   )) %>%
#'   dplyr::mutate(periods = dplyr::case_when(
#'     dplyr::row_number() <= 190 ~ 1,
#'     dplyr::row_number() <= 380 ~ 2,
#'     dplyr::row_number() <= 570 ~ 3,
#'     TRUE ~ 4
#'   )) %>%  # Assign periods based on match number
#'   dplyr::select(periods, home_team = home,
#'    away_team = visitor, match_outcome)
#'
#' # Dynamic Ranking Example with Median Rank Measure
#' fit_result_dyn <- btd_foot(
#'   data = italy_2020_2021,
#'   dynamic_rank = TRUE,
#'   home_effect = TRUE,
#'   prior_par = list(
#'     logStrength = normal(0, 10),
#'     logTie = normal(0, 5),
#'     home = normal(0, 5)
#'   ),
#'   rank_measure = "median",
#'   iter = 1000,
#'   cores = 2,
#'   chains = 2
#' )
#'
#' summary(fit_result_dyn)
#'
#' # Static Ranking Example with MAP Rank Measure
#' fit_result_stat <- btd_foot(
#'   data = italy_2020_2021,
#'   dynamic_rank = FALSE,
#'   prior_par = list(
#'     logStrength = normal(0, 10),
#'     logTie = normal(0, 5),
#'     home = normal(0, 5)
#'   ),
#'   rank_measure = "MAP",
#'   iter = 1000,
#'   chains = 2
#' )
#'
#' summary(fit_result_stat)
#' }
#' @importFrom rstan stan extract
#' @export


btd_foot <- function(data,
                     dynamic_rank = FALSE,
                     home_effect = FALSE,
                     prior_par = list(
                        logStrength = normal(0, 3),
                        logTie = normal(0, 0.3),
                        home = normal(0, 5)
                     ),
                     rank_measure = "median",
                     ...) {

  # Set default priors
  default_priors <- list(
    logStrength = normal(0, 3),
    logTie = normal(0, 0.3),
    home = normal(0, 5)
  )

  # If prior_par is NULL, set it to an empty list
  if (is.null(prior_par)) {
    prior_par <- list()
  }

  # Merge prior_par with defaults
  prior_par <- utils::modifyList(default_priors, prior_par)

  # Validate prior names
  allowed_prior_names <- c("logStrength", "logTie", "home")

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
  logStrength_prior <- prior_par$logStrength
  logTie_prior <- prior_par$logTie
  home_prior <- prior_par$home

  # Only normal prior

  if (logStrength_prior$dist != "normal" || logTie_prior$dist != "normal" ||
      (home_effect && home_prior$dist != "normal")) {
    stop("Prior distributions must be 'normal'.")
  }


  mean_logStrength <- logStrength_prior$location
  sd_logStrength <- logStrength_prior$scale
  mean_logTie <- logTie_prior$location
  sd_logTie <- logTie_prior$scale

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
  # check_prior(mean_logStrength, "mean_logStrength")
  # check_prior(sd_logStrength, "sd_logStrength", positive = TRUE)
  # check_prior(mean_logTie, "mean_logTie")
  # check_prior(sd_logTie, "sd_logTie", positive = TRUE)
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
  required_cols <- c("periods", "home_team", "away_team", "match_outcome")
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
#   if (!is.numeric(mean_logStrength) || length(mean_logStrength) != 1) {
#     stop("'mean_logStrength' must be a numeric value.")
#   }
#   if (!is.numeric(sd_logStrength) || length(sd_logStrength) != 1 || sd_logStrength <= 0) {
#     stop("'sd_logStrength' must be a positive numeric value.")
#   }
#   if (!is.numeric(mean_logTie) || length(mean_logTie) != 1) {
#     stop("'mean_logTie' must be a numeric value.")
#   }
#   if (!is.numeric(sd_logTie) || length(sd_logTie) != 1 || sd_logTie <= 0) {
#     stop("'sd_logTie' must be a positive numeric value.")
#   }





  # Prepare data for Stan based on dynamic_rank
  if (!dynamic_rank) {
    stan_data <- list(
      N = N,
      nteams = nteams,
      team1 = as.integer(home_team_idx),
      team2 = as.integer(away_team_idx),
      mean_logStrength = mean_logStrength,
      sd_logStrength = sd_logStrength,
      mean_logTie = mean_logTie,
      sd_logTie = sd_logTie,
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
      mean_logStrength = mean_logStrength,
      sd_logStrength = sd_logStrength,
      mean_logTie = mean_logTie,
      sd_logTie = sd_logTie,
      mean_home = mean_home,
      sd_home = sd_home,
      ind_home = ind_home,
      y = as.integer(match_outcome)
    )
  }

  # Prepare data for Stan based on dynamic_rank

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
      logStrength_samples <- BTDparameters[["logStrength"]][, team_index]

      # Summary statistic based on rank_measure
      rank_point <- switch(rank_measure,
                           median = stats::median(logStrength_samples),
                           mean = mean(logStrength_samples),
                           MAP = compute_MAP(logStrength_samples))

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
        logStrength_samples <- BTDparameters[["logStrength"]][, team_index, k]
        switch(rank_measure,
               median = stats::median(logStrength_samples),
               mean = mean(logStrength_samples),
               MAP = compute_MAP(logStrength_samples))
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
    mean_logStrength = mean_logStrength,
    sd_logStrength = sd_logStrength,
    mean_logTie = mean_logTie,
    sd_logTie = sd_logTie
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
    stan_code = fit@stanmodel,
    prior_par = priors_output,
    rank_measure = rank_measure,
    team_names = teams
  )

  class(output) <- "btdFoot"
  return(output)
}

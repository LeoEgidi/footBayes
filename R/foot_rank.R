#' Rank and points predictions
#'
#' Posterior predictive plots and final rank table for football seasons.
#'
#' @param object An object either of class \code{stanFoot}, \code{CmdStanFit}, or \code{stanfit}.
#' @param data A data frame containing match data with columns:
#'   \itemize{
#'     \item \code{periods}:  Time point of each observation (integer >= 1).
#'     \item \code{home_team}: Home team's name (character string).
#'     \item \code{away_team}: Away team's name (character string).
#'     \item \code{home_goals}: Goals scored by the home team (integer >= 0).
#'     \item \code{away_goals}: Goals scored by the away team (integer >= 0).
#'   }
#' @param teams An optional character vector specifying team names to include. If \code{NULL}, all teams are included.
#' @param visualize Type of plots, one among \code{"aggregated"} or \code{"individual"}. Default is \code{"individual"}.
#'
#' @return
#'
#' Final rank tables and plots with the predicted points for the selected teams as given by an object of class
#' \code{stanFoot}, \code{CmdStanFit}, or \code{stanfit}.
#'
#' @details
#'
#' For Bayesian models fitted via \code{stan_foot} the final rank tables are computed according to the
#' simulation from the posterior predictive distribution of future (out-of-sample) matches.
#' The dataset should refer to one or more seasons from a given national football league (Premier League, Serie A, La Liga, etc.).
#'
#'
#'
#' @author Leonardo Egidi \email{legidi@units.it} and Roberto Macrì Demartino \email{roberto.macridemartino@deams.units.it}
#'
#' @examples
#' \dontrun{
#' if (instantiate::stan_cmdstan_exists()) {
#'   library(dplyr)
#'
#'   data("italy")
#'   italy_1999_2000 <- italy %>%
#'     dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
#'     dplyr::filter(Season == "1999" | Season == "2000")
#'
#'   colnames(italy_1999_2000) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")
#'
#'   fit <- stan_foot(italy_1999_2000, "double_pois", iter_sampling = 200)
#'   foot_rank(fit, italy_1999_2000)
#'   foot_rank(fit, italy_1999_2000, visualize = "individual")
#' }
#' }
#'
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point
#'   scale_colour_manual ggtitle labs theme_bw theme element_text annotate facet_wrap
#'   xlab ylab ylim
#' @export

foot_rank <- function(object, data,
                      teams = NULL,
                      visualize = "individual") {
  #   ____________________________________________________________________________
  #   Data and argument checks                                                ####

  # Check required columns in the data
  required_cols <- c("periods", "home_team", "away_team", "home_goals", "away_goals")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("data is missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  # Check required visualize
  match.arg(visualize, c("aggregated", "individual"))

  # Extract simulation draws based on the object's class
  valid_sim_names <- c("y_prev", "diff_y_prev", "y_rep", "diff_y_rep")

  if (inherits(object, c("stanFoot", "CmdStanFit"))) {
    draws <- if (inherits(object, "stanFoot")) {
      object$fit$draws()
    } else {
      object$draws() # for CmdStanFit objects
    }
    draws <- posterior::as_draws_rvars(draws)
    if (!any(valid_sim_names %in% names(draws))) {
      stop("Model does not contain at least one between: 'y_prev', 'y_rep', 'diff_y_prev', 'diff_y_rep' in its samples.")
    }
    sims <- list()
    for (name in valid_sim_names) {
      if (name %in% names(draws)) {
        sims[[name]] <- posterior::draws_of(draws[[name]])
      }
    }
  } else if (inherits(object, "stanfit")) {
    sims <- rstan::extract(object)
    if (!any(valid_sim_names %in% names(sims))) {
      stop("Model does not contain at least one between: 'y_prev', 'y_rep', 'diff_y_prev', 'diff_y_rep' in its samples.")
    }
  } else {
    stop("Please provide an object of class 'stanFoot', 'CmdStanFit', or 'stanfit'.")
  }

  # Prepare team and season information
  y <- as.matrix(data[, 4:5])
  teams_all <- unique(c(data$home_team, data$away_team))
  team_home <- match(data$home_team, teams_all)
  team_away <- match(data$away_team, teams_all)
  seasons_levels <- unique(data$periods)
  team_seasons <- vector("list", length(seasons_levels))
  for (j in seq_along(seasons_levels)) {
    team_seasons[[j]] <- unique(team_home[data$periods == seasons_levels[j]])
  }

  # Determine simulation type: in-sample versus out-of-sample
  if (is.null(sims$diff_y_prev) && is.null(sims$y_prev)) {
    # In-sample case: use y_rep or diff_y_rep from training matches
    N <- nrow(data)
    N_prev <- 0
    if (!is.null(sims$y_rep)) {
      y_rep1 <- sims$y_rep[, , 1]
      y_rep2 <- sims$y_rep[, , 2]
    } else {
      # t-student case
      y_rep1 <- round(abs(sims$diff_y_rep) * (sims$diff_y_rep > 0))
      y_rep2 <- round(abs(sims$diff_y_rep) * (sims$diff_y_rep < 0))
    }
    team1_prev <- team_home[1:N]
    team2_prev <- team_away[1:N]
  }


  if (!is.null(sims$diff_y_prev) && is.null(sims$y_prev)) {
    # Out-of-sample: t-student case
    N_prev <- dim(sims$diff_y_prev)[2]
    N <- dim(sims$diff_y_rep)[2]
    y_rep1 <- round(abs(sims$diff_y_prev) * (sims$diff_y_prev > 0))
    y_rep2 <- round(abs(sims$diff_y_prev) * (sims$diff_y_prev < 0))
    team1_prev <- team_home[(N + 1):(N + N_prev)]
    team2_prev <- team_away[(N + 1):(N + N_prev)]
  }

  if (is.null(sims$diff_y_prev) && !is.null(sims$y_prev)) {
    # Out-of-sample: double Poisson / bivariate Poisson case
    N_prev <- dim(sims$y_prev)[2]
    N <- dim(sims$y_rep)[2]
    y_rep1 <- sims$y_prev[, , 1]
    y_rep2 <- sims$y_prev[, , 2]
    team1_prev <- team_home[(N + 1):(N + N_prev)]
    team2_prev <- team_away[(N + 1):(N + N_prev)]
  }

  if (!is.null(sims$diff_y_prev) && !is.null(sims$y_prev)) {
    # Out-of-sample: Skellam case
    N_prev <- dim(sims$y_prev)[2]
    N <- dim(sims$y_rep)[2]
    y_rep1 <- sims$y_prev[, , 1]
    y_rep2 <- sims$y_prev[, , 2]
    team1_prev <- team_home[(N + 1):(N + N_prev)]
    team2_prev <- team_away[(N + 1):(N + N_prev)]
  }

  # Identify the season in which predictions are made
  season_prev <- unique(data$periods[(N + 1):(N + N_prev)])
  season_prev <- season_prev[!is.na(season_prev)]

  # Condition to ensure that data from different seasons cannot be predicted
  if (length(unique(data$periods[(N + 1):(N + N_prev)][!is.na(data$periods[(N + 1):(N + N_prev)])])) != 1) {
    stop("Out-of-sample matches must belong to one season. Consider refitting the model.")
  }

  # Condition to ensure that when only the last matchday is predicted,
  # all teams are considered
  ind_season_prev <- which(seasons_levels == season_prev)

  if (N_prev < length(team_seasons[[ind_season_prev]]) / 2 && N_prev != 0) {
    stop(paste(
      "The number of out-of-sample matches is too small.",
      "Please refit the model with predict argument >=",
      length(team_seasons[[ind_season_prev]]) / 2
    ))

    sims$diff_y_prev <- NULL
    sims$y_prev <- NULL

    # consider in-sample case
    if (!is.null(sims$y_rep)) {
      N <- nrow(data) - N_prev
      y_rep1 <- sims$y_rep[, , 1]
      y_rep2 <- sims$y_rep[, , 2]
      team1_prev <- team_home[1:N]
      team2_prev <- team_away[1:N]
    } else {
      # t-student case
      N <- nrow(data) - N_prev
      # N_prev <- N
      y_rep1 <- round(abs(sims$diff_y_rep) * (sims$diff_y_rep > 0))
      y_rep2 <- round(abs(sims$diff_y_rep) * (sims$diff_y_rep < 0))
      team1_prev <- team_home[1:N]
      team2_prev <- team_away[1:N]
    }
  }

  # Last matchday condition
  if (length(unique(team1_prev)) != length(unique(c(team1_prev, team2_prev)))) {
    stop("Please select more out-of-sample matches (at least two complete match-days) for predictions.")
  }

  # Define index for analysis: in-sample uses matches in season_prev; otherwise use test indices
  in_sample_cond <- is.null(sims$diff_y_prev) && is.null(sims$y_prev)

  if (in_sample_cond) {
    set <- (1:N)[data$periods == season_prev]
  } else {
    set <- 1:N_prev
  }

  # Select teams to include in output
  if (is.null(teams) || all(teams == "all")) {
    teams <- teams_all[unique(team1_prev[set])]
  }
  team_index <- match(teams, teams_all)
  if (any(is.na(team_index))) {
    warning(paste(
      teams[is.na(team_index)],
      "is not in the test set. Please provide valid team names. "
    ))
    team_index <- team_index[!is.na(team_index)]
  }
  team_names <- teams_all[team_index]

  # Initialize matrices for point counts
  M <- dim(sims$diff_y_rep)[1]
  points_count <- matrix(0, M, length(teams_all))
  true_point_count <- rep(0, length(teams_all))
  number_match_days <- length(unique(team1_prev)) * 2 - 2

  # This condition means that we are "within" the season
  # and that the training set has the same teams as the test set
  suppressWarnings(
    cond_1 <- all(sort(unique(team_home)) == sort(unique(team1_prev)))
  )

  # This condition means that the training set does NOT have
  # the same teams as the test set and that we are considering
  # training data from multiple seasons
  suppressWarnings(
    cond_2 <- N > length(unique(team1_prev)) * (length(unique(team1_prev)) - 1) &&
      all(sort(unique(team_home)) == sort(unique(team1_prev))) == FALSE &&
      N %% (length(unique(team1_prev)) * (length(unique(team1_prev)) - 1)) != 0
  )

  # This condition means that we are at the end of a season
  suppressWarnings(
    cond_3 <- N %% (length(unique(team1_prev)) * (length(unique(team1_prev)) - 1)) == 0
  )

  # Type of plot

  if (visualize == "aggregated") {
    if (in_sample_cond) {
      # In-sample analysis
      true_point_count <- rep(0, length(unique(team_home)))
      # Loop over indices corresponding to season_prev
      for (n in (1:N)[data$periods == season_prev]) {
        if (y[n, 1] > y[n, 2]) {
          true_point_count[team_home[n]] <- true_point_count[team_home[n]] + 3
        } else if (y[n, 1] == y[n, 2]) {
          true_point_count[team_home[n]] <- true_point_count[team_home[n]] + 1
          true_point_count[team_away[n]] <- true_point_count[team_away[n]] + 1
        } else { # y[n,1] < y[n,2]
          true_point_count[team_away[n]] <- true_point_count[team_away[n]] + 3
        }
      }
    } else {
      # Out-of-sample analysis:
      # Define the indices for the test set (from N+1 to N+N_prev)
      test_indices <- (N + 1):(N + N_prev)
      # Check if any home or away goals are NA in the test set.
      if (any(is.na(y[test_indices, 1])) || any(is.na(y[test_indices, 2]))) {
        stop(
          "NA values detected in home_goals and/or away_goals colums in the test data.\n",
          "`visualize = \"aggregated\"` is only available for \n",
          "1) In-sample analysis\n",
          "2) Out-of-sample analysis with known home/away scores in test data"
        )
      }

      true_point_count <- rep(0, length(unique(team_home)))
      for (n in 1:N_prev) {
        current_index <- N + n # current row in y
        if (y[current_index, 1] > y[current_index, 2]) {
          true_point_count[team1_prev[n]] <- true_point_count[team1_prev[n]] + 3
        } else if (y[current_index, 1] == y[current_index, 2]) {
          true_point_count[team1_prev[n]] <- true_point_count[team1_prev[n]] + 1
          true_point_count[team2_prev[n]] <- true_point_count[team2_prev[n]] + 1
        } else {
          true_point_count[team2_prev[n]] <- true_point_count[team2_prev[n]] + 3
        }
      }
    }
    obs <- sort.int(true_point_count, index.return = TRUE, decreasing = TRUE)$x
    obs_names <- sort.int(true_point_count, index.return = TRUE, decreasing = TRUE)$ix
    teams_rank_names <- teams_all[obs_names]
    teams_rank_names <- teams_rank_names[1:length(unique(team1_prev))]

    if (cond_2 == TRUE) {
      number_match_days <- length(unique(team1_prev)) * 2 - 2
      mod <- floor((N / (length(unique(team1_prev)) / 2)) / number_match_days)
      old_matches <- number_match_days * mod * length(unique(team1_prev)) / 2
      new_N <- seq(1 + old_matches, N)

      # compute the true points on the training set
      true_point_count_pre <- rep(0, length(unique(team_home)))
      for (n in new_N) {
        if (y[(n), 1] > y[(n), 2]) {
          true_point_count_pre[team_home[n]] <- true_point_count_pre[team_home[n]] + 3
        } else if (y[(n), 1] == y[(n), 2]) {
          true_point_count_pre[team_home[n]] <- true_point_count_pre[team_home[n]] + 1
          true_point_count_pre[team_away[n]] <- true_point_count_pre[team_away[n]] + 1
        } else if (y[(n), 1] < y[(n), 2]) {
          true_point_count_pre[team_away[n]] <- true_point_count_pre[team_away[n]] + 3
        }
      }
    } else {
      # compute the true points on the training set
      true_point_count_pre <- rep(0, length(unique(team_home)))
      if (in_sample_cond == FALSE) {
        for (n in 1:N) {
          if (y[(n), 1] > y[(n), 2]) {
            true_point_count_pre[team_home[n]] <- true_point_count_pre[team_home[n]] + 3
          } else if (y[(n), 1] == y[(n), 2]) {
            true_point_count_pre[team_home[n]] <- true_point_count_pre[team_home[n]] + 1
            true_point_count_pre[team_away[n]] <- true_point_count_pre[team_away[n]] + 1
          } else if (y[(n), 1] < y[(n), 2]) {
            true_point_count_pre[team_away[n]] <- true_point_count_pre[team_away[n]] + 3
          }
        }
      }
    }

    # compute the points on the MCMC
    for (t in 1:M) {
      if (cond_1 || cond_2) {
        points_count[t, ] <- true_point_count_pre
      }
      if (in_sample_cond) {
        set <- (1:N)[data$periods == season_prev]
      } else {
        set <- (1:N_prev)
      }

      for (n in set) {
        if (y_rep1[t, n] > y_rep2[t, n]) {
          points_count[t, team1_prev[n]] <- points_count[t, team1_prev[n]] + 3
        } else if (y_rep1[t, n] == y_rep2[t, n]) {
          points_count[t, team1_prev[n]] <- points_count[t, team1_prev[n]] + 1
          points_count[t, team2_prev[n]] <- points_count[t, team2_prev[n]] + 1
        } else if (y_rep1[t, n] < y_rep2[t, n]) {
          points_count[t, team2_prev[n]] <- points_count[t, team2_prev[n]] + 3
        }
      }
    }

    # Assumption for games "within" the season
    if (cond_1 || cond_2) {
      obs <- sort.int(true_point_count + true_point_count_pre, index.return = TRUE, decreasing = TRUE)$x
      obs_names <- sort.int(true_point_count + true_point_count_pre, index.return = TRUE, decreasing = TRUE)$ix
      teams_rank_names <- teams_all[obs_names]
      teams_rank_names <- teams_rank_names[1:length(unique(team1_prev))]
    }

    if (is.matrix(points_count[, team_index])) {
      expected_point <- apply(points_count[, team_index], 2, median)
      points_25 <- apply(points_count[, team_index], 2, function(x) quantile(x, 0.25))
      points_75 <- apply(points_count[, team_index], 2, function(x) quantile(x, 0.75))
      points_025 <- apply(points_count[, team_index], 2, function(x) quantile(x, 0.025))
      points_975 <- apply(points_count[, team_index], 2, function(x) quantile(x, 0.975))
      sd_expected <- apply(points_count[, team_index], 2, sd)
    } else {
      expected_point <- median(points_count[, team_index])
      points_25 <- quantile(points_count[, team_index], 0.25)
      points_75 <- quantile(points_count[, team_index], 0.75)
      points_025 <- quantile(points_count[, team_index], 0.025)
      points_975 <- quantile(points_count[, team_index], 0.975)
      sd_expected <- sd(points_count[, team_index])
    }

    class <- sort.int(expected_point,
      index.return = TRUE,
      decreasing = TRUE
    )

    rank_bar <- cbind(
      teams_all[team_index][class$ix],
      class$x,
      points_25[class$ix],
      points_75[class$ix],
      points_025[class$ix],
      points_975[class$ix]
    )

    rank_frame <- data.frame(
      teams = rank_bar[, 1],
      mid = as.numeric(as.vector(rank_bar[, 2])),
      obs = obs[match(rank_bar[, 1], teams_rank_names)],
      lo = as.numeric(as.vector(rank_bar[, 3])),
      hi = as.numeric(as.vector(rank_bar[, 4])),
      lo2 = as.numeric(as.vector(rank_bar[, 5])),
      hi2 = as.numeric(as.vector(rank_bar[, 6]))
    )

    rank_frame$teams <- factor(rank_frame$teams,
      levels = rank_bar[, 1]
    )
    p <- ggplot() +
      geom_ribbon(aes(x = teams, ymin = lo2, ymax = hi2, group = 1),
        data = rank_frame,
        fill = "#efe173" # Light yellow for 95% CI ribbon
      ) +
      geom_ribbon(aes(x = teams, ymin = lo, ymax = hi, group = 1),
        data = rank_frame,
        fill = "#d6c545" # Medium yellow for 50% CI ribbon
      ) +
      geom_line(aes(x = teams, y = mid, group = 1, color = "Simulated"),
        data = rank_frame
      ) +
      geom_point(aes(x = teams, y = obs, color = "Observed"),
        data = rank_frame
      ) +
      scale_colour_manual(
        name = "",
        values = c(Observed = "blue", Simulated = "#ab9d37"), # Dark yellow for line
        labels = c("Observed", "Simulated")
      ) +
      ggtitle("Posterior predicted points and ranks") +
      labs(x = "Teams", y = "Points") +
      theme_bw() +
      theme(
        legend.position = "top",
        legend.text = element_text(size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )


    tbl <- rank_frame[, c(1, 3, 2, 4, 5)]
    tbl$lo <- round(tbl$lo)
    tbl$hi <- round(tbl$hi)
    tbl$mid <- round(tbl$mid)

    colnames(tbl) <- c("teams", "obs. points", "median", "q25", "q75")
    if (length(teams) == 1) {
      rownames(tbl) <- "1"
      return(tbl)
    } else {
      return(list(rank_table = tbl, rank_plot = p))
    }
  } else if (visualize == "individual") {
    if (cond_1 == TRUE) {
      if (in_sample_cond == TRUE) {
        day_index <- floor((length(set) / (length(unique(team1_prev[set])) / 2)))
        day_index_rep <- rep(rep(seq(1, day_index),
          each = length(unique(team1_prev[set])) / 2
        ), length(unique(seasons_levels)))
        day_index_prev <- rep(seq(
          (day_index + 1),
          (length(set) + N_prev) / (length(unique(team1_prev[set])) / 2)
        ), each = length(unique(team1_prev[set])) / 2)
        day_index_prev <- day_index_rep
        set2 <- set
      } else {
        day_index <- floor(N / (length(unique(team1_prev[set])) / 2))
        day_index_rep <- rep(rep(seq(1, day_index),
          each = length(unique(team1_prev[set])) / 2
        ), length(unique(seasons_levels)))
        day_index_prev <- rep(seq(
          (day_index + 1),
          (N + N_prev) / (length(unique(team1_prev[set])) / 2)
        ), each = length(unique(team1_prev[set])) / 2)
        set2 <- (1:N)
      }


      # Compute the true point for the training sample, dynamically
      true_point_count_pre_dyn <- matrix(0, length(unique(team_home)), day_index)
      for (n in set2) {
        if (y[(n), 1] > y[(n), 2]) {
          true_point_count_pre_dyn[team_home[n], day_index_rep[n]] <- true_point_count_pre_dyn[team_home[n], day_index_rep[n]] + 3
          true_point_count_pre_dyn[team_away[n], day_index_rep[n]] <- true_point_count_pre_dyn[team_away[n], day_index_rep[n]]
        } else if (y[(n), 1] == y[(n), 2]) {
          true_point_count_pre_dyn[team_home[n], day_index_rep[n]] <- true_point_count_pre_dyn[team_home[n], day_index_rep[n]] + 1
          true_point_count_pre_dyn[team_away[n], day_index_rep[n]] <- true_point_count_pre_dyn[team_away[n], day_index_rep[n]] + 1
        } else if (y[(n), 1] < y[(n), 2]) {
          true_point_count_pre_dyn[team_home[n], day_index_rep[n]] <- true_point_count_pre_dyn[team_home[n], day_index_rep[n]]
          true_point_count_pre_dyn[team_away[n], day_index_rep[n]] <- true_point_count_pre_dyn[team_away[n], day_index_rep[n]] + 3
        }
      }


      cumsum_points_pre <- t(apply(true_point_count_pre_dyn, 1, cumsum))
    } else if (cond_2 == TRUE) {
      mod <- floor((N / (length(unique(team1_prev)) / 2)) / number_match_days)


      day_index <- max(1, floor((N / (length(unique(team1_prev)) / 2))) - mod * number_match_days)
      if (day_index == 1) {
        stop("Please, provide more training set matches in the model fit!")
      }

      day_index_rep <- rep(seq(1, day_index),
        each = length(unique(team1_prev)) / 2
      )
      day_index_prev <- rep(
        seq(
          (day_index + 1),
          day_index + (N + N_prev) / (length(unique(team1_prev)) / 2) - floor((N / (length(unique(team1_prev)) / 2)))
        ),
        each = length(unique(team1_prev)) / 2
      )

      if (in_sample_cond == TRUE) {
        day_index_prev <- day_index_rep
      }

      true_point_count_pre_dyn <- matrix(0, length(unique(team_home)), day_index)

      # Note: The number of teams per season may vary.
      # For example, the 2004-2005 Serie A season featured 20 teams, while the preceding season had 18.
      # Therefore, the new_N function works correctly only if the championship being predicted
      # and the historical seasons all have the same number of teams.
      old_matches <- number_match_days * mod * length(unique(team1_prev)) / 2
      new_N <- seq(1 + old_matches, N)


      # Compute the true point for the training sample, dynamically
      for (n in new_N) {
        if (y[(n), 1] > y[(n), 2]) {
          true_point_count_pre_dyn[team_home[n], day_index_rep[n - old_matches]] <- true_point_count_pre_dyn[team_home[n], day_index_rep[n - old_matches]] + 3
          true_point_count_pre_dyn[team_away[n], day_index_rep[n - old_matches]] <- true_point_count_pre_dyn[team_away[n], day_index_rep[n - old_matches]]
        } else if (y[(n), 1] == y[(n), 2]) {
          true_point_count_pre_dyn[team_home[n], day_index_rep[n - old_matches]] <- true_point_count_pre_dyn[team_home[n], day_index_rep[n - old_matches]] + 1
          true_point_count_pre_dyn[team_away[n], day_index_rep[n - old_matches]] <- true_point_count_pre_dyn[team_away[n], day_index_rep[n - old_matches]] + 1
        } else if (y[(n), 1] < y[(n), 2]) {
          true_point_count_pre_dyn[team_home[n], day_index_rep[n - old_matches]] <- true_point_count_pre_dyn[team_home[n], day_index_rep[n - old_matches]]
          true_point_count_pre_dyn[team_away[n], day_index_rep[n - old_matches]] <- true_point_count_pre_dyn[team_away[n], day_index_rep[n - old_matches]] + 3
        }
      }
      cumsum_points_pre <- t(apply(true_point_count_pre_dyn, 1, cumsum))
    } else if (cond_3 == TRUE) {
      day_index <- 0
      day_index_rep <- rep(seq(1, day_index),
        each = length(unique(team1_prev)) / 2
      )
      day_index_prev <- rep(seq((day_index + 1), (N_prev) / (length(unique(team1_prev)) / 2)),
        each = length(unique(team1_prev)) / 2
      )

      if (in_sample_cond == TRUE) {
        day_index_prev <- day_index_rep
      }
    }

    # Compute the true points for the test set sample, dynamically
    true_points_count_post_dyn <- matrix(NA, length(unique(team_home)), max(unique(day_index_prev)))


    # Compute the points on the MCMC, dynamically
    point_count_dyn <- array(0, c(M, length(unique(team_home)), max(day_index_prev)))
    cumsum_points_dyn <- array(0, c(M, length(unique(team_home)), max(day_index_prev)))
    for (t in 1:M) {
      if (cond_3 == FALSE) {
        if (in_sample_cond == FALSE) {
          point_count_dyn[t, , 1:day_index] <- true_point_count_pre_dyn
        }
      }

      if (in_sample_cond) {
        set <- (1:N)[data$periods == season_prev]
      } else {
        set <- (1:N_prev)
      }

      for (n in set) {
        if (y_rep1[t, n] > y_rep2[t, n]) {
          point_count_dyn[t, team1_prev[n], day_index_prev[n]] <- point_count_dyn[t, team1_prev[n], day_index_prev[n]] + 3
          point_count_dyn[t, team2_prev[n], day_index_prev[n]] <- point_count_dyn[t, team2_prev[n], day_index_prev[n]]
        } else if (y_rep1[t, n] == y_rep2[t, n]) {
          point_count_dyn[t, team1_prev[n], day_index_prev[n]] <- point_count_dyn[t, team1_prev[n], day_index_prev[n]] + 1
          point_count_dyn[t, team2_prev[n], day_index_prev[n]] <- point_count_dyn[t, team2_prev[n], day_index_prev[n]] + 1
        } else if (y_rep1[t, n] < y_rep2[t, n]) {
          point_count_dyn[t, team1_prev[n], day_index_prev[n]] <- point_count_dyn[t, team1_prev[n], day_index_prev[n]]
          point_count_dyn[t, team2_prev[n], day_index_prev[n]] <- point_count_dyn[t, team2_prev[n], day_index_prev[n]] + 3
        }
      }

      cumsum_points_dyn[t, , ] <- t(apply(point_count_dyn[t, , ], 1, cumsum))
    }

    cumsum_points_post <- t(apply(true_points_count_post_dyn, 1, cumsum))
    cumsum_points_post <- cumsum_points_post[, unique(day_index_prev)]
    # If cumsum_points_post is a vector, it means we are predicting
    # only the last matchday. For the following code,
    # it needs to be converted into a matrix
    if (is.vector(cumsum_points_post)) {
      cumsum_points_post <- as.matrix(cumsum_points_post)
    }

    # Compute quantiles for MCMC point
    points_dyn_med <- apply(cumsum_points_dyn, c(2, 3), median)
    punti_dyn_025 <- apply(cumsum_points_dyn, c(2, 3), function(x) quantile(x, c(0.025)))
    punti_dyn_25 <- apply(cumsum_points_dyn, c(2, 3), function(x) quantile(x, c(0.25)))
    punti_dyn_75 <- apply(cumsum_points_dyn, c(2, 3), function(x) quantile(x, c(0.75)))
    punti_dyn_975 <- apply(cumsum_points_dyn, c(2, 3), function(x) quantile(x, c(0.975)))

    if (cond_1 == TRUE) {
      if (in_sample_cond == FALSE) {
        if (length(teams) == 1) {
          mt_obs <- reshape2::melt(c(
            cumsum_points_pre[team_index, ],
            cumsum_points_pre[team_index, day_index] +
              cumsum_points_post[team_index, ]
          ))$value
          mt_50 <- reshape2::melt(c(
            rep(NA, day_index),
            points_dyn_med[team_index, (day_index + 1):max(day_index_prev)]
          ))$value
        } else {
          mt_obs <- reshape2::melt(cbind(
            cumsum_points_pre[team_index, ],
            cumsum_points_pre[team_index, day_index] +
              cumsum_points_post[team_index, ]
          ))$value

          mt_50 <- reshape2::melt(cbind(
            matrix(
              NA,
              length(team_names),
              # length(unique(team_home)),
              day_index
            ),
            points_dyn_med[team_index, (day_index + 1):max(day_index_prev)]
          ))$value
        }
      } else {
        mt_obs <- reshape2::melt(cumsum_points_pre[team_index, ])$value
        mt_50 <- reshape2::melt(points_dyn_med[team_index, ])$value
      }
    } else if (cond_2 == TRUE) {
      if (length(teams) == 1) {
        mt_obs <- reshape2::melt(c(
          cumsum_points_pre[team_index, ],
          cumsum_points_pre[team_index, day_index] +
            cumsum_points_post[team_index, ]
        ))$value
        mt_50 <- reshape2::melt(c(
          rep(NA, day_index),
          points_dyn_med[team_index, (day_index + 1):max(day_index_prev)]
        ))$value
      } else {
        mt_obs <- reshape2::melt(cbind(
          cumsum_points_pre[team_index, ],
          cumsum_points_pre[team_index, day_index] +
            cumsum_points_post[team_index, ]
        ))$value
        mt_50 <- reshape2::melt(cbind(
          matrix(
            NA,
            length(team_names),
            day_index
          ),
          points_dyn_med[team_index, (day_index + 1):max(day_index_prev)]
        ))$value
      }
    } else if (cond_3 == TRUE) {
      mt_obs <- reshape2::melt(cumsum_points_post[team_index, ])$value
      mt_50 <- reshape2::melt(points_dyn_med[team_index, (day_index + 1):max(day_index_prev)])$value
    }
    mt_025 <- reshape2::melt((punti_dyn_025)[team_index, ])$value
    mt_25 <- reshape2::melt((punti_dyn_25)[team_index, ])$value
    mt_75 <- reshape2::melt((punti_dyn_75)[team_index, ])$value
    mt_975 <- reshape2::melt((punti_dyn_975)[team_index, ])$value


    df_team_sel <- data.frame(
      obs = mt_obs,
      day = rep(seq(1, max(day_index_prev)), each = length(team_index)),
      q_50 = mt_50,
      q_025 = mt_025,
      q_25 = mt_25,
      q_75 = mt_75,
      q_975 = mt_975,
      teams = rep(teams_all[team_index], max(day_index_prev))
    )



    p <- ggplot(df_team_sel, aes(day, obs)) +
      geom_ribbon(aes(x = day, ymin = q_025, ymax = q_975, group = 1),
        fill = "#efe173",
        data = df_team_sel
      ) +
      geom_ribbon(aes(x = day, ymin = q_25, ymax = q_75, group = 1),
        data = df_team_sel,
        fill = "#d6c545"
      ) +
      geom_line(aes(x = day, y = q_50, color = "Simulated"),
        data = df_team_sel,
        linewidth = 1.1, na.rm = TRUE
      ) +
      geom_line(linewidth = 0.8, linetype = "solid", aes(color = "Observed"), na.rm = TRUE) +
      xlab("Match day") +
      ylab("Cumulated Points") +
      ylim(0, max(mt_975) + 2) +
      scale_colour_manual(
        name = "",
        values = c(Observed = "blue", Simulated = "#ab9d37"),
        labels = c("Observed", "Simulated")
      ) +
      facet_wrap("teams", scales = "free") +
      ggtitle("Posterior predicted points") +
      theme_bw() +
      theme(
        plot.title = element_text(size = 22),
        legend.position = "top",
        legend.text = element_text(size = 15)
      ) +
      annotate("rect", xmin = -Inf, xmax = day_index, ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "white") +
      annotate("rect", xmin = day_index, xmax = max(day_index_prev), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "white")

    return(p)
  }
}

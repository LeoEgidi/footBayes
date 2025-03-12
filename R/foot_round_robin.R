#' Round-robin for football leagues
#'
#' Posterior predictive probabilities for a football season in a round-robin format
#'
#' @param object An object either of class \code{stanFoot}, \code{CmdStanFit}, \code{stanfit}.
#' @param data A data frame containing match data with columns:
#'   \itemize{
#'     \item \code{periods}:  Time point of each observation (integer >= 1).
#'     \item \code{home_team}: Home team's name (character string).
#'     \item \code{away_team}: Away team's name (character string).
#'     \item \code{home_goals}: Goals scored by the home team (integer >= 0).
#'     \item \code{away_goals}: Goals scored by the away team (integer >= 0).
#'   }
#' @param teams An optional character vector specifying team names to include. If \code{NULL}, all teams are included.
#' @param output An optional character string specifying the type of output to return. One of \code{"both"}, \code{"table"},
#'   or \code{"plot"}. Default is \code{"both"}.
#' @details
#'
#' For Bayesian models fitted via \code{stan_foot} the round-robin table is computed according to the
#' simulation from the posterior predictive distribution of future (out-of-sample) matches.
#' The dataset should refer to one or more seasons from a given national football league (Premier League, Serie A, La Liga, etc.).
#'
#' @return
#'
#' Round-robin plot with the home-win posterior probabilities computed from the posterior predictive distribution of the fitted model.
#'
#'
#' @author Leonardo Egidi \email{legidi@units.it} and Roberto Macrì Demartino \email{roberto.macridemartino@deams.units.it}
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' data("italy")
#' italy_1999_2000 <- italy %>%
#'   dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
#'   dplyr::filter(Season == "1999" | Season == "2000")
#'
#' colnames(italy_1999_2000) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")
#'
#' fit <- stan_foot(italy_1999_2000, "double_pois", predict = 45, iter = 200)
#'
#' foot_round_robin(fit, italy_1999_2000)
#' foot_round_robin(fit, italy_1999_2000, c("Parma AC", "AS Roma"))
#' }
#' @importFrom ggplot2 ggplot aes geom_tile geom_text geom_rect scale_fill_gradient
#'   scale_x_continuous scale_y_continuous theme_bw theme element_text ggtitle rel
#' @importFrom dplyr as_tibble
#' @export


foot_round_robin <- function(object, data, teams = NULL, output = "both") {

  #   ____________________________________________________________________________
  #   Data and arguments checks                                               ####

  # Check required columns in the data
  required_cols <- c("periods", "home_team", "away_team", "home_goals", "away_goals")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("data is missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  # Check required output
  match.arg(output, c("both", "table", "plot"))


  # Extract simulation draws based on the object's class
  if (inherits(object, c("stanFoot", "CmdStanFit"))) {
    draws <- if (inherits(object, "stanFoot")) {
      object$fit$draws()
    } else {
      object$draws() # for CmdStanFit objects
    }
    draws <- posterior::as_draws_rvars(draws)
    if (!("y_prev" %in% names(draws) && "y_rep" %in% names(draws) || "diff_y_prev" %in% names(draws) && "diff_y_rep" %in% names(draws))) {
      stop("Model does not contain at least one valid pair between 'y_prev' and 'y_rep' or 'diff_y_prev' and 'diff_y_rep' in its samples.")
    }
    sims <- list()
    if ("y_prev" %in% names(draws)) {
      sims$y_prev <- posterior::draws_of(draws[["y_prev"]])
    }
    if ("diff_y_prev" %in% names(draws)) {
      sims$diff_y_prev <- posterior::draws_of(draws[["diff_y_prev"]])
    }
    if ("y_rep" %in% names(draws)) {
      sims$y_rep <- posterior::draws_of(draws[["y_rep"]])
    }
    if ("diff_y_rep" %in% names(draws)) {
      sims$diff_y_rep <- posterior::draws_of(draws[["diff_y_rep"]])
    }
  } else if (inherits(object, "stanfit")) {
    sims <- rstan::extract(object)

    if (!("y_prev" %in% names(sims) && "y_rep" %in% names(sims) || "diff_y_prev" %in% names(sims) && "diff_y_rep" %in% names(sims))) {
      stop("Model does not contain at least one valid pair between 'y_prev' and 'y_rep' or 'diff_y_prev' and 'diff_y_rep' in its samples.")
    }
  } else {
    stop("Provide one among these three model fit classes: 'stanfit', 'CmdStanFit', 'stanFoot'.")
  }

  #   ____________________________________________________________________________
  #   Dataframes and plots                                                    ####

  # Prepare team and season information
  y <- as.matrix(data[, 4:5])
  teams_all <- unique(c(data$home_team, data$away_team))
  team_home <- match(data$home_team, teams_all)
  team_away <- match(data$away_team, teams_all)

  # Determine the model
  if (!is.null(sims$diff_y_prev) && is.null(sims$y_prev)) {
    # t-student case
    N_prev <- dim(sims$diff_y_prev)[2]
    N <- dim(sims$diff_y_rep)[2]
    y_rep1 <- round(abs(sims$diff_y_prev) * (sims$diff_y_prev > 0))
    y_rep2 <- round(abs(sims$diff_y_prev) * (sims$diff_y_prev < 0))
    team1_prev <- team_home[(N + 1):(N + N_prev)]
    team2_prev <- team_away[(N + 1):(N + N_prev)]
  } else if (!is.null(sims$y_prev)) {
    # Skellam, double Poisson, or bivariate Poisson cases
    N_prev <- dim(sims$y_prev)[2]
    N <- dim(sims$y_rep)[2]
    y_rep1 <- sims$y_prev[, , 1]
    y_rep2 <- sims$y_prev[, , 2]
    team1_prev <- team_home[(N + 1):(N + N_prev)]
    team2_prev <- team_away[(N + 1):(N + N_prev)]
  }


  # Condition to ensure that when only the last matchday is predicted, all teams are considered
  if (length(unique(team1_prev)) !=
    length(unique(c(team1_prev, team2_prev)))) {
    team1_prev <- c(team1_prev, team2_prev)
    team2_prev <- c(team2_prev, team1_prev)
  }

  # Select teams to include in output
  if (is.null(teams)) {
    teams <- teams_all[unique(team1_prev)]
  }
  team_index <- match(teams, teams_all)


  if (anyNA(team_index)) {
    stop(paste(
      teams[is.na(team_index)],
      "is not in the test set. Please provide a valid team name. "
    ))
    #team_index <- team_index[!is.na(team_index)]
  }

  # Initialize matrices
  team_names <- teams_all[team_index]
  nteams <- length(unique(team_home))
  nteams_new <- length(team_index)
  M <- dim(sims$diff_y_rep)[1]
  counts_mix <- matrix(0, nteams, nteams)
  number_match_days <- length(unique(team1_prev)) * 2 - 2
  punt <- matrix("-", nteams, nteams)

  suppressWarnings(
    # This condition means that we are "within" the season
    # and that the training set has the same teams as the test set
    cond_1 <- all(sort(unique(team_home)) == sort(unique(team1_prev))) && N < length(unique(team1_prev)) * (length(unique(team1_prev)) - 1)
  )
  # This condition means that the training set does NOT have
  # the same teams as the test set and that we are considering
  # training data from multiple seasons
  suppressWarnings(
    cond_2 <- N > length(unique(team1_prev)) * (length(unique(team1_prev)) - 1) &&
      all(sort(unique(team_home)) == sort(unique(team1_prev))) == FALSE &&
      N %% (length(unique(team1_prev)) * (length(unique(team1_prev)) - 1)) != 0
  )

  suppressWarnings(
    # This condition means that we are at the end of a season
    cond_3 <- N %% (length(unique(team1_prev)) * (length(unique(team1_prev)) - 1)) == 0
  )



  # Fill in the 'punt' matrix with observed match scores based on the condition
  if (cond_1 == TRUE) {
    for (n in 1:N) {
      punt[team_home[n], team_away[n]] <-
        paste(y[n, 1], "-", y[n, 2], sep = "")
    }
  } else if (cond_2 == TRUE) {
    mod <- floor((N / (length(unique(team1_prev)) / 2)) / number_match_days)
    old_matches <- number_match_days * mod * length(unique(team1_prev)) / 2
    new_N <- seq(1 + old_matches, N)

    for (n in new_N) {
      punt[team_home[n], team_away[n]] <-
        paste(y[n, 1], "-", y[n, 2], sep = "")
    }
  }

  # Compute posterior probabilities for home wins in the predicted matches
  for (n in seq_len(N_prev)) {
    prob <- sum(y_rep1[, n] > y_rep2[, n]) / M
    counts_mix[
      unique(team1_prev[n]),
      unique(team2_prev[n])
    ] <- prob
  }

  x1 <- seq(0.5, nteams_new - 1 + 0.5)
  x2 <- seq(1.5, nteams_new - 1 + 1.5)
  x1_x2 <- matrix(0, nteams_new, nteams_new)
  x2_x1 <- matrix(0, nteams_new, nteams_new)
  y1_y2 <- matrix(0, nteams_new, nteams_new)
  y2_y1 <- matrix(0, nteams_new, nteams_new)
  for (j in 1:nteams_new) {
    x1_x2[j, j] <- x1[j]
    x2_x1[j, j] <- x2[j]
    y1_y2[j, j] <- x1[j]
    y2_y1[j, j] <- x2[j]
  }
  x_ex <- seq(1, nteams_new, length.out = nteams_new)
  y_ex <- seq(1, nteams_new, length.out = nteams_new)
  data_ex <- expand.grid(Home = x_ex, Away = y_ex)
  data_ex$prob <- as.double(counts_mix[1:nteams, 1:nteams][team_index, team_index])

  # Pre-compute the rectangle boundaries into a data frame
  rect_df <- data.frame(
    xmin = as.vector(x1_x2),
    xmax = as.vector(x2_x1),
    ymin = as.vector(x1_x2),
    ymax = as.vector(x2_x1)
  )

  # Create the plot
  round_plot <- ggplot(data_ex, aes(x = Home, y = Away)) +
    geom_tile(aes(fill = prob)) +
    geom_text(aes(label = as.vector(punt[team_index, team_index])), size = 4.5) +
    geom_rect(
      data = rect_df,
      aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin, ymax = .data$ymax),
      inherit.aes = FALSE,
      fill = "black", color = "black", linewidth = 1
    ) +
    scale_fill_gradient(low = "white", high = "red3", name = "Prob") +
    scale_x_continuous(breaks = x_ex, labels = team_names, name = "Home Team") +
    scale_y_continuous(breaks = y_ex, labels = team_names, name = "Away Team") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = rel(1.2)),
      axis.text.y = element_text(size = rel(1.2)),
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12)
    ) +
    ggtitle("Home win posterior probabilities")

  if (sum(data_ex$prob) == 0) {
    tbl <- cbind(teams[data_ex$Home], teams[data_ex$Away], as.vector(punt[team_index, team_index]))
    colnames(tbl) <- c("Home", "Away", "Observed")
    tbl <- dplyr::as_tibble(tbl) %>% dplyr::filter(Home != Away)
  } else {
    tbl <- cbind(
      teams[data_ex$Home], teams[data_ex$Away], round(data_ex$prob, 3),
      as.vector(punt[team_index, team_index])
    )
    colnames(tbl) <- c("Home", "Away", "Home_prob", "Observed")
    tbl <- dplyr::as_tibble(tbl) %>% dplyr::filter(Home != Away & Home_prob != 0)
  }

  result <- list(round_table = tbl, round_plot = round_plot)
  if (output == "both") {
    return(result)
  } else if (output == "plot") {
    return(round_plot)
  } else if (output == "table") {
    return(tbl)
  }
}

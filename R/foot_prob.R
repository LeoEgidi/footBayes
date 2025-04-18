#' Plot football matches probabilities for out-of-sample football matches.
#'
#' The function provides a table containing the home win, draw and away win probabilities for a bunch of
#' out-of-sample matches as specified by \code{stan_foot} or \code{mle_foot}.
#'
#' @param object An object either of class \code{stanFoot}, \code{CmdStanFit}, \code{\link[rstan]{stanfit}}, or class
#'               \code{\link{list}} containing the Maximum Likelihood Estimates (MLE) for the model parameters fitted
#'                with \code{mle_foot}.
#' @param data A data frame containing match data with columns:
#'   \itemize{
#'     \item \code{periods}:  Time point of each observation (integer >= 1).
#'     \item \code{home_team}: Home team's name (character string).
#'     \item \code{away_team}: Away team's name (character string).
#'     \item \code{home_goals}: Goals scored by the home team (integer >= 0).
#'     \item \code{away_goals}: Goals scored by the away team (integer >= 0).
#'   }
#' @param home_team The home team(s) for the predicted matches.
#' @param away_team The away team(s) for the predicted matches.
#'
#' @return
#' A list with components:
#'  \itemize{
#'  \item \code{prob_table}: A data frame containing the results probabilities of the out-of-sample matches.
#'  \item \code{prob_plot}: A \code{ggplot} object for Bayesian models only showing the posterior predictive heatmap
#'   of exact score probabilities, with the true result highlighted.
#' }
#'
#' @details
#'
#' For Bayesian models the results probabilities are computed according to the
#' simulation from the posterior predictive distribution of future (out-of-sample) matches.
#' Specifically, matches are ordered from those in which the favorite team has the highest posterior probability
#' of winning to those where the underdog is more likely to win. For MLE models
#' fitted via the \code{mle_foot} the probabilities are computed by simulating from the MLE estimates.
#'
#'
#' @author Leonardo Egidi \email{legidi@units.it} and Roberto Macrì Demartino \email{roberto.macridemartino@deams.units.it}.
#'
#' @examples
#' \dontrun{
#' if (instantiate::stan_cmdstan_exists()) {
#'   library(dplyr)
#'
#'   data("italy")
#'   italy_2000 <- italy %>%
#'     dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
#'     dplyr::filter(Season == "2000")
#'
#'   colnames(italy_2000) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")
#'
#'
#'
#'   fit <- stan_foot(
#'     data = italy_2000,
#'     model = "double_pois",
#'     predict = 18
#'   ) # double pois
#'
#'   foot_prob(
#'     fit, italy_2000, "Inter",
#'     "Bologna FC"
#'   )
#'
#'   foot_prob(fit, italy_2000) # all the out-of-sample matches
#' }
#' }
#'
#' @importFrom posterior as_draws_rvars draws_of
#' @importFrom rstan extract
#' @importFrom ggplot2 ggplot geom_tile theme_bw scale_fill_gradient geom_rect labs
#'   scale_x_continuous scale_y_continuous element_text scale_color_manual
#'    guides facet_wrap unit ylab xlab element_blank
#' @importFrom dplyr group_by mutate arrange distinct
#' @importFrom extraDistr rbvpois rskellam
#' @importFrom metRology rt.scaled
#'
#' @export


foot_prob <- function(object, data, home_team, away_team) {
  #   ____________________________________________________________________________
  #   Data and argument checks                                                ####

  required_cols <- c("periods", "home_team", "away_team", "home_goals", "away_goals")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("data is missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  teams <- unique(c(data$home_team, data$away_team))

  if (inherits(object, "stanFoot") || inherits(object, "CmdStanFit")) {
    if (inherits(object, "stanFoot")) {
      # For stanFoot objects, the draws are available in object$fit
      draws <- object$fit$draws()
    } else {
      # For CmdStanFit objects, draw directly from object
      draws <- object$draws()
    }
    draws <- posterior::as_draws_rvars(draws)

    # Check that at least one of 'y_prev' or 'diff_y_prev' exists in draws.
    if (!("y_prev" %in% names(draws) || "diff_y_prev" %in% names(draws))) {
      stop("Model '%s' does not contain 'y_prev' or 'diff_y_prev' in its samples. Ensure the model was fitted with 'predict' set appropriately.")
    }

    sims <- list()
    # Use 'y_prev' if available; otherwise, use 'diff_y_prev'
    if ("y_prev" %in% names(draws)) {
      sims$y_prev <- posterior::draws_of(draws[["y_prev"]])
      predict <- dim(sims$y_prev)[2]
    } else {
      sims$diff_y_prev <- posterior::draws_of(draws[["diff_y_prev"]])
      predict <- dim(sims$diff_y_prev)[2]
    }
  } else if (inherits(object, "stanfit")) {
    sims <- rstan::extract(object)

    if (!("y_prev" %in% names(sims) || "diff_y_prev" %in% names(sims))) {
      stop("Model '%s' does not contain 'y_prev' or 'diff_y_prev' in its samples. Ensure the model was fitted with 'predict' set appropriately.")
    }

    if ("y_prev" %in% names(sims)) {
      predict <- dim(sims$y_prev)[2]
    } else {
      predict <- dim(sims$diff_y_prev)[2]
    }
  } else if (inherits(object, "list")) {
    predict <- object$predict
  } else {
    stop("Provide one among these four model fit classes: 'stanfit', 'CmdStanFit', 'stanFoot' or 'list'.")
  }

  if (is.null(predict) || predict == 0) {
    stop("foot_prob cannot be used if the 'predict' argument is set to zero.")
  }


  #   ____________________________________________________________________________
  #   Dataframes and plots                                                    ####


  data_prev <- data[(dim(data)[1] - predict + 1):(dim(data)[1]), ]

  # Checks on home_team/away_team

  if (missing(home_team) & missing(away_team)) {
    home_team <- data_prev$home_team
    away_team <- data_prev$away_team
  }

  if (length(home_team) != length(away_team)) {
    stop("Please, include the same number for home and away teams.")
  }

  find_match <- c()
  for (i in 1:length(home_team)) {
    find_match[i] <- which(data_prev$home_team %in% home_team[i] & data_prev$away_team %in% away_team[i])
  }


  true_gol_home <- data_prev$home_goals[find_match]
  true_gol_away <- data_prev$away_goals[find_match]

  if (length(find_match) == 0) {
    stop(paste("There is not any out-of-sample match:",
      home_team, "-", away_team,
      sep = ""
    ))
  }

  # Compute probabilities with stan/mle

  if (inherits(object, c("stanFoot", "stanfit", "CmdStanFit"))) {
    if (is.null(sims$y_prev)) { # student_t model
      M <- dim(sims$diff_y_prev)[1]
      prob_h <- prob_d <- prob_a <- c()

      x <- round(sims$diff_y_prev, 0)
      prob_h <- round(apply(x, 2, function(x) sum(x > 0)) / M, 3)
      prob_d <- round(apply(x, 2, function(x) sum(x == 0)) / M, 3)
      prob_a <- round(apply(x, 2, function(x) sum(x < 0)) / M, 3)

      # only table

      tbl <- data.frame(
        home_team = home_team,
        away_team = away_team,
        prob_h = prob_h[find_match],
        prob_d = prob_d[find_match],
        prob_a = prob_a[find_match]
      )

      return(list(prob_table = tbl))
    } else { # poisson models
      M <- dim(sims$y_prev)[1]
      prediction1 <- sims$y_prev[, find_match, 1]
      prediction2 <- sims$y_prev[, find_match, 2]

      prob_h <- prob_d <- prob_a <- c()
      # For MLO computation in multiple matches branch:
      row_pos <- rep(NA, length(find_match))
      col_pos <- rep(NA, length(find_match))
      mlo <- rep(NA, length(find_match))

      data_exp_tot <- data.frame(
        Home = numeric(0),
        Away = numeric(0),
        Prob = numeric(0),
        matches = character(0),
        true_gol_home = numeric(0),
        true_gol_away = numeric(0)
      )

      if (length(find_match) == 1) {
        posterior_prop1 <- table(subset(prediction1, prediction1 < 15))
        posterior_prop2 <- table(subset(prediction2, prediction2 < 15))

        teamaa <- home_team
        teamab <- away_team

        x_min <- y_min <- min(
          length(posterior_prop1),
          length(posterior_prop2)
        )

        counts_mix <- matrix(0, x_min, y_min)

        for (j in 1:x_min) {
          for (t in 1:y_min) {
            counts_mix[j, t] <- posterior_prop1[j] * posterior_prop2[t]
          }
        }
        dim1 <- dim(counts_mix)[1]
        dim2 <- dim(counts_mix)[2]

        x_seq <- seq(0, dim1 - 1, length.out = dim1)
        y_seq <- seq(0, dim2 - 1, length.out = dim2)
        data_exp <- expand.grid(Home = x_seq, Away = y_seq)
        data_exp$Prob <- as.double(counts_mix / (M * M))
        data_exp$matches <- paste(teamaa, "-", teamab)
        data_exp$true_gol_home <- true_gol_home
        data_exp$true_gol_away <- true_gol_away

        # Overall "adjusted" probabilities
        prob_h_val <- sum(counts_mix[lower.tri(counts_mix)] / (M * M)) / sum(data_exp$Prob)
        prob_d_val <- sum(diag(counts_mix / (M * M))) / sum(data_exp$Prob)
        prob_a_val <- sum(counts_mix[upper.tri(counts_mix)] / (M * M)) / sum(data_exp$Prob)

        # MLO (most likely outcome) using which.max and arrayInd to get a single index
        ind_max <- which.max(counts_mix)
        pos <- arrayInd(ind_max, dim(counts_mix))
        row_pos_val <- pos[1]
        col_pos_val <- pos[2]
        mlo_val <- paste(row_pos_val - 1, "-", col_pos_val - 1, " (",
          round(max(counts_mix / (M * M)), 3), ")",
          sep = ""
        )

        data_exp_tot <- rbind(data_exp_tot, data_exp)

        tbl <- data.frame(
          home_team = home_team,
          away_team = away_team,
          prob_h = round(prob_h_val, 3),
          prob_d = round(prob_d_val, 3),
          prob_a = round(prob_a_val, 3),
          mlo = mlo_val
        )

        p <- ggplot(data_exp_tot, aes(Home, Away, z = Prob)) +
          geom_tile(aes(fill = Prob)) +
          theme_bw() +
          scale_fill_gradient(low = "white", high = "black") +
          geom_rect(
            aes(
              xmin = as.numeric(as.vector(true_gol_home)) - 0.5,
              xmax = as.numeric(as.vector(true_gol_home)) + 0.5,
              ymin = as.numeric(as.vector(true_gol_away)) - 0.5,
              ymax = as.numeric(as.vector(true_gol_away)) + 0.5,
              color = "True Result"
            ),
            fill = "transparent", linewidth = 1.5
          ) +
          labs(title = "Posterior match probabilities") +
          scale_x_continuous(breaks = unique(data_exp_tot$Home)) +
          scale_y_continuous(breaks = unique(data_exp_tot$Away)) +
          theme_bw() +
          theme(
            plot.title = element_text(size = 18),
            strip.text = element_text(size = 12),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16),
            plot.subtitle = element_text(size = 13),
            axis.title = element_text(size = 18),
            legend.text = element_text(size = 14)
          ) +
          scale_color_manual(name = "", values = c("True Result" = "red")) +
          guides(color = guide_legend(override.aes = list(fill = NA)))
      } else {
        teamaa <- teamab <- c()
        for (i in 1:length(find_match)) {
          posterior_prop1 <- table(prediction1[, i])
          posterior_prop2 <- table(prediction2[, i])

          teamaa[i] <- home_team[i]
          teamab[i] <- away_team[i]

          x_min <- y_min <- 5

          counts_mix <- matrix(0, x_min, y_min)

          for (j in 1:x_min) {
            for (t in 1:y_min) {
              counts_mix[j, t] <- posterior_prop1[j] * posterior_prop2[t]
            }
          }

          dim1 <- dim(counts_mix)[1]
          dim2 <- dim(counts_mix)[2]

          x_seq <- seq(0, dim1 - 1, length.out = dim1)
          y_seq <- seq(0, dim2 - 1, length.out = dim2)
          data_exp <- expand.grid(Home = x_seq, Away = y_seq)
          data_exp$Prob <- as.double(counts_mix / (M * M))
          data_exp$matches <- paste(teamaa[i], "-", teamab[i])
          data_exp$true_gol_home <- true_gol_home[i]
          data_exp$true_gol_away <- true_gol_away[i]

          # overall "adjusted" probabilities
          prob_h[i] <- sum(counts_mix[lower.tri(counts_mix)] / (M * M)) / sum(data_exp$Prob)
          prob_d[i] <- sum(diag(counts_mix / (M * M))) / sum(data_exp$Prob)
          prob_a[i] <- sum(counts_mix[upper.tri(counts_mix)] / (M * M)) / sum(data_exp$Prob)

          # MLO (most likely outcome) using which.max and arrayInd
          ind_max <- which.max(counts_mix)
          pos <- arrayInd(ind_max, dim(counts_mix))
          row_pos[i] <- pos[1]
          col_pos[i] <- pos[2]

          mlo[i] <- paste(row_pos[i] - 1, "-", col_pos[i] - 1, " (",
            round(max(counts_mix / (M * M)), 3), ")",
            sep = ""
          )

          data_exp_tot <- rbind(data_exp_tot, data_exp)
        }

        tbl <- data.frame(
          home_team = home_team,
          away_team = away_team,
          prob_h = round(prob_h, 3),
          prob_d = round(prob_d, 3),
          prob_a = round(prob_a, 3),
          mlo = mlo
        )

        data_exp_tot <- data_exp_tot %>%
          dplyr::group_by(matches) %>%
          dplyr::mutate(
            prob_h = sum(Prob[Home > Away]),
            prob_d = sum(Prob[Home == Away]),
            prob_a = sum(Prob[Home < Away])
          )

        data_exp_tot$favorite <- rep(teamaa, each = x_min * y_min)
        data_exp_tot$underdog <- rep(teamab, each = x_min * y_min)

        # Use which() to avoid NA values in indexes
        indexes <- which(data_exp_tot$prob_h < data_exp_tot$prob_a)
        temp1 <- data_exp_tot$prob_h[indexes]
        temp2 <- data_exp_tot$prob_a[indexes]
        data_exp_tot$prob_h[indexes] <- temp2
        data_exp_tot$prob_a[indexes] <- temp1

        temp_name1 <- data_exp_tot$favorite[indexes]
        temp_name2 <- data_exp_tot$underdog[indexes]
        data_exp_tot$favorite[indexes] <- temp_name2
        data_exp_tot$underdog[indexes] <- temp_name1

        temp_coord1 <- data_exp_tot$Home[indexes]
        temp_coord2 <- data_exp_tot$Away[indexes]
        data_exp_tot$Home[indexes] <- temp_coord2
        data_exp_tot$Away[indexes] <- temp_coord1

        temp_tg1 <- data_exp_tot$true_gol_home[indexes]
        temp_tg2 <- data_exp_tot$true_gol_away[indexes]
        data_exp_tot$true_gol_home[indexes] <- temp_tg2
        data_exp_tot$true_gol_away[indexes] <- temp_tg1

        data_exp_tot <- dplyr::arrange(data_exp_tot, prob_h)
        fav_teams <- data_exp_tot %>% distinct(favorite)
        und_teams <- data_exp_tot %>% distinct(underdog)
        axes_titles <- data.frame(
          matches = unique(data_exp_tot$matches),
          axis_title_x = fav_teams[, 2],
          axis_title_y = und_teams[, 2]
        )
        data_exp_tot$new_matches <- paste(data_exp_tot$favorite, "-", data_exp_tot$underdog)

        num_matches <- length(unique(data_exp_tot$new_matches))
        nrow_plot <- ceiling(sqrt(num_matches))
        ncol_plot <- ceiling(num_matches / nrow_plot)

        p <- ggplot(data_exp_tot, aes(Home, Away, z = Prob)) +
          geom_tile(aes(fill = Prob)) +
          theme_bw() +
          scale_fill_gradient(low = "white", high = "black") +
          facet_wrap(
            facets = ~ reorder(new_matches, prob_h),
            scales = "fixed",
            ncol = ncol_plot, nrow = nrow_plot
          ) +
          geom_rect(
            aes(
              xmin = as.numeric(as.vector(true_gol_home)) - 0.5,
              xmax = as.numeric(as.vector(true_gol_home)) + 0.5,
              ymin = as.numeric(as.vector(true_gol_away)) - 0.5,
              ymax = as.numeric(as.vector(true_gol_away)) + 0.5,
              color = "True Result"
            ),
            fill = "transparent", linewidth = 1.5
          ) +
          labs(title = "Posterior match probabilities") +
          ylab("Underdog") +
          xlab("Favorite") +
          theme_bw() +
          theme(
            plot.title = element_text(size = 18),
            strip.text = element_text(size = 11),
            strip.background = element_blank(),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16),
            plot.subtitle = element_text(size = 8.5),
            axis.title = element_text(size = 18),
            legend.text = element_text(size = 14),
            panel.spacing = unit(0.2, "lines")
          ) +
          scale_color_manual(name = "", values = c("True Result" = "red")) +
          guides(color = guide_legend(override.aes = list(fill = NA)))
      }
      return(list(prob_table = tbl, prob_plot = p))
    }
  } else if (inherits(object, "list")) {
    model <- object$model
    predict <- object$predict
    n.iter <- object$n.iter
    team1_prev <- object$team1_prev
    team2_prev <- object$team2_prev
    N_prev <- predict

    prediction_routine <- function(team1_prev, team2_prev, att, def, home,
                                   corr, ability, model, predict, n.iter) {
      mean_home <- exp(home[1, 2] + att[team1_prev, 2] + def[team2_prev, 2])
      mean_away <- exp(att[team2_prev, 2] + def[team1_prev, 2])

      if (model == "double_pois") {
        x <- y <- matrix(NA, n.iter, predict)
        for (n in 1:N_prev) {
          x[, n] <- rpois(n.iter, mean_home[n])
          y[, n] <- rpois(n.iter, mean_away[n])
        }
      } else if (model == "biv_pois") {
        couple <- array(NA, c(n.iter, predict, 2))
        for (n in 1:N_prev) {
          couple[, n, ] <- rbvpois(n.iter,
            a = mean_home[n],
            b = mean_away[n],
            c = corr[1, 2]
          )
        }
        x <- couple[, , 1]
        y <- couple[, , 2]
      } else if (model == "skellam") {
        diff_y <- matrix(NA, n.iter, predict)
        for (n in 1:N_prev) {
          diff_y[, n] <- rskellam(n.iter,
            mu1 = mean_home[n],
            mu2 = mean_away[n]
          )
        }
        x <- diff_y
        y <- matrix(0, n.iter, predict)
      } else if (model == "student_t") {
        sigma_y <- object$sigma_y
        diff_y <- matrix(NA, n.iter, predict)
        for (n in 1:N_prev) {
          diff_y[, n] <- rt.scaled(n.iter,
            df = 7,
            mean = home[1, 2] + ability[team1_prev[n], 2] - ability[team2_prev[n], 2],
            sd = sigma_y
          )
        }
        x <- round(diff_y)
        y <- matrix(0, n.iter, predict)
      }

      prob_func <- function(mat_x, mat_y) {
        res <- mat_x - mat_y
        prob_h <- apply(res, 2, function(x) sum(x > 0)) / n.iter
        prob_d <- apply(res, 2, function(x) sum(x == 0)) / n.iter
        prob_a <- apply(res, 2, function(x) sum(x < 0)) / n.iter
        return(list(
          prob_h = prob_h,
          prob_d = prob_d,
          prob_a = prob_a
        ))
      }

      conf <- prob_func(x, y)

      tbl <- data.frame(
        home_team = teams[team1_prev[find_match]],
        away_team = teams[team2_prev[find_match]],
        prob_h = conf$prob_h[find_match],
        prob_d = conf$prob_d[find_match],
        prob_a = conf$prob_a[find_match]
      )
      return(tbl)
    }

    if (predict != 0) {
      prob_matrix <- prediction_routine(
        team1_prev, team2_prev, object$att,
        object$def, object$home,
        object$corr, object$abilities, model, predict,
        n.iter
      )
    }

    return(list(prob_table = prob_matrix))
  }
}

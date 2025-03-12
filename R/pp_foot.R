#' Posterior predictive checks for football models
#'
#' The function provides posterior predictive plots to check the adequacy of the Bayesian models as
#' returned by the \code{stan_foot} function.
#'
#' @param object An object either of class \code{stanFoot}, \code{CmdStanFit}, \code{\link[rstan]{stanfit}}.
#' @param data A data frame containing match data with columns:
#'   \itemize{
#'     \item \code{periods}:  Time point of each observation (integer >= 1).
#'     \item \code{home_team}: Home team's name (character string).
#'     \item \code{away_team}: Away team's name (character string).
#'     \item \code{home_goals}: Goals scored by the home team (integer >= 0).
#'     \item \code{away_goals}: Goals scored by the away team (integer >= 0).
#'   }
#' @param type  Type of plots, one among \code{"aggregated"} or \code{"matches"}. Default is \code{"aggregated"}.
#' @param coverage Argument to specify the width \eqn{1-\alpha} of posterior probability intervals. Default is 0.95.
#'
#' @return
#'
#' Posterior predictive plots: when \code{"aggregated"} (default) is selected, the function
#' returns a frequency plot for some pre-selected goal-difference values,
#' along with their correspondent Bayesian p-values, computed as
#' \eqn{Pr(y_rep \ge y)|y)}, where \eqn{y_rep} is a data replication from the
#' posterior predictive distribution (more details in Gelman et al., 2013).
#' Bayesian p-values very close to 0 or 1 could exhibit
#' possible model misfits.
#'
#' When \code{"matches"} is selected an ordered-frequency plot for all the
#' goal-differences in the considered matches is provided, along with the
#' empirical Bayesian coverage at level \eqn{1-\alpha}.
#'
#' @author Leonardo Egidi \email{legidi@units.it} and Roberto Macrì Demartino \email{roberto.macridemartino@deams.units.it}
#'
#' @references
#'
#' Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B. (2013). Bayesian data analysis. CRC press.
#'
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
#'   fit <- stan_foot(italy_2000, "double_pois", iter = 200)
#'
#'   pp_foot(fit, italy_2000)
#' }
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point labs scale_colour_manual annotate
#'   scale_x_discrete scale_x_continuous scale_y_continuous dup_axis geom_ribbon geom_line
#'   theme_bw theme rel
#' @importFrom matrixStats colMedians colVars colQuantiles
#' @importFrom rstan extract
#' @importFrom posterior as_draws_rvars draws_of
#' @importFrom rlang .data
#'
#' @export


pp_foot <- function(object, data,
                    type = "aggregated",
                    coverage = 0.95) {
  #   ____________________________________________________________________________
  #   Data and argument checks                                                ####

  type <- match.arg(type, c("aggregated", "matches"))

  required_cols <- c("periods", "home_team", "away_team", "home_goals", "away_goals")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("data is missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Check if object is of class "stanFoot" or "stanfit"
  if (inherits(object, c("stanFoot", "CmdStanFit"))) {
    draws <- if (inherits(object, "stanFoot")) {
      object$fit$draws()
    } else {
      object$draws() # for CmdStanFit objects
    }
    draws <- posterior::as_draws_rvars(draws)
    if (!("diff_y_rep" %in% names(draws))) {
      stop("Model does not contain 'diff_y_rep' in its samples.")
    }
    sims <- list()
    sims$diff_y_rep <- posterior::draws_of(draws[["diff_y_rep"]])
  } else if (inherits(object, "stanfit")) {
    sims <- rstan::extract(object)

    if (!("diff_y_rep" %in% names(sims))) {
      stop("Model does not contain 'diff_y_rep' in its samples.")
    }
  } else {
    stop("Provide one among these three model fit classes: 'stanfit', 'CmdStanFit', 'stanFoot'.")
  }


  #   ____________________________________________________________________________
  #   Dataframes and plots                                                    ####

  y <- as.matrix(data[, 4:5])
  goal_diff <- as.vector(y[, 1] - y[, 2])
  goal_diff_rep <- sims$diff_y_rep
  esiti_short <- seq(-3, 3, 1)
  M <- dim(goal_diff_rep)[1]
  freq_rel_matrix <- matrix(NA, M, length(esiti_short))
  ngames_train <- dim(goal_diff_rep)[2]


  if (type == "aggregated") {
    check.integer <- function(x) {
      x == round(x)
    }

    if (check.integer(median(goal_diff_rep)) == FALSE) { # student_t models adjustment
      goal_diff_rep <- round(goal_diff_rep, 0)
    }

    freq_rel_matrix <- t(apply(goal_diff_rep, 1, function(row_j) {
      counts <- table(factor(row_j, levels = esiti_short))
      as.vector(counts) / ngames_train
    }))


    freq_rel_frame_add <- do.call(rbind, lapply(1:M, function(j) {
      data.frame(valori = esiti_short, rel = freq_rel_matrix[j, ])
    }))


    freq_rel_obs <- sapply(esiti_short, function(x) {
      sum(goal_diff == x) / ngames_train
    })

    frame <- data.frame(valori = esiti_short, rel = freq_rel_frame_add[, 2])

    p <- ggplot(frame, aes(x = valori, y = rel)) +
      # Simulated points
      geom_point(position = "jitter", alpha = 0.2, aes(colour = "simulated")) +
      # Observed segments
      annotate("segment",
        x = -3 - 0.5, y = freq_rel_obs[1],
        xend = -3 + 0.5, yend = freq_rel_obs[1],
        colour = "#1E90FF", linewidth = 2
      ) +
      annotate("segment",
        x = -2 - 0.5, y = freq_rel_obs[2],
        xend = -2 + 0.5, yend = freq_rel_obs[2],
        colour = "#1E90FF", linewidth = 2
      ) +
      annotate("segment",
        x = -1 - 0.5, y = freq_rel_obs[3],
        xend = -1 + 0.5, yend = freq_rel_obs[3],
        colour = "#1E90FF", linewidth = 2
      ) +
      annotate("segment",
        x = 0 - 0.5, y = freq_rel_obs[4],
        xend = 0 + 0.5, yend = freq_rel_obs[4],
        colour = "#1E90FF", linewidth = 2
      ) +
      annotate("segment",
        x = 1 - 0.5, y = freq_rel_obs[5],
        xend = 1 + 0.5, yend = freq_rel_obs[5],
        colour = "#1E90FF", linewidth = 2
      ) +
      annotate("segment",
        x = 2 - 0.5, y = freq_rel_obs[6],
        xend = 2 + 0.5, yend = freq_rel_obs[6],
        colour = "#1E90FF", linewidth = 2
      ) +
      annotate("segment",
        x = 3 - 0.5, y = freq_rel_obs[7],
        xend = 3 + 0.5, yend = freq_rel_obs[7],
        colour = "#1E90FF", linewidth = 2
      ) +
      # Dummy layer to add legend for observed data:
      geom_point(
        data = data.frame(x = Inf, y = Inf),
        aes(x = .data$x, y = .data$y, colour = "observed"),
        size = 2
      ) +
      labs(x = "Goal difference", y = "Posterior predictive distribution") +
      scale_colour_manual(
        name = "",
        values = c(observed = "#1E90FF", simulated = "#FFA500"),
        labels = c("Observed", "Simulated")
      ) +
      scale_x_continuous(breaks = -3:3, limits = c(-4, 4)) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 19),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.position = "top",
        legend.text = element_text(size = 15)
      )


    p_value <- sapply(seq_along(esiti_short), function(j) {
      round(sum(frame$rel[frame$valori == esiti_short[j]] >= freq_rel_obs[j]) / M, 3)
    })

    tbl <- data.frame(
      `goal diff.` = esiti_short,
      `Bayesian p-value` = p_value,
      check.names = FALSE
    )

    return(list(pp_plot = p, pp_table = tbl))
  } else if (type == "matches") {
    scd <- as.numeric(as.vector(goal_diff))[1:ngames_train]
    scd_sims <- goal_diff_rep
    scd_hat <- matrixStats::colMedians(scd_sims)
    scd_se <- sqrt(matrixStats::colVars(scd_sims))
    alpha <- coverage
    scd_ub <- matrixStats::colQuantiles(scd_sims, probs = 1 - (1 - alpha) / 2)
    scd_lb <- matrixStats::colQuantiles(scd_sims, probs = (1 - alpha) / 2)
    ci_alpha <- sum(scd < scd_ub & scd_lb < scd) / ngames_train
    ngames_train_draw <- sum(scd == 0)
    scd_draw <- scd[scd == 0]
    ci95_draw <- sum(scd_draw < scd_ub[scd == 0] & scd_lb[scd == 0] < scd_draw) / ngames_train_draw


    sort_scd <- scd[order(scd)]
    sort_scd_hat <- scd_hat[order(scd)]
    sort_scd_se <- scd_se[order(scd)]
    sort_scd_ub <- scd_ub[order(scd)]
    sort_scd_lb <- scd_lb[order(scd)]

    df <- data.frame(list(
      scd = sort_scd, scd_hat = sort_scd_hat, scd_se = sort_scd_se,
      scd_ub = sort_scd_ub, scd_lb = sort_scd_lb
    ))

    p <- ggplot(df, aes(x = c(1:ngames_train))) +
      geom_ribbon(aes(ymin = scd_lb, ymax = scd_ub),
        fill = "#FFA500"
      ) +
      geom_line(aes(y = scd_hat, colour = "simulated")) +
      geom_point(aes(y = scd, colour = "observed"), fill = "#1E90FF", size = 0.5) +
      scale_x_continuous(name = "games") +
      scale_y_continuous(
        name = "Goal difference",
        breaks = c(-8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8),
        sec.axis = dup_axis()
      ) +
      scale_colour_manual(
        name = "",
        values = c(observed = "#1E90FF", simulated = "#FFA500"),
        labels = c("Observed", "Simulated")
      ) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 19),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        legend.position = "top",
        legend.text = element_text(size = 15)
      )

    tbl <- data.frame(alpha = coverage, coverage = round(ci_alpha, 3))
    colnames(tbl) <- c("1-alpha", "emp. coverage")
    return(list(pp_plot = p, pp_table = tbl))
  }
}

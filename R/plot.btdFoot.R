#' Plot Method for btdFoot Objects
#'
#' Visualizes team rankings or posterior distributions using different plots based on the specified \code{plot_type}.
#'
#' - **Ranking Plots**:
#'   - **Dynamic Ranking (`plot_type = "rankings"`)**: Plots Rank Points over Periods for each team with lines and dots.
#'   - **Static Ranking (`plot_type = "rankings"`)**: Plots Rank Points on the x-axis against Team Names on the y-axis with horizontal lines and dots.
#' - **Posterior Boxplots**:
#'   - **Dynamic Ranking (`plot_type = "posterior"`)**: Faceted boxplots of posterior log-strengths by team and period.
#'   - **Static Ranking (`plot_type = "posterior"`)**: Boxplots of posterior log-strengths for each team.
#'
#' @param x An object of class \code{btdFoot}.
#' @param plot_type A character string specifying the type of plot. Options are:
#'   - \code{"rankings"}: Visualizes team rankings.
#'   - \code{"posterior"}: Visualizes posterior distributions of team strengths.
#' @param teams_of_interest Optional. A character vector of team names to include in the posterior boxplots. If \code{NULL}, all teams are included.
#' @param ... Additional arguments passed to \code{geom_line()}, \code{geom_point()}, \code{geom_segment()}, and \code{geom_boxplot()} for customization (e.g., \code{size}, \code{alpha}, \code{color}).
#'
#' @method plot btdFoot
#' @import ggplot2
#' @importFrom rstan extract
#' @export



plot.btdFoot <- function(x, plot_type = "rankings", teams_of_interest = NULL, ...) {
  # Check if the object is of class 'btdFoot'
  if (!inherits(x, "btdFoot")) {
    stop("Object must be of class 'btdFoot'.")
  }

  # Ensure that 'ggplot2' is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.")
  }

  # Check 'plot_type' argument
  allowed_plot_types <- c("rankings", "posterior")
  plot_type <- match.arg(plot_type, choices = allowed_plot_types)


  # Determine if the ranking is dynamic or static
  is_dynamic <- ifelse(max(x$rank$periods, na.rm = TRUE) > 1, TRUE, FALSE)

  if (plot_type == "rankings") {
    if (is_dynamic) {
      # Dynamic Ranking Plot: Rank Points over Periods for each team
      p <- ggplot2::ggplot(x$rank, ggplot2::aes(x = periods, y = rank_points, color = team)) +
        ggplot2::geom_line(...) +
        ggplot2::geom_point(...) +
        ggplot2::labs(
          x = "Periods",
          y = "Log-Strength Values"
        ) +
        ggplot2::theme_light() +
        ggplot2::theme(
          legend.position = "right",
          legend.title = ggplot2::element_text(size = 10),
          plot.title = ggplot2::element_text(hjust = 0.5)
        ) +
        ggplot2::guides(
          color = ggplot2::guide_legend(title = "Team")
        )
    } else {
      # Static Ranking Plot: Horizontal lines from x=0 to x=rank_points for each team, with a dot at the end
      p <- ggplot2::ggplot(x$rank, ggplot2::aes(y = stats::reorder(team, rank_points), x = rank_points)) +
        ggplot2::geom_segment(
          ggplot2::aes(x = 0, xend = rank_points, yend = team),
          color = "deepskyblue4",
          size = 1,
          ...
        ) +
        ggplot2::geom_point(
          color = "firebrick4",
          size = 3,
          ...
        ) +
        ggplot2::labs(
          x = "Log-Strength Values",
          y = "Teams"
        ) +
        ggplot2::theme_light() +
        ggplot2::theme(
          legend.position = "none",
          plot.title = ggplot2::element_text(hjust = 0.5)
        )
    }
  } else if (plot_type == "posterior") {

    # Ensure that 'rstan' is available
    if (!requireNamespace("rstan", quietly = TRUE)) {
      stop("Package 'rstan' is required for extracting posterior samples.")
    }

    BTDparameters <- rstan::extract(x$fit)

    teams <- unique(c(x$data$home_team, x$data$away_team))
    nteams <- length(teams)

    # Define teams_of_interest if not provided
    if (is.null(teams_of_interest)) {
      teams_of_interest <- teams
    } else {
      if (!all(teams_of_interest %in% teams)) {
        stop("Some teams in 'teams_of_interest' are not present in the data.")
      }
    }

    boxplot_df <- data.frame()

    # Determine number of periods
    if (is_dynamic) {
      ntimes_rank <- max(x$rank$periods, na.rm = TRUE)
    } else {
      ntimes_rank <- 1
    }

    # Create the dataframe for boxplot
    for (team in teams_of_interest) {
      team_index <- which(teams == team)

      # Extract posterior samples for each period
      for (k in 1:ntimes_rank) {
        if (is_dynamic) {
          posterior_values <- BTDparameters[["psi"]][, team_index, k]
        } else {
          posterior_values <- BTDparameters[["psi"]][, team_index]
        }

        df <- data.frame(
          period = k,
          log_strength = posterior_values,
          team = team,
          stringsAsFactors = FALSE
        )
        boxplot_df <- rbind(boxplot_df, df)
      }
    }

    if (is_dynamic) {
      # Dynamic Posterior Boxplot: Faceted by team and period
      facetBoxplot <- ggplot2::ggplot(boxplot_df, ggplot2::aes(x = as.factor(period), y = log_strength)) +
        ggplot2::geom_boxplot(aes(fill = as.factor(period)), ...) +
        ggplot2::facet_wrap(~ team, scales = "free_x", ncol = 8) +
        ggplot2::labs(
          x = "Period",
          y = "Log-Strength Values",
          fill = "Period"
        ) +
        ggplot2::theme_light() +
        ggplot2::theme(
          strip.text = ggplot2::element_text(size = 15),
          axis.text.x = ggplot2::element_text(angle = 0, hjust = 0)
        )
    } else {
      # Static Posterior Boxplot: Boxplot for each team
      facetBoxplot <- ggplot2::ggplot(boxplot_df, ggplot2::aes(x = as.factor(1), y = log_strength)) +
        ggplot2::geom_boxplot(aes(fill = team), ...) +
        ggplot2::facet_wrap(~ team, scales = "free_x", ncol = 4) +
        ggplot2::labs(
          x = "",
          y = "Log-Strength Values",
          fill = "Team"
        ) +
        ggplot2::theme_light() +
        ggplot2::theme(
          strip.text = ggplot2::element_text(size = 15),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        )
    }

    p <- facetBoxplot
  }

  print(p)

  invisible(p)
}


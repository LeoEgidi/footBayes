#' Plot Rankings for btdFoot Objects
#'
#' Visualizes team rankings based on whether the ranking is dynamic or static.
#'
#'\itemize{
#' \item Dynamic Ranking: Plots Rank Points over Periods for each team with lines and points.
#' \item Static Ranking: Plots Rank Points on the x-axis against Team Names on the y-axis with horizontal lines and points.
#'}
#'
#' @param x An object of class \code{btdFoot}.
#' @param teams_of_interest Optional. A character vector of team names to include in the rankings plot. If \code{NULL}, all teams are included.
#' @param ncol Optional. An integer specifying the number of columns in the facet wrap for dynamic ranking plots. Defaults to \code{8}.
#' @param ... Additional arguments passed to \code{geom_line()}, \code{geom_point()}, and \code{geom_segment()} for customization (e.g., \code{size}, \code{alpha}, \code{color}).
#'
#' @return A ggplot object representing the rankings plot.
#'
#' @author Roberto Macrì Demartino \email{roberto.macridemartino@phd.unipd.it}.
#'
#' @import ggplot2
#' @export
plot_logStrength <- function(x, teams_of_interest = NULL, ncol = 8, ...) {
  # Check if the object is of class 'btdFoot'
  if (!inherits(x, "btdFoot")) {
    stop("Object must be of class 'btdFoot'.")
  }

  # Ensure that 'ggplot2' is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.")
  }

  # Determine if the ranking is dynamic or static
  is_dynamic <- ifelse(max(x$rank$periods, na.rm = TRUE) > 1, TRUE, FALSE)

  # Handle teams_of_interest
  if (!is.null(teams_of_interest)) {
    # Check that specified teams exist
    available_teams <- unique(c(x$data$home_team, x$data$away_team))
    missing_teams <- setdiff(teams_of_interest, available_teams)
    if (length(missing_teams) > 0) {
      stop(paste0("The following teams are not present in the data: ", paste(missing_teams, collapse = ", ")))
    }

    x$rank <- x$rank[x$rank$team %in% teams_of_interest, ]
  }

  if (is_dynamic) {
    # Dynamic Ranking Plot: Rank Points over Periods for each team
    p <- ggplot(x$rank, aes(x = periods, y = rank_points, color = team)) +
      geom_line(...) +
      geom_point(...) +
      labs(
        x = "Periods",
        y = "Log-Strength Values"
      ) +
      theme_bw() +
      theme(
        legend.position = "right",
        legend.title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)
      ) +
      guides(
        color = guide_legend(title = "Team")
      )
  } else {
    # Static Ranking Plot: Horizontal lines from x=0 to x=rank_points for each team, with a dot at the end
    p <- ggplot(x$rank, aes(y = stats::reorder(team, rank_points), x = rank_points)) +
      geom_segment(
        aes(x = 0, xend = rank_points, yend = team),
        color = "deepskyblue4",
        size = 1,
        ...
      ) +
      geom_point(
        color = "firebrick4",
        size = 3,
        ...
      ) +
      labs(
        x = "Log-Strength Values",
        y = "Teams"
      ) +
      theme_bw() +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)
      )
  }

  print(p)
  invisible(p)
}


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
#' @param teams An optional character vector specifying team names to include in the rankings plot. If \code{NULL}, all teams are included.
#' @param ... Additional arguments passed to \code{geom_line()}, \code{geom_point()}, and \code{geom_segment()} for customization (e.g., \code{size}, \code{alpha}, \code{color}).
#'
#' @return A ggplot object representing the rankings plot.
#'
#' @author Roberto Macrì Demartino \email{roberto.macridemartino@phd.unipd.it}.
#'
#'
#' @examples
#'
#' \dontrun{
#' library(dplyr)
#'
#' data("italy")
#'
#'italy_2020_2021_rank <- italy %>%
#'  select(Season, home, visitor, hgoal, vgoal) %>%
#'  filter(Season == "2020" | Season == "2021") %>%
#'  mutate(match_outcome = case_when(
#'    hgoal > vgoal ~ 1,        # Home team wins
#'    hgoal == vgoal ~ 2,       # Draw
#'    hgoal < vgoal ~ 3         # Away team wins
#'  )) %>%
#'  mutate(periods = case_when(
#'    row_number() <= 190 ~ 1,
#'    row_number() <= 380 ~ 2,
#'    row_number() <= 570 ~ 3,
#'    TRUE ~ 4
#'  )) %>%  # Assign periods based on match number
#'  select(periods, home_team = home,
#'                away_team = visitor, match_outcome)
#'
#'fit_rank_dyn <- btd_foot(
#'  data = italy_2020_2021_rank,
#'  dynamic_rank = TRUE,
#'  rank_measure = "median",
#'  iter = 1000,
#'  cores = 2,
#'  chains = 2)
#'
#'plot_logStrength(fit_rank_dyn)
#'
#'plot_logStrength(fit_rank_dyn, teams = c("AC Milan", "AS Roma", "Juventus", "Inter"))
#'
#'
#' }
#' @import ggplot2
#' @importFrom rlang .data
#' @export
plot_logStrength <- function(x, teams = NULL, ...) {
  # Check if the object is of class 'btdFoot'
  if (!inherits(x, "btdFoot")) {
    stop("Object must be of class 'btdFoot'.")
  }

  # # Ensure that 'ggplot2' is available
  # if (!requireNamespace("ggplot2", quietly = TRUE)) {
  #   stop("Package 'ggplot2' is required for plotting.")
  # }

  # Determine if the ranking is dynamic or static
  is_dynamic <- ifelse(max(x$rank$periods, na.rm = TRUE) > 1, TRUE, FALSE)

  # Handle teams
  if (!is.null(teams)) {
    # Check that specified teams exist
    available_teams <- unique(c(x$data$home_team, x$data$away_team))
    missing_teams <- setdiff(teams, available_teams)
    if (length(missing_teams) > 0) {
      stop(paste0("The following teams are not present in the data: ", paste(missing_teams, collapse = ", ")))
    }

    x$rank <- x$rank[x$rank$team %in% teams, ]
  }

  if (is_dynamic) {
    # Dynamic Ranking Plot
    p <- ggplot(x$rank, aes(x = .data$periods, y = .data$log_strengths, color = .data$team)) +
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
    # Static Ranking Plot
    p <- ggplot(x$rank, aes(y = stats::reorder(.data$team, .data$log_strengths), x = .data$log_strengths)) +
      geom_segment(
        aes(x = 0, xend = .data$log_strengths, yend = .data$team),
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


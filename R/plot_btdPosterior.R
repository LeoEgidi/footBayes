#' Plot Posterior Distributions for btdFoot Objects
#'
#' Visualizes posterior distributions of team strengths with customizable facets.
#'
#'\itemize{
#' \item Dynamic Ranking: Faceted boxplots of posterior log-strengths by team and period.
#' \item Static Ranking: Faceted boxplots of posterior log-strengths for each team.
#'
#'}
#'
#' @param x An object of class \code{btdFoot}.
#' @param teams An optional character vector specifying team names to include in the posterior boxplots. If \code{NULL}, all teams are included.
#' @param ncol An optional integer specifying the number of columns in the facet wrap. Defaults:
#' \itemize{
#'   \item Dynamic Ranking: \code{ncol = 8}.
#'   \item Static Ranking: \code{ncol = 4}.
#'}
#' @param scales An optional character string specifying the scales for the facets. Options:
#' \itemize{
#'   \item Dynamic Ranking: \code{"free_x"}, \code{"free_y"}, \code{"free"}.
#'   \item Static Ranking: \code{"free_y"}, \code{"fixed"}.
#'}
#'  Defaults:
#' \itemize{
#'   \item Dynamic Ranking: \code{"free_x"}.
#'   \item Static Ranking: \code{"free_x"}.
#'}
#' @param ... Additional arguments passed to \code{geom_boxplot()} for customization (e.g., \code{size}, \code{alpha}, \code{color}).
#'
#' @return A ggplot object representing the posterior distributions plot.
#'
#' @author Roberto Macrì Demartino \email{roberto.macridemartino@phd.unipd.it}.
#'
#' @examples
#'
#' \dontrun{
#' require(dplyr)
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
#'plot_btdPosterior(fit_rank_dyn)
#'
#'plot_btdPosterior(fit_rank_dyn, teams = c("AC Milan", "AS Roma", "Juventus", "Inter"), ncol = 2)
#'
#'
#' }
#' @import ggplot2
#' @export

plot_btdPosterior <- function(x, teams = NULL, ncol = NULL, scales = NULL, ...) {
  # Check if the object is of class 'btdFoot'
  if (!inherits(x, "btdFoot")) {
    stop("Object must be of class 'btdFoot'.")
  }

  # # Ensure that required packages are available
  # if (!requireNamespace("ggplot2", quietly = TRUE)) {
  #   stop("Package 'ggplot2' is required for plotting.")
  # }
  # if (!requireNamespace("rstan", quietly = TRUE)) {
  #   stop("Package 'rstan' is required for extracting posterior samples.")
  # }

  # Extract parameters
  BTDparameters <- rstan::extract(x$fit)

  # Get team names
  teams_all <- unique(c(x$data$home_team, x$data$away_team))

  # Define teams if not provided
  if (is.null(teams)) {
    teams <- teams_all
  } else {
    # Validate that provided teams exist
    if (!all(teams %in% teams_all)) {
      stop("Some teams in 'teams' are not present in the data.")
    }
  }

  boxplot_df <- data.frame()

  # Determine if the ranking is dynamic or static
  is_dynamic <- ifelse(max(x$rank$periods, na.rm = TRUE) > 1, TRUE, FALSE)

  # Determine number of periods
  if (is_dynamic) {
    ntimes_rank <- max(x$rank$periods, na.rm = TRUE)
  } else {
    ntimes_rank <- 1
  }

  # Create the dataframe for boxplot using vectorized operations
  for (team in teams) {
    team_index <- which(teams_all == team)

    for (k in 1:ntimes_rank) {
      if (is_dynamic) {
        if (length(dim(BTDparameters[["logStrength"]])) < 3) {
          stop("Expected 'logStrength' to have three dimensions for dynamic ranking.")
        }
        posterior_values <- BTDparameters[["logStrength"]][, k, team_index]
      } else {
        if (length(dim(BTDparameters[["logStrength"]])) == 2) {
          posterior_values <- BTDparameters[["logStrength"]][, team_index]
        } else {
          stop("Unexpected dimensions for 'logStrength' parameter.")
        }
      }

      df <- data.frame(
        period = if (is_dynamic) k else 1,
        log_strength = posterior_values,
        team = team,
        stringsAsFactors = FALSE
      )
      boxplot_df <- rbind(boxplot_df, df)
    }
  }

  # Set default values if not provided
  if (is_dynamic) {
    if (is.null(ncol)) ncol <- 8
    if (is.null(scales)) scales <- "free_x"
  } else {
    if (is.null(ncol)) ncol <- 4
    if (is.null(scales)) scales <- "free_x"
  }

  if (is_dynamic) {
    # Dynamic Posterior Boxplot: Faceted by team and period
    p <- ggplot(boxplot_df, aes(x = as.factor(period), y = log_strength)) +
      geom_boxplot(aes(fill = as.factor(period)), ...) +
      facet_wrap(~ team, scales = scales, ncol = ncol) +
      labs(
        x = "Periods",
        y = "Log-Strength Values",
        fill = "Periods"
      ) +
      theme_bw() +
      theme(
        strip.text = element_text(size = 12, color = "black"),
        legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0)
      )
  } else {
    # Static Posterior Boxplot: Boxplot for each team
    p <- ggplot(boxplot_df, aes(x = "", y = log_strength)) +
      geom_boxplot(aes(fill = team), ...) +
      facet_wrap(~ team, scales = scales, ncol = ncol) +
      labs(
        x = "",
        y = "Log-Strength Values",
        fill = "Team"
      ) +
      theme_bw() +
      theme(
        strip.text = element_text(size = 12, color = "black"),
        legend.position = "none",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  }

  # Print and return the plot
  print(p)
  invisible(p)
}



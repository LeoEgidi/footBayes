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
#' @param teams_of_interest Optional. A character vector of team names to include in the posterior boxplots. If \code{NULL}, all teams are included.
#' @param ncol Optional. An integer specifying the number of columns in the facet wrap. Defaults:
#' \itemize{
#'   \item Dynamic Ranking: \code{ncol = 8}.
#'   \item Static Ranking: \code{ncol = 4}.
#'}
#' @param scales Optional. A character string specifying the scales for the facets. Options:
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
#'
#' @export

plot_btdPosterior <- function(x, teams_of_interest = NULL, ncol = NULL, scales = NULL, ...) {
  # Check if the object is of class 'btdFoot'
  if (!inherits(x, "btdFoot")) {
    stop("Object must be of class 'btdFoot'.")
  }

  # Ensure that required packages are available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting.")
  }
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Package 'rstan' is required for extracting posterior samples.")
  }

  # Extract parameters
  BTDparameters <- rstan::extract(x$fit)

  # Get team names
  teams <- unique(c(x$data$home_team, x$data$away_team))

  # Define teams_of_interest if not provided
  if (is.null(teams_of_interest)) {
    teams_of_interest <- teams
  } else {
    # Validate that provided teams exist
    if (!all(teams_of_interest %in% teams)) {
      stop("Some teams in 'teams_of_interest' are not present in the data.")
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
  for (team in teams_of_interest) {
    team_index <- which(teams == team)

    for (k in 1:ntimes_rank) {
      if (is_dynamic) {
        if (length(dim(BTDparameters[["psi"]])) < 3) {
          stop("Expected 'psi' to have three dimensions for dynamic ranking.")
        }
        posterior_values <- BTDparameters[["psi"]][, team_index, k]
      } else {
        if (length(dim(BTDparameters[["psi"]])) == 2) {
          posterior_values <- BTDparameters[["psi"]][, team_index]
        } else {
          stop("Unexpected dimensions for 'psi' parameter.")
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
    p <- ggplot2::ggplot(boxplot_df, ggplot2::aes(x = as.factor(period), y = log_strength)) +
      ggplot2::geom_boxplot(aes(fill = as.factor(period)), ...) +
      ggplot2::facet_wrap(~ team, scales = scales, ncol = ncol) +
      ggplot2::labs(
        x = "Period",
        y = "Log-Strength Values",
        fill = "Period"
      ) +
      ggplot2::theme_light() +
      ggplot2::theme(
        strip.text = ggplot2::element_text(size = 15),
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 0, hjust = 0)
      )
  } else {
    # Static Posterior Boxplot: Boxplot for each team
    p <- ggplot2::ggplot(boxplot_df, ggplot2::aes(x = "", y = log_strength)) +
      ggplot2::geom_boxplot(aes(fill = team), ...) +
      ggplot2::facet_wrap(~ team, scales = scales, ncol = ncol) +
      ggplot2::labs(
        x = "",
        y = "Log-Strength Values",
        fill = "Team"
      ) +
      ggplot2::theme_light() +
      ggplot2::theme(
        strip.text = ggplot2::element_text(size = 15),
        legend.position = "none",
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      )
  }

  # Print and return the plot
  print(p)
  invisible(p)
}



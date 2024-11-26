#' Plot Posterior Distributions for \code{btdFoot} Objects
#'
#' Plots for the posterior distributions of team log-strengths and other parameters with customizable plot types and facets.
#'
#' \itemize{
#'   \item \strong{Dynamic Ranking}: Faceted boxplots or density plots (including the 95\% credible interval) of posterior log-strengths by team and period.
#'   \item \strong{Static Ranking}: Boxplots or density plots (including the 95\% credible interval) of posterior log-strengths for each team.
#' }
#'
#' @param x An object of class \code{btdFoot}.
#' @param pars A character string specifying the parameter to plot.
#'   Choices are \code{"logStrength"}, \code{"logTie"}, and \code{"home"}.
#'   Default is \code{"logStrength"}.
#' @param plot_type A character string specifying the type of plot.
#'   Choices are \code{"boxplot"} and \code{"density"}.
#'   Default is \code{"boxplot"}.
#' @param teams An optional character vector specifying team names to include in the posterior
#'   boxplots or density plots. If \code{NULL}, all teams are included.
#' @param ncol An optional integer specifying the number of columns in the facet wrap
#'   when using a dynamic Bayesian Bradley-Terry-Davidson model.
#'   Default is \code{8}.
#' @param scales An optional character string specifying the scales for the facets
#'   when using a dynamic Bayesian Bradley-Terry-Davidson model.
#'   Options include \code{"free"}, \code{"fixed"}, \code{"free_x"}, and \code{"free_y"}.
#'   Default is \code{"free_x"}.
#' @param ... Additional arguments passed to \code{geom_boxplot()}, \code{geom_density_ridges()},
#'   or other geoms for customization (e.g., \code{size}, \code{alpha}, \code{color}).
#'
#' @return A \code{ggplot} object representing the posterior distributions plot.
#'
#' @author
#' Roberto Macrì Demartino \email{roberto.macridemartino@phd.unipd.it}.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Load example data
#' data("italy")
#'
#' # Prepare the data
#' italy_2020_2021_rank <- italy %>%
#'   select(Season, home, visitor, hgoal, vgoal) %>%
#'   filter(Season %in% c("2020", "2021")) %>%
#'   mutate(match_outcome = case_when(
#'     hgoal > vgoal ~ 1,        # Home team wins
#'     hgoal == vgoal ~ 2,       # Draw
#'     hgoal < vgoal ~ 3         # Away team wins
#'   )) %>%
#'   mutate(periods = case_when(
#'     row_number() <= 190 ~ 1,
#'     row_number() <= 380 ~ 2,
#'     row_number() <= 570 ~ 3,
#'     TRUE ~ 4
#'   )) %>%  # Assign periods based on match number
#'   select(periods, home_team = home,
#'          away_team = visitor, match_outcome)
#'
#' # Fit the Bayesian Bradley-Terry-Davidson model with dynamic ranking
#' fit_rank_dyn <- btd_foot(
#'   data = italy_2020_2021_rank,
#'   dynamic_rank = TRUE,
#'   rank_measure = "median",
#'   iter = 1000,
#'   cores = 2,
#'   chains = 2
#' )
#'
#' # Plot posterior distributions with default settings
#' plot_btdPosterior(fit_rank_dyn)
#'
#' # Plot posterior distributions for specific teams with customized facets
#' plot_btdPosterior(
#'   fit_rank_dyn,
#'   teams = c("AC Milan", "AS Roma", "Juventus", "Inter"),
#'   ncol = 2
#' )
#'
#' plot_btdPosterior(
#'   fit_rank_dyn,
#'   plot_type = "density",
#'   teams = c("AC Milan", "AS Roma", "Juventus", "Inter"),
#'   ncol = 2
#' )
#' }
#'
#' @import ggplot2
#' @importFrom rstan extract
#' @importFrom ggridges stat_density_ridges
#' @importFrom stats approx
#' @importFrom rlang .data
#' @export

plot_btdPosterior <- function(x, pars = "logStrength", plot_type = "boxplot", teams = NULL, ncol = NULL, scales = NULL, ...) {
  # Check if the object is of class 'btdFoot'
  if (!inherits(x, "btdFoot")) {
    stop("Object must be of class 'btdFoot'.")
  }

  # Validate pars
  allowed_pars <- c("logStrength", "logTie", "home")
  pars <- match.arg(pars, choices = allowed_pars)

  # Validate plot_type
  allowed_plot_types <- c("boxplot", "density")
  plot_type <- match.arg(plot_type, choices = allowed_plot_types)

  # Extract parameters
  BTDparameters <- rstan::extract(x$fit)

  if (pars == "logStrength") {
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

    data_df <- data.frame()

    # Determine if the ranking is dynamic or static
    is_dynamic <- ifelse(max(x$rank$periods, na.rm = TRUE) > 1, TRUE, FALSE)

    # Determine number of periods
    if (is_dynamic) {
      ntimes_rank <- max(x$rank$periods, na.rm = TRUE)
    } else {
      ntimes_rank <- 1
    }

    # Create the dataframe for plotting
    for (team in teams) {
      team_index <- which(teams_all == team)

      for (k in 1:ntimes_rank) {
        if (is_dynamic) {
          posterior_values <- BTDparameters[["logStrength"]][, k, team_index]
        } else {
          posterior_values <- BTDparameters[["logStrength"]][, team_index]
        }

        df <- data.frame(
          period = if (is_dynamic) k else 1,
          log_strength = posterior_values,
          team = team,
          stringsAsFactors = FALSE
        )
        data_df <- rbind(data_df, df)
      }
    }

    if (plot_type == "boxplot") {
      if (is_dynamic) {
        # Set default values if not provided
        if (is.null(ncol)) ncol <- 8
        if (is.null(scales)) scales <- "free_x"

        # Dynamic Posterior Boxplot: Faceted by team and period
        p <- ggplot(data_df, aes(x = as.factor(period), y = .data$log_strength)) +
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
            axis.text.x = element_text(angle = 0, hjust = 0.5)
          )
      } else {
        # Static Posterior Boxplot: Boxplots on the same plot with team names on x-axis
        p <- ggplot(data_df, aes(x = team, y = .data$log_strength)) +
          geom_boxplot(aes(fill = team), ...) +
          labs(
            x = "Teams",
            y = "Log-Strength Values",
            fill = "Team"
          ) +
          theme_bw() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none"
          )
      }
    } else if (plot_type == "density") {

      if (is_dynamic) {
        # Set default values if not provided
        if (is.null(ncol)) ncol <- 8
        if (is.null(scales)) scales <- "free_x"

        # Dynamic Posterior Density Plot using ggridges
        p <- ggplot(data_df, aes(x = .data$log_strength, y = as.factor(period), fill = stat(quantile))) +
          ggridges::stat_density_ridges(quantile_lines = TRUE,
                                        calc_ecdf = TRUE,
                                        geom = "density_ridges_gradient",
                                        quantiles = c(0.025, 0.975),
                                        scale = 1.2,
                                        rel_min_height = 0.01, ...) +
          facet_wrap(~ team, scales = scales, ncol = ncol) +
          labs(
            x = "Log-Strength Values",
            y = "Periods",
            fill = "Probability"
          ) +
          theme_bw() +
          theme(
            strip.text = element_text(size = 12, color = "black")
          ) +
          scale_fill_manual(name = "Prob.", values = c("#FFA500", "#1E90FF", "#FFA500"),
                            labels = c("(0, 2.5%]", "(2.5%, 97.5%]", "(97.5%, 1]")) +
          scale_y_discrete(
            expand = expansion(mult = c(0, 0.35))  # Adds 35% padding on the top
          )
      } else {
        # Static Posterior Density Plot using ggridges
        p <- ggplot(data_df, aes(x = .data$log_strength, y = team, fill = stat(quantile))) +
          ggridges::stat_density_ridges(quantile_lines = TRUE,
                                        calc_ecdf = TRUE,
                                        geom = "density_ridges_gradient",
                                        quantiles = c(0.025, 0.975),
                                        scale = 0.9,
                                        rel_min_height = 0.01,
                                        alpha = 0.4,
                                        ...) +
          labs(
            x = "Log-Strength Values",
            y = "Teams",
            fill = "Probability"
          ) +
          theme_bw() +
          scale_fill_manual(name = "Prob.", values = c("#FFA500", "#1E90FF", "#FFA500"),
                            labels = c("(0, 2.5%]", "(2.5%, 97.5%]", "(97.5%, 1]")) +
          scale_y_discrete(
            expand = expansion(mult = c(0, 0.35))  # Adds 35% padding on the top
          )
      }
    }
  } else if (pars %in% c("logTie", "home")) {
    # For 'logTie' and 'home', extract the samples and plot
    if (!pars %in% names(BTDparameters)) {
      stop(paste("Parameter", pars, "not found in the model."))
    }

    label <- if (pars == "logTie") {
      "Log-Tie Values"
    } else if (pars == "home") {
      "Home Effect Values"
    } else {
      "Values"
    }

    posterior_values <- BTDparameters[[pars]]

    df <- data.frame(
      parameter = pars,
      value = posterior_values
    )

    if (plot_type == "boxplot") {
      p <- ggplot(df, aes(x = .data$parameter, y = .data$value)) +
        geom_boxplot(...) +
        labs(
          x = "",
          y = label
        ) +
        theme_bw()
    } else if (plot_type == "density") {

      density_data <- with(density(df$value, adjust = 1), data.frame(x, y))

      # Calculate the 95% CI
      quantiles <- quantile(df$value, probs = c(0.025, 0.975))

      # Separate the data for different regions
      density_data$quantile_fill <- cut(
        density_data$x,
        breaks = c(-Inf, quantiles[1], quantiles[2], Inf),
        labels = c("Lower Quantile", "Middle Region", "Upper Quantile")
      )

      # Create the plot
      p <- ggplot(density_data, aes(x = .data$x, y = .data$y)) +
        # Add the filled regions
        geom_ribbon(aes(ymin = 0, ymax = .data$y, fill = .data$quantile_fill), alpha = 1) +
        # Add the density curve line
        geom_line(color = "black", size = 0.5) +
        # Add vertical lines for quantiles within the density range
        geom_segment(aes(x = quantiles[1], xend = quantiles[1], y = 0, yend = approx(.data$x, .data$y, xout = quantiles[1])$y),
                     color = "black", linetype = "solid", size = 0.5) +
        geom_segment(aes(x = quantiles[2], xend = quantiles[2], y = 0, yend = approx(.data$x, .data$y, xout = quantiles[2])$y),
                     color = "black", linetype = "solid", size = 0.5) +
        labs(
          x = label,
          y = "Posterior Density",
          fill = "Probability"
        ) +
        scale_fill_manual(values = c("#FFA500", "#1E90FF", "#FFA500"),
                          labels = c("(0, 2.5%]", "(2.5%, 97.5%]", "(97.5%, 1]")) +
        theme_bw()
    }
  } else {
    stop("Invalid parameter specified.")
  }

  # Print and return the plot
  print(p)
  invisible(p)
}

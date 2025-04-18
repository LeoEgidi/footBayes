#' Plot Posterior Distributions for \code{btdFoot} Objects
#'
#' Plots for the posterior distributions of team log-strengths and other parameters with customizable plot types and facets.
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
#'
#' @return
#' A \code{ggplot} object displaying:
#' \itemize{
#'   \item For \code{pars="logStrength"}:
#'     \itemize{
#'       \item Dynamic BTD: Faceted boxplots or density plots (including the 95\% credible interval) of posterior log-strengths by team and period.
#'       \item Static BTD: Boxplots or density plots (including the 95\% credible interval) of posterior log-strengths for each team.
#'     }
#'   \item For \code{pars="logTie"} or \code{pars="home"}: A single boxplot or density plot with 95\% credible interval.
#' }
#'
#' @author
#' Roberto Macrì Demartino \email{roberto.macridemartino@deams.units.it}.
#'
#' @examples
#' \dontrun{
#' if (instantiate::stan_cmdstan_exists()) {
#'   library(dplyr)
#'
#'   # Load example data
#'   data("italy")
#'
#'   # Prepare the data
#'   italy_2020_2021_rank <- italy %>%
#'     select(Season, home, visitor, hgoal, vgoal) %>%
#'     filter(Season %in% c("2020", "2021")) %>%
#'     mutate(match_outcome = case_when(
#'       hgoal > vgoal ~ 1, # Home team wins
#'       hgoal == vgoal ~ 2, # Draw
#'       hgoal < vgoal ~ 3 # Away team wins
#'     )) %>%
#'     mutate(periods = case_when(
#'       row_number() <= 190 ~ 1,
#'       row_number() <= 380 ~ 2,
#'       row_number() <= 570 ~ 3,
#'       TRUE ~ 4
#'     )) %>% # Assign periods based on match number
#'     select(periods,
#'       home_team = home,
#'       away_team = visitor, match_outcome
#'     )
#'
#'   # Fit the Bayesian Bradley-Terry-Davidson model with dynamic ranking
#'   fit_rank_dyn <- btd_foot(
#'     data = italy_2020_2021_rank,
#'     dynamic_rank = TRUE,
#'     rank_measure = "median",
#'     iter_sampling = 1000,
#'     parallel_chains = 2,
#'     chains = 2
#'   )
#'
#'   # Plot posterior distributions with default settings
#'   plot_btdPosterior(fit_rank_dyn)
#'
#'   # Plot posterior distributions for specific teams with customized facets
#'   plot_btdPosterior(
#'     fit_rank_dyn,
#'     teams = c("AC Milan", "AS Roma", "Juventus", "Inter"),
#'     ncol = 2
#'   )
#'
#'   plot_btdPosterior(
#'     fit_rank_dyn,
#'     plot_type = "density",
#'     teams = c("AC Milan", "AS Roma", "Juventus", "Inter"),
#'     ncol = 2
#'   )
#' }
#' }
#' @importFrom ggplot2 ggplot aes labs geom_boxplot facet_wrap theme_bw theme
#' element_text geom_line geom_segment geom_ribbon scale_fill_manual
#' scale_y_discrete expansion after_stat
#' @importFrom rstan extract
#' @importFrom ggridges stat_density_ridges
#' @importFrom stats approx
#' @importFrom rlang .data
#' @export

plot_btdPosterior <- function(x, pars = "logStrength", plot_type = "boxplot", teams = NULL, ncol = NULL, scales = NULL) {
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
  BTDparameters <- x$fit$draws(format = "df")

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
    is_dynamic <- ifelse(length(unique(x$rank$periods, na.rm = TRUE)) > 1, TRUE, FALSE)

    # Determine number of periods
    if (is_dynamic) {
      times_rank <- match(x$rank$periods, unique(x$rank$periods))
      ntimes_rank <- max(times_rank, na.rm = TRUE)
    } else {
      ntimes_rank <- 1
    }

    # Create the dataframe for plotting
    for (team in teams) {
      team_index <- which(teams_all == team)

      for (k in 1:ntimes_rank) {
        if (is_dynamic) {
          column_name <- paste0("logStrength[", k, ",", team_index, "]")
        } else {
          column_name <- paste0("logStrength[", team_index, "]")
        }

        posterior_values <- BTDparameters[[column_name]]

        df <- data.frame(
          period = if (is_dynamic) unique(x$rank$periods)[k] else 1,
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
        p <- ggplot(data_df, aes(x = as.factor(.data$period), y = .data$log_strength)) +
          geom_boxplot(aes(fill = as.factor(.data$period))) +
          facet_wrap(~team, scales = scales, ncol = ncol) +
          labs(
            x = "Periods",
            y = "Log-Strength Values",
            fill = "Periods"
          ) +
          scale_x_discrete(breaks = sort(unique(x$rank$periods))) +
          theme_bw() +
          theme(
            strip.text = element_text(size = 12, color = "black"),
            legend.position = "none",
            axis.text.x = element_text(angle = 0, hjust = 0.5)
          )
      } else {
        # Static Posterior Boxplot: Boxplots on the same plot with team names on x-axis
        p <- ggplot(data_df, aes(x = team, y = .data$log_strength)) +
          geom_boxplot(aes(fill = team)) +
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
        p <- ggplot(data_df, aes(x = .data$log_strength, y = as.factor(.data$period), fill = after_stat(quantile))) +
          ggridges::stat_density_ridges(
            quantile_lines = TRUE,
            calc_ecdf = TRUE,
            geom = "density_ridges_gradient",
            quantiles = c(0.025, 0.975),
            scale = 1.2,
            rel_min_height = 0.01
          ) +
          facet_wrap(~team, scales = scales, ncol = ncol) +
          labs(
            x = "Log-Strength Values",
            y = "Periods",
            fill = "Probability"
          ) +
          theme_bw() +
          theme(
            strip.text = element_text(size = 12, color = "black"),
            legend.position = "top"
          ) +
          scale_fill_manual(
            name = "Prob.", values = c("#FFA500", "#1E90FF", "#FFA500"),
            labels = c("(0, 2.5%]", "(2.5%, 97.5%]", "(97.5%, 1]")
          ) +
          scale_y_discrete(
            breaks = sort(unique(x$rank$periods)),
            expand = expansion(mult = c(0, 0.35)) # Adds 35% padding on the top
          )
      } else {
        # Static Posterior Density Plot using ggridges
        p <- ggplot(data_df, aes(x = .data$log_strength, y = team, fill = after_stat(quantile))) +
          ggridges::stat_density_ridges(
            quantile_lines = TRUE,
            calc_ecdf = TRUE,
            geom = "density_ridges_gradient",
            quantiles = c(0.025, 0.975),
            scale = 0.9,
            rel_min_height = 0.01,
            alpha = 0.4,
          ) +
          labs(
            x = "Log-Strength Values",
            y = "Teams",
            fill = "Probability"
          ) +
          theme_bw() +
          theme(legend.position = "top") +
          scale_fill_manual(
            name = "Prob.", values = c("#FFA500", "#1E90FF", "#FFA500"),
            labels = c("(0, 2.5%]", "(2.5%, 97.5%]", "(97.5%, 1]")
          ) +
          scale_y_discrete(
            expand = expansion(mult = c(0, 0.35)) # Adds 35% padding on the top
          )
      }
    }
  } else if (pars %in% c("logTie", "home")) {
    # For 'logTie' and 'home', extract the samples and plot
    if (pars %in% "home" && x$stan_data$ind_home == 0) {
      stop(paste("Parameter 'home' not found in the model."))
    }

    label <- if (pars == "logTie") {
      "Log-Tie Values"
    } else {
      "Home Effect Values"
    }

    posterior_values <- BTDparameters[[pars]]

    df <- data.frame(
      parameter = pars,
      value = posterior_values
    )

    if (plot_type == "boxplot") {
      p <- ggplot(df, aes(x = .data$parameter, y = .data$value)) +
        geom_boxplot() +
        labs(
          x = "",
          y = label
        ) +
        theme_bw()
    } else if (plot_type == "density") {
      density_data <- with(density(df$value, adjust = 1), data.frame(x, y))

      # Calculate the 95% CI
      quantiles <- quantile(df$value, probs = c(0.025, 0.975))

      # Get y-values for quantiles
      yend1 <- approx(density_data$x, density_data$y, xout = quantiles[1])$y
      yend2 <- approx(density_data$x, density_data$y, xout = quantiles[2])$y

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
        geom_line(color = "black", linewidth = 0.5) +
        # Add vertical lines for quantiles within the density range
        annotate("segment",
          x = quantiles[1], xend = quantiles[1],
          y = 0, yend = yend1,
          color = "black", linetype = "solid", linewidth = 0.5
        ) +
        annotate("segment",
          x = quantiles[2], xend = quantiles[2],
          y = 0, yend = yend2,
          color = "black", linetype = "solid", linewidth = 0.5
        ) +
        labs(
          x = label,
          y = "Posterior Density",
          fill = "Probability"
        ) +
        scale_fill_manual(
          values = c("#FFA500", "#1E90FF", "#FFA500"),
          labels = c("(0, 2.5%]", "(2.5%, 97.5%]", "(97.5%, 1]")
        ) +
        theme_bw()
    }
  }

  # Print and return the plot
  return(p)
}

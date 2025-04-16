#' Plot football abilities from Stan and MLE models
#'
#' Depicts teams' abilities either from the Stan models fitted via the \code{stan_foot} function
#' or from MLE models fitted via the \code{mle_foot} function.
#'
#'
#' @param object An object either of class \code{stanFoot}, \code{CmdStanFit}, \code{stanfit}, or class
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
#' @param type Type of ability in Poisson models: one among \code{"defense"}, \code{"attack"} or \code{"both"}. Default is \code{"both"}.
#' @param teams  An optional character vector specifying team names to include. If \code{NULL}, all teams are included.
#'
#' @return
#'
#' Abilities plots for the selected teams.
#'
#' @author Leonardo Egidi \email{legidi@units.it} and Roberto Macrì Demartino \email{roberto.macridemartino@deams.units.it}.
#'
#' @examples
#' \dontrun{
#' if (instantiate::stan_cmdstan_exists()) {
#'   library(dplyr)
#'
#'   data("italy")
#'   italy <- as_tibble(italy)
#'
#'   ### no dynamics, no prediction
#'
#'   italy_2000_2002 <- italy %>%
#'     dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
#'     dplyr::filter(Season == "2000" | Season == "2001" | Season == "2002")
#'
#'   colnames(italy_2000_2002) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")
#'
#'   fit1 <- stan_foot(
#'     data = italy_2000_2002,
#'     model = "double_pois"
#'   ) # double poisson
#'
#'   fit2 <- stan_foot(
#'     data = italy_2000_2002,
#'     model = "biv_pois"
#'   ) # bivariate poisson
#'
#'   fit3 <- stan_foot(
#'     data = italy_2000_2002,
#'     model = "skellam"
#'   ) # skellam
#'
#'   fit4 <- stan_foot(
#'     data = italy_2000_2002,
#'     model = "student_t"
#'   ) # student_t
#'
#'   foot_abilities(fit1, italy_2000_2002)
#'   foot_abilities(fit2, italy_2000_2002)
#'   foot_abilities(fit3, italy_2000_2002)
#'   foot_abilities(fit4, italy_2000_2002)
#'
#'   ### seasonal dynamics, predict the last season
#'
#'   fit5 <- stan_foot(
#'     data = italy_2000_2002,
#'     model = "biv_pois",
#'     predict = 180,
#'     dynamic_type = "seasonal"
#'   ) # bivariate poisson
#'   foot_abilities(fit5, italy_2000_2002)
#' }
#' }
#' @importFrom ggplot2 ggplot geom_ribbon geom_line facet_wrap lims scale_x_continuous labs
#'   theme_bw theme scale_color_manual geom_errorbarh geom_vline position_dodge
#' @importFrom rstan extract
#' @importFrom stats median quantile
#' @importFrom reshape2 melt
#' @importFrom dplyr group_by summarise
#' @importFrom posterior as_draws_rvars draws_of
#' @importFrom rlang .data
#' @export



foot_abilities <- function(object, data,
                           type = "both",
                           teams = NULL) {
  #   ____________________________________________________________________________
  #   Data and arguments checks                                               ####

  match.arg(type, c("attack", "defense", "both"))

  required_cols <- c("periods", "home_team", "away_team", "home_goals", "away_goals")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste("data is missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  teams_all <- unique(c(data$home_team, data$away_team))
  periods <- unique(data$periods)

  if (!is.character(teams_all)) {
    teams_all <- as.character(teams_all)
  }

  if (inherits(object, c("stanFoot", "stanfit", "CmdStanFit"))) {
    # For stanFoot or CmdStanFit, extract draws as a data frame
    if (inherits(object, "stanFoot") || inherits(object, "CmdStanFit")) {
      if (inherits(object, "stanFoot")) {
        # For stanFoot objects, the draws are available in object$fit
        draws <- object$fit$draws()
      } else {
        # For CmdStanFit objects, draw directly from object
        draws <- object$draws()
      }

      draws <- posterior::as_draws_rvars(draws)
      sims <- list()
      if ("att" %in% names(draws) && "def" %in% names(draws)) {
        sims$att <- posterior::draws_of(draws[["att"]])
        sims$def <- posterior::draws_of(draws[["def"]])
      } else if ("ability" %in% names(draws)) {
        sims$ability <- posterior::draws_of(draws[["ability"]])
      } else {
        stop("Model does not contain the 'att' and 'def' parameters or the 'ability' parameter in its samples.")
      }
    } else {
      # Extract posterior samples
      sims <- rstan::extract(object)

      # Check that the extracted samples include the required parameters
      if (!(("att" %in% names(sims) && "def" %in% names(sims)) || ("ability" %in% names(sims)))) {
        stop("Model does not contain the 'att' and 'def' parameters or the 'ability' parameter in its samples.")
      }
    }

    att <- sims$att
    def <- sims$def

    if (is.null(teams)) {
      sel_teams <- teams_all
    } else {
      sel_teams <- teams_all[match(teams, teams_all)]
    }
    sel_teams_index <- match(sel_teams, teams_all)

    if (is.na(sum(sel_teams_index))) {
      stop("Select only valid teams' names!")
    }


    #   ____________________________________________________________________________
    #   Dataframes and plots                                                    ####


    num_teams <- length(unique(sel_teams))

    # Calculate the number of rows and columns
    nrow_plot <- ceiling(sqrt(num_teams)) # Approximate square root for rows
    ncol_plot <- ceiling(num_teams / nrow_plot) # Determine columns to fit all teams


    if (length(dim(att)) == 3) {
      T <- dim(att)[2]
      nteams <- dim(att)[3]
      att_med <- apply(att, c(2, 3), stats::median)
      def_med <- apply(def, c(2, 3), stats::median)
      att_025 <- apply(att, c(2, 3), function(x) stats::quantile(x, 0.025))
      att_25 <- apply(att, c(2, 3), function(x) stats::quantile(x, 0.25))
      att_75 <- apply(att, c(2, 3), function(x) stats::quantile(x, 0.75))
      att_975 <- apply(att, c(2, 3), function(x) stats::quantile(x, 0.975))
      def_025 <- apply(def, c(2, 3), function(x) stats::quantile(x, 0.025))
      def_25 <- apply(def, c(2, 3), function(x) stats::quantile(x, 0.25))
      def_75 <- apply(def, c(2, 3), function(x) stats::quantile(x, 0.75))
      def_975 <- apply(def, c(2, 3), function(x) stats::quantile(x, 0.975))

      valid_teams <- match(sel_teams, unique(c(data$home_team, data$away_team)))

      mt_att_025 <- reshape2::melt(att_025[, valid_teams])
      mt_att_25 <- reshape2::melt(att_25[, valid_teams])
      mt_att_50 <- reshape2::melt(att_med[, valid_teams])
      mt_att_75 <- reshape2::melt(att_75[, valid_teams])
      mt_att_975 <- reshape2::melt(att_975[, valid_teams])

      mt_def_025 <- reshape2::melt(def_025[, valid_teams])
      mt_def_25 <- reshape2::melt(def_25[, valid_teams])
      mt_def_50 <- reshape2::melt(def_med[, valid_teams])
      mt_def_75 <- reshape2::melt(def_75[, valid_teams])
      mt_def_975 <- reshape2::melt(def_975[, valid_teams])

      teams_fac_rep <- rep(sel_teams, each = T)
      times_rep <- rep(1:T, length(sel_teams))

      att_data <- data.frame(
        teams = teams_fac_rep,
        times = times_rep,
        mid = mt_att_50$value,
        lo = mt_att_25$value,
        hi = mt_att_75$value
      )

      def_data <- data.frame(
        teams = teams_fac_rep,
        times = times_rep,
        mid = mt_def_50$value,
        lo = mt_def_25$value,
        hi = mt_def_75$value
      )

      if (length(unique(data$periods)) == 1) {
        timings <- 1:dim(sims$att)[2]
        sp <- length(timings) %/% 5
        timings_breaks <- timings[sp * c(1:5)]
      } else {
        timings <- unique(data$periods)
        timings_breaks <- timings
      }
      if (type == "both") {
        position_lookup <-
          att_data %>%
          group_by(teams) %>%
          summarise(pos = first(teams))
        label_w_position <- function(team_name) {
          paste0(team_name, " (", with(position_lookup, pos[teams == player_name]), ")")
        }
        p <- ggplot() +
          geom_ribbon(
            aes(x = times, ymin = lo, ymax = hi),
            data = att_data,
            fill = "#D3D3D3"
          ) +
          geom_ribbon(
            aes(x = times, ymin = lo, ymax = hi),
            data = def_data,
            fill = "#D3D3D3"
          ) +
          geom_line(
            aes(x = times, y = mid, colour = "attack"),
            data = att_data,
            linewidth = 1
          ) +
          geom_line(
            aes(x = times, y = mid, colour = "defense"),
            data = def_data,
            linewidth = 1
          ) +
          facet_wrap("teams", scales = "free", ncol = ncol_plot, nrow = nrow_plot) +
          lims(y = c(min(att_25 - 0.3), max(att_75 + 0.3))) +
          scale_x_discrete(limits = factor(timings), breaks = timings_breaks) +
          labs(
            x = "Periods", y = "Teams' effects",
            title = "Attack and defense effects (50% posterior bars)"
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(size = 16),
            strip.text = element_text(size = 12, color = "black"),
            axis.text.x = element_text(color = "black", size = 11),
            axis.text.y = element_text(size = 11),
            plot.subtitle = element_text(size = 12),
            legend.position = "top",
            legend.text = element_text(size = 15)
          ) +
          scale_color_manual(
            name = "",
            values = c(
              defense = "#377EB8",
              attack = "#cd1719"
            ),
            labels = c("Attack Effects", "Defense Effects")
          )
        return(p)
      } else if (type == "attack") {
        position_lookup <-
          att_data %>%
          group_by(teams) %>%
          summarise(pos = first(teams))

        label_w_position <- function(team_name) {
          paste0(team_name, " (", with(position_lookup, pos[teams == player_name]), ")")
        }

        p <- ggplot() +
          geom_ribbon(
            aes(x = times, ymin = lo, ymax = hi),
            data = att_data,
            fill = "#D3D3D3"
          ) +
          geom_line(
            aes(x = times, y = mid, colour = "attack"),
            data = att_data,
            linewidth = 1
          ) +
          facet_wrap("teams", scales = "free", ncol = ncol_plot, nrow = nrow_plot) +
          lims(y = c(min(att_25 - 0.3), max(att_75 + 0.3))) +
          scale_x_discrete(limits = factor(timings), breaks = timings_breaks) +
          labs(
            x = "Periods", y = "Teams' effects",
            title = "Attack effects (50% posterior bars)"
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(size = 16),
            strip.text = element_text(size = 12, color = "black"),
            axis.text.x = element_text(color = "black", size = 9),
            axis.text.y = element_text(size = 11),
            plot.subtitle = element_text(size = 12),
            legend.position = "top",
            legend.text = element_text(size = 15)
          ) +
          scale_color_manual(
            name = "",
            values = c(attack = "#cd1719"),
            labels = c("Attack Effects")
          )
        return(p)
      } else if (type == "defense") {
        position_lookup <-
          def_data %>%
          group_by(teams) %>%
          summarise(pos = first(teams))

        label_w_position <- function(team_name) {
          paste0(team_name, " (", with(position_lookup, pos[teams == player_name]), ")")
        }

        p <- ggplot() +
          geom_ribbon(
            aes(x = times, ymin = lo, ymax = hi),
            data = def_data,
            fill = "#D3D3D3"
          ) +
          geom_line(
            aes(x = times, y = mid, colour = "defense"),
            data = def_data,
            linewidth = 1
          ) +
          facet_wrap("teams", scales = "free", ncol = ncol_plot, nrow = nrow_plot) +
          lims(y = c(min(def_25 - 0.3), max(def_75 + 0.3))) +
          scale_x_discrete(limits = factor(timings), breaks = timings_breaks) +
          labs(
            x = "Periods", y = "Teams' effects",
            title = "Defense effects (50% posterior bars)"
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(size = 16),
            strip.text = element_text(size = 12, color = "black"),
            axis.text.x = element_text(color = "black", size = 11),
            axis.text.y = element_text(size = 11),
            plot.subtitle = element_text(size = 12),
            legend.position = "top",
            legend.text = element_text(size = 15)
          ) +
          scale_color_manual(
            name = "",
            values = c(defense = "#377EB8"),
            labels = c("Defense Effects")
          )
        return(p)
      }
    } else if (length(dim(att)) == 2) {
      att_mean <- apply(att, 2, mean)[sel_teams_index]
      att_025 <- apply(att, 2, function(x) quantile(x, 0.025))[sel_teams_index]
      att_975 <- apply(att, 2, function(x) quantile(x, 0.975))[sel_teams_index]
      def_mean <- apply(def, 2, mean)[sel_teams_index]
      def_025 <- apply(def, 2, function(x) quantile(x, 0.025))[sel_teams_index]
      def_975 <- apply(def, 2, function(x) quantile(x, 0.975))[sel_teams_index]

      if (type == "both") {
        df_att <- data.frame(
          team = sel_teams, mean = att_mean,
          lower = att_025, upper = att_975, effect = "attack"
        )
        df_def <- data.frame(
          team = sel_teams, mean = def_mean,
          lower = def_025, upper = def_975, effect = "defense"
        )
        df <- rbind(df_att, df_def)
        # Order teams by attack estimates (highest at top)
        df$team <- factor(df$team, levels = sel_teams[order(att_mean)])
        p <- ggplot(df, aes(x = .data$mean, y = .data$team, color = .data$effect)) +
          geom_errorbarh(aes(xmin = .data$lower, xmax = .data$upper),
            height = 0.5, position = position_dodge(width = 0.5)
          ) +
          geom_point(position = position_dodge(width = 0.5), size = 2.3) +
          geom_vline(xintercept = 0, linetype = "twodash", color = "grey20") +
          labs(
            x = "Values", y = "Team",
            title = "Attack and Defense abilities (95% post. intervals)"
          ) +
          scale_color_manual(
            name = "",
            values = c(
              defense = "#377EB8",
              attack = "#cd1719"
            ),
            labels = c("Attack Effects", "Defense Effects")
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(size = 16),
            axis.text.x = element_text(color = "black", size = 11),
            axis.text.y = element_text(size = 11),
            plot.subtitle = element_text(size = 12),
            legend.position = "top",
            legend.text = element_text(size = 15)
          )
        return(p)
      } else if (type == "attack") {
        df_att <- data.frame(
          team = sel_teams, mean = att_mean,
          lower = att_025, upper = att_975
        )
        df_att$team <- factor(df_att$team, levels = sel_teams[order(att_mean)])
        p <- ggplot(df_att, aes(x = .data$mean, y = .data$team, color = "Attack Effects")) +
          geom_errorbarh(aes(xmin = .data$lower, xmax = .data$upper), height = 0.5) +
          geom_point(size = 2.3) +
          geom_vline(xintercept = 0, linetype = "twodash", color = "grey20") +
          labs(
            x = "Values", y = "Team",
            title = "Attack abilities (95% post. intervals)"
          ) +
          scale_color_manual(
            name = "",
            values = c("Attack Effects" = "#cd1719"),
            labels = c("Attack Effects")
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(size = 16),
            axis.text.x = element_text(color = "black", size = 11),
            axis.text.y = element_text(size = 11),
            plot.subtitle = element_text(size = 12),
            legend.position = "top",
            legend.text = element_text(size = 15)
          )
        return(p)
      } else if (type == "defense") {
        df_def <- data.frame(
          team = sel_teams, mean = def_mean,
          lower = def_025, upper = def_975
        )
        df_def$team <- factor(df_def$team, levels = sel_teams[order(-def_mean)])
        p <- ggplot(df_def, aes(x = .data$mean, y = .data$team, color = "Defense Effects")) +
          geom_errorbarh(aes(xmin = .data$lower, xmax = .data$upper), height = 0.5) +
          geom_point(size = 2.3) +
          geom_vline(xintercept = 0, linetype = "twodash", color = "grey20") +
          labs(
            x = "Values", y = "Team",
            title = "Defense abilities (95% post. intervals)"
          ) +
          scale_color_manual(
            name = "",
            values = c("Defense Effects" = "#377EB8"),
            labels = c("Defense Effects")
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(size = 16),
            axis.text.x = element_text(color = "black", size = 11),
            axis.text.y = element_text(size = 11),
            plot.subtitle = element_text(size = 12),
            legend.position = "top",
            legend.text = element_text(size = 15)
          )
        return(p)
      }
    } else if (length(dim(att)) == 0) { # student_t case

      ability <- sims$ability

      if (length(dim(ability)) > 3) {
        ability <- ability[, , 1, ] # consider abilities just with the first ranking period
        T <- dim(ability)[2]
        nteams <- dim(ability)[3]
        ability_med <- apply(ability, c(2, 3), stats::median)
        ability_025 <- apply(ability, c(2, 3), function(x) stats::quantile(x, 0.025))
        ability_25 <- apply(ability, c(2, 3), function(x) stats::quantile(x, 0.25))
        ability_75 <- apply(ability, c(2, 3), function(x) stats::quantile(x, 0.75))
        ability_975 <- apply(ability, c(2, 3), function(x) stats::quantile(x, 0.975))

        valid_teams <- match(sel_teams, unique(c(data$home_team, data$away_team)))

        mt_ability_025 <- reshape2::melt(ability_025[, valid_teams])
        mt_ability_25 <- reshape2::melt(ability_25[, valid_teams])
        mt_ability_50 <- reshape2::melt(ability_med[, valid_teams])
        mt_ability_75 <- reshape2::melt(ability_75[, valid_teams])
        mt_ability_975 <- reshape2::melt(ability_975[, valid_teams])

        teams_fac_rep <- rep(sel_teams, each = T)
        times_rep <- rep(1:T, length(sel_teams))

        ability_data <- data.frame(
          teams = teams_fac_rep,
          times = times_rep,
          mid = mt_ability_50$value,
          lo = mt_ability_25$value,
          hi = mt_ability_75$value
        )

        if (length(unique(data$periods)) == 1) {
          timings <- 1:dim(sims$ability)[2]
          sp <- length(timings) %/% 5
          timings_breaks <- timings[sp * c(1:5)]
        } else {
          timings <- unique(data$periods)
          timings_breaks <- timings
        }

        position_lookup <-
          ability_data %>%
          group_by(teams) %>%
          summarise(pos = first(teams))
        label_w_position <- function(team_name) {
          paste0(team_name, " (", with(position_lookup, pos[teams == player_name]), ")")
        }
        p <- ggplot() +
          geom_ribbon(
            aes(x = times, ymin = lo, ymax = hi),
            data = ability_data,
            fill = "#D3D3D3"
          ) +
          geom_line(
            aes(x = times, y = mid),
            data = ability_data,
            linewidth = 1,
            color = "black"
          ) +
          scale_color_manual(
            name = "",
            values = c(
              "#377EB8",
              "#cd1719"
            )
          ) +
          facet_wrap("teams", scales = "free", ncol = ncol_plot, nrow = nrow_plot) +
          lims(y = c(min(ability_25 - 0.2), max(ability_75 + 0.2))) +
          scale_x_discrete(limits = factor(timings), breaks = timings_breaks) +
          labs(
            x = "Periods", y = "Teams' effects",
            title = "Global abilities effects (50% posterior bars)"
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(size = 16),
            strip.text = element_text(size = 12, color = "black"),
            axis.text.x = element_text(
              color = "black",
              angle = 0, size = 11
            ),
            axis.text.y = element_text(size = 11),
            plot.subtitle = element_text(size = 12),
            legend.position = "top",
            legend.text = element_text(size = 15)
          )
        return(p)
      } else {
        # Non-dynamic student_t case
        ability <- ability[, 1, ]
        ability_mean <- apply(ability, 2, mean)[sel_teams_index]
        ability_025 <- apply(ability, 2, function(x) quantile(x, 0.025))[sel_teams_index]
        ability_975 <- apply(ability, 2, function(x) quantile(x, 0.975))[sel_teams_index]

        df_ability <- data.frame(
          team = sel_teams, mean = ability_mean,
          lower = ability_025, upper = ability_975
        )
        df_ability$team <- factor(df_ability$team, levels = sel_teams[order(ability_mean)])
        p <- ggplot(df_ability, aes(x = .data$mean, y = .data$team, color = "Global Effects")) +
          geom_errorbarh(aes(xmin = .data$lower, xmax = .data$upper), height = 0.5) +
          geom_point(size = 2.3) +
          geom_vline(xintercept = 0, linetype = "twodash", color = "grey20") +
          labs(
            x = "Values", y = "Team",
            title = "Global abilities (95% post. intervals)"
          ) +
          scale_color_manual(
            name = "",
            values = c("Global Effects" = "black"),
            labels = c("Global Effects")
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(size = 16),
            axis.text.x = element_text(color = "black", size = 11),
            axis.text.y = element_text(size = 11),
            plot.subtitle = element_text(size = 12),
            legend.position = "top",
            legend.text = element_text(size = 15)
          )

        return(p)
      }
    }
  } else if (inherits(object, "list")) {
    # List-based (MLE) models
    if (is.null(teams)) {
      sel_teams <- teams_all
    } else {
      sel_teams <- teams_all[match(teams, teams_all)]
    }
    sel_teams_index <- match(sel_teams, teams_all)
    if (any(is.na(sel_teams_index))) {
      stop("Select only valid teams' names!")
    }

    if (!is.null(dim(object$att))) {
      att <- object$att[sel_teams_index, ]
      def <- object$def[sel_teams_index, ]
      if (is.vector(att) & is.vector(def)) {
        stop("Please, select at least two teams")
      }
      if (type == "both") {
        df_att <- data.frame(
          team = sel_teams, mean = att[, 2],
          lower = att[, 1], upper = att[, 3], effect = "attack"
        )
        df_def <- data.frame(
          team = sel_teams, mean = def[, 2],
          lower = def[, 1], upper = def[, 3], effect = "defense"
        )
        df <- rbind(df_att, df_def)
        df$team <- factor(df$team, levels = sel_teams[order(att[, 2])])
        p <- ggplot(df, aes(x = .data$mean, y = .data$team, color = .data$effect)) +
          geom_vline(xintercept = 0, linetype = "twodash", color = "grey20") +
          geom_errorbarh(aes(xmin = .data$lower, xmax = .data$upper),
            height = 0.2, position = position_dodge(width = 0.3)
          ) +
          geom_point(position = position_dodge(width = 0.3), size = 2) +
          labs(
            x = "Values", y = "Team",
            title = "Attack and Defense abilities (95% intervals)"
          ) +
          scale_color_manual(
            name = "",
            values = c(
              defense = "#377EB8",
              attack = "#cd1719"
            ),
            labels = c("Attack Effects", "Defense Effects")
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(size = 16),
            axis.text.x = element_text(color = "black", size = 11),
            axis.text.y = element_text(size = 11),
            plot.subtitle = element_text(size = 12),
            legend.position = "top",
            legend.text = element_text(size = 15)
          )
        return(p)
      } else if (type == "attack") {
        df_att <- data.frame(
          team = sel_teams, mean = att[, 2],
          lower = att[, 1], upper = att[, 3]
        )
        df_att$team <- factor(df_att$team, levels = sel_teams[order(att[, 2])])
        p <- ggplot(df_att, aes(x = .data$mean, y = .data$team, color = "Attack Effects")) +
          geom_vline(xintercept = 0, linetype = "twodash", color = "grey20") +
          geom_errorbarh(aes(xmin = .data$lower, xmax = .data$upper), height = 0.5) +
          geom_point(size = 2.3) +
          labs(
            x = "Values", y = "Team",
            title = "Attack abilities (95% intervals)"
          ) +
          scale_color_manual(
            name = "",
            values = c("Attack Effects" = "#cd1719"),
            labels = c("Attack Effects")
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(size = 16),
            axis.text.x = element_text(color = "black", size = 11),
            axis.text.y = element_text(size = 11),
            plot.subtitle = element_text(size = 12),
            legend.position = "top",
            legend.text = element_text(size = 15)
          )
        return(p)
      } else if (type == "defense") {
        df_def <- data.frame(
          team = sel_teams, mean = def[, 2],
          lower = def[, 1], upper = def[, 3]
        )
        df_def$team <- factor(df_def$team, levels = sel_teams[order(-def[, 2])])
        p <- ggplot(df_def, aes(x = .data$mean, y = .data$team, color = "Defense Effects")) +
          geom_vline(xintercept = 0, linetype = "twodash", color = "grey20") +
          geom_errorbarh(aes(xmin = .data$lower, xmax = .data$upper), height = 0.5) +
          geom_point(size = 2.3) +
          labs(
            x = "Values", y = "Team",
            title = "Defense abilities (95% intervals)"
          ) +
          scale_color_manual(
            name = "",
            values = c("Defense Effects" = "#377EB8"),
            labels = c("Defense Effects")
          ) +
          theme_bw() +
          theme(
            plot.title = element_text(size = 16),
            axis.text.x = element_text(color = "black", size = 11),
            axis.text.y = element_text(size = 11),
            plot.subtitle = element_text(size = 12),
            legend.position = "top",
            legend.text = element_text(size = 15)
          )
        return(p)
      }
    } else {
      # student_t case for list object
      ability <- object$abilities[sel_teams_index, ]
      if (is.vector(ability)) {
        stop("Please, select at least two teams")
      }
      df_ability <- data.frame(
        team = sel_teams, mean = ability[, 2],
        lower = ability[, 1], upper = ability[, 3]
      )
      df_ability$team <- factor(df_ability$team, levels = sel_teams[order(ability[, 2])])
      p <- ggplot(df_ability, aes(x = .data$mean, y = .data$team, color = "Global Effects")) +
        geom_vline(xintercept = 0, linetype = "twodash", color = "grey20") +
        geom_errorbarh(aes(xmin = .data$lower, xmax = .data$upper), height = 0.5) +
        geom_point(size = 2.3) +
        labs(
          x = "Values", y = "Team",
          title = "Global abilities (95% intervals)"
        ) +
        scale_color_manual(
          name = "",
          values = c("Global Effects" = "black"),
          labels = c("Global Effects")
        ) +
        theme_bw() +
        theme(
          plot.title = element_text(size = 16),
          axis.text.x = element_text(color = "black", size = 11),
          axis.text.y = element_text(size = 11),
          plot.subtitle = element_text(size = 12),
          legend.position = "top",
          legend.text = element_text(size = 15)
        )
      return(p)
    }
  } else {
    stop("Provide one among these three model fit classes: 'stanfit', 'stanFoot' or 'list'.")
  }
}

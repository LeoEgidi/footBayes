#' Bayesian Bradley-Terry-Davidson Model
#'
#' Fits a Bayesian Bradley-Terry-Davidson model using Stan. Supports both static and dynamic ranking models, allowing for the estimation of team strengths over time.
#'
#' @param data A data frame containing the observations with columns:
#'   \itemize{
#'     \item \code{periods}: Time point of each observation (integer >= 1).
#'     \item \code{home_team}: Home team's name (character string).
#'     \item \code{away_team}: Away team's name (character string).
#'     \item \code{match_outcome}: Outcome (1 if home team beats away team, 2 for tie, and 3 if away team beats home team).
#'   }
#'   The data frame must not contain missing values.
#' @param dynamic_rank A logical value indicating whether a dynamic ranking model is used (default is \code{FALSE}).
#' @param home_effect A logical value indicating the inclusion of a home effect in the model. (default is \code{FALSE}).
#' @param prior_par A list specifying the prior distributions for the parameters of interest, using the \code{normal} function:
#'   \itemize{
#'     \item \code{logStrength}: Prior for the team log-strengths. Default is \code{normal(0, 3)}.
#'     \item \code{logTie}: Prior for the tie parameter. Default is \code{normal(0, 0.3)}.
#'     \item \code{home}: Prior for the home effect (\code{home}). Applicable only if \code{home_effect = TRUE}. Default is \code{normal(0, 5)}.
#'   }
#'   Only normal priors are allowed for this model.
#' @param rank_measure A character string specifying the method used to summarize the posterior distributions of the team strengths. Options are:
#'   \itemize{
#'     \item \code{"median"}: Uses the median of the posterior samples (default).
#'     \item \code{"mean"}: Uses the mean of the posterior samples.
#'     \item \code{"map"}: Uses the Maximum A Posteriori estimate, calculated as the mode of the posterior distribution.
#'   }
#'
#' @param method A character string specifying the method used to obtain the Bayesian estimates. Options are:
#'   \itemize{
#'     \item \code{"MCMC"}: Markov chain Monte Carlo algorithm (default).
#'     \item \code{"VI"}: Automatic differentiation variational inference algorithms.
#'     \item \code{"pathfinder"}: Pathfinder variational inference algorithm.
#'     \item \code{"laplace"}: Laplace algorithm.
#'   }
#' @param ... Additional arguments passed to \code{\link[cmdstanr]{cmdstanr}} (e.g., \code{iter_sampling}, \code{chains}, \code{parallel_chains}).
#'
#' @return A list of class \code{"btdFoot"} containing:
#'   \itemize{
#'     \item \code{fit}: The fitted \code{stanfit} object returned by \code{\link[cmdstanr]{cmdstanr}}.
#'     \item \code{rank}: A data frame with the rankings, including columns:
#'       \itemize{
#'         \item \code{periods}: The time period.
#'         \item \code{team}: The team name.
#'         \item \code{rank_points}: The estimated strength of the team based on the chosen \code{rank_measure}.
#'       }
#'     \item \code{data}: The input data.
#'     \item \code{stan_data}: The data list prepared for Stan.
#'     \item \code{stan_code}: The path to the Stan model code used.
#'     \item \code{stan_args}: The optional parameters passed to (\code{...}).
#'     \item \code{rank_measure}: The method used to compute the rankings.
#'     \item \code{alg_method}: The method used to obtain the Bayesian estimates.
#'   }
#'
#' @author Roberto Macrì Demartino \email{roberto.macridemartino@deams.units.it}.
#'
#' @examples
#' \dontrun{
#' if (instantiate::stan_cmdstan_exists()) {
#'   library(dplyr)
#'
#'   data("italy")
#'
#'   italy_2020_2021 <- italy %>%
#'     dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
#'     dplyr::filter(Season == "2020" | Season == "2021") %>%
#'     dplyr::mutate(match_outcome = dplyr::case_when(
#'       hgoal > vgoal ~ 1, # Home team wins
#'       hgoal == vgoal ~ 2, # Draw
#'       hgoal < vgoal ~ 3 # Away team wins
#'     )) %>%
#'     dplyr::mutate(periods = dplyr::case_when(
#'       dplyr::row_number() <= 190 ~ 1,
#'       dplyr::row_number() <= 380 ~ 2,
#'       dplyr::row_number() <= 570 ~ 3,
#'       TRUE ~ 4
#'     )) %>% # Assign periods based on match number
#'     dplyr::select(periods,
#'       home_team = home,
#'       away_team = visitor, match_outcome
#'     )
#'
#'   # Dynamic Ranking Example with Median Rank Measure
#'   fit_result_dyn <- btd_foot(
#'     data = italy_2020_2021,
#'     dynamic_rank = TRUE,
#'     home_effect = TRUE,
#'     prior_par = list(
#'       logStrength = normal(0, 10),
#'       logTie = normal(0, 5),
#'       home = normal(0, 5)
#'     ),
#'     rank_measure = "median",
#'     iter_sampling = 1000,
#'     parallel_chains = 2,
#'     chains = 2
#'   )
#'
#'   print(fit_result_dyn)
#'
#'   print(fit_result_dyn, pars = c("logStrength", "home"), teams = c("AC Milan", "AS Roma"))
#'
#'   # Static Ranking Example with MAP Rank Measure
#'   fit_result_stat <- btd_foot(
#'     data = italy_2020_2021,
#'     dynamic_rank = FALSE,
#'     prior_par = list(
#'       logStrength = normal(0, 10),
#'       logTie = normal(0, 5),
#'       home = normal(0, 5)
#'     ),
#'     rank_measure = "map",
#'     iter_sampling = 1000,
#'     parallel_chains = 2,
#'     chains = 2
#'   )
#'
#'   print(fit_result_stat)
#' }
#' }
# @importFrom instantiate stan_package_model
#' @export


btd_foot <- function(data,
                     dynamic_rank = FALSE,
                     home_effect = FALSE,
                     prior_par = list(
                       logStrength = normal(0, 3),
                       logTie = normal(0, 0.3),
                       home = normal(0, 5)
                     ),
                     rank_measure = "median",
                     method = "MCMC",
                     ...) {
  #   ____________________________________________________________________________
  #   Data check                                                              ####

  # Check that data is a data frame
  if (!is.data.frame(data)) {
    stop("Data must be a data frame.")
  }

  # Check that required columns are present
  required_cols <- c("periods", "home_team", "away_team", "match_outcome")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Data is missing required columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  # NAs
  if (any(is.na(data))) {
    stop("Data contains missing values. Please handle them before fitting the model.")
  }

  # Check periods
  if (any(data$periods < 0) || !is.numeric(data$periods) || any(data$periods != as.integer(data$periods))) {
    stop("Column 'periods' must contain positive integer values.")
  }
  # if (any(instants_rank < 1 | instants_rank > ntimes_rank)) {
  #   stop(paste("Values in 'periods' must be between 1 and", ntimes_rank, "."))
  # }

  # Check match_outcome
  if (!is.numeric(data$match_outcome) || any(data$match_outcome != as.integer(data$match_outcome))) {
    stop("Column 'match_outcome' must contain integer values.")
  }
  if (any(!data$match_outcome %in% c(1, 2, 3))) {
    stop("Values in 'match_outcome' must be 1, 2, or 3.")
  }

  # Extract variables
  instants_rank <- match(data$periods, unique(data$periods))
  home_team <- data$home_team
  away_team <- data$away_team
  match_outcome <- data$match_outcome
  N <- nrow(data)

  # Create teams vector and map team names to integer indices
  teams <- unique(c(home_team, away_team))
  nteams <- length(teams)
  ntimes_rank <- length(unique(data$periods))

  # Map team names to integer indices
  home_team_idx <- match(home_team, teams)
  away_team_idx <- match(away_team, teams)

  # Check for NAs in home_team_idx and away_team_idx
  if (any(is.na(home_team_idx))) {
    stop("Some values in 'home_team' do not match any known teams.")
  }
  if (any(is.na(away_team_idx))) {
    stop("Some values in 'away_team' do not match any known teams.")
  }

  #   ____________________________________________________________________________
  #   Home Effect Check                                                       ####


  # Check that home_effect is logical
  if (!is.logical(home_effect) || length(home_effect) != 1) {
    stop("'home_effect' must be a single logical value (TRUE or FALSE).")
  }


  #   ____________________________________________________________________________
  #   Additional Stan Parameters                                              ####

  # Initialize user_dots with default arguments
  user_dots <- list(
    chains = 4,
    iter_warmup = NULL,
    iter_sampling = 1000,
    thin = 1,
    seed = sample.int(.Machine$integer.max, 1),
    parallel_chains = getOption("mc.cores", 1L),
    adapt_delta = 0.8,
    max_treedepth = 10
  )


  #   ____________________________________________________________________________
  #   Optional Arguments Checks                                               ####

  # Process optional arguments
  user_dots_prel <- list(...)

  # Update 'user_dots' with any other provided arguments
  user_dots <- utils::modifyList(user_dots, user_dots_prel)

  # Set warmup if not provided
  if (is.null(user_dots$iter_warmup)) {
    user_dots$iter_warmup <- floor(user_dots$iter_sampling)
  }

  #   ____________________________________________________________________________
  #   Setting priors                                                          ####

  # Set default priors
  default_priors <- list(
    logStrength = normal(0, 3),
    logTie = normal(0, 0.3),
    home = normal(0, 5)
  )

  # If prior_par is NULL, set default priors
  if (is.null(prior_par)) {
    prior_par <- default_priors
  }


  # Validate prior names
  allowed_prior_names <- c("logStrength", "logTie", "home")

  # Check that prior_par contains only allowed elements
  if (!is.null(prior_par)) {
    if (!is.list(prior_par)) {
      stop("'prior_par' must be a list.")
    }
    unknown_prior_names <- setdiff(names(prior_par), allowed_prior_names)
    if (length(unknown_prior_names) > 0) {
      stop(
        paste(
          "Unknown elements in 'prior_par':",
          paste(unknown_prior_names, collapse = ", ")
        )
      )
    }
  }

  # Merge prior_par with defaults
  prior_par <- utils::modifyList(default_priors, prior_par)


  # Extract prior parameters from the priors list
  logStrength_prior <- prior_par$logStrength
  logTie_prior <- prior_par$logTie
  home_prior <- prior_par$home

  # Only normal prior

  if (logStrength_prior$dist != "normal" || logTie_prior$dist != "normal" ||
    (home_effect && home_prior$dist != "normal")) {
    stop("Prior distributions must be 'normal'.")
  }


  mean_logStrength <- logStrength_prior$location
  sd_logStrength <- logStrength_prior$scale
  mean_logTie <- logTie_prior$location
  sd_logTie <- logTie_prior$scale

  #   ____________________________________________________________________________
  #   Rank measure validation                                                 ####

  # Validate rank_measure
  allowed_rank_measures <- c("median", "mean", "map")

  rank_measure <- match.arg(rank_measure, allowed_rank_measures)


  #   ____________________________________________________________________________
  #   Computational algorithm                                                 ####

  allowed_method_names <- c("MCMC", "VI", "pathfinder", "laplace")

  method <- match.arg(method, allowed_method_names)



  # # Check prior_par' value
  #
  # check_prior(mean_logStrength, "mean_logStrength")
  # check_prior(sd_logStrength, "sd_logStrength", positive = TRUE)
  # check_prior(mean_logTie, "mean_logTie")
  # check_prior(sd_logTie, "sd_logTie", positive = TRUE)
  #
  # if (home_effect) {
  #   check_prior(mean_home, "mean_home")
  #   check_prior(sd_home, "sd_home", positive = TRUE)
  # }



  #   ____________________________________________________________________________
  #   Models                                                                  ####


  if (home_effect) {
    ind_home <- 1
    mean_home <- home_prior$location
    sd_home <- home_prior$scale
  } else {
    ind_home <- 0
    mean_home <- 0
    sd_home <- 1 # Default value (can be adjusted)
  }

  # Prepare data for Stan based on dynamic_rank
  if (!dynamic_rank) {
    data_stan <- list(
      N = N,
      nteams = nteams,
      team1 = as.integer(home_team_idx),
      team2 = as.integer(away_team_idx),
      mean_logStrength = mean_logStrength,
      sd_logStrength = sd_logStrength,
      mean_logTie = mean_logTie,
      sd_logTie = sd_logTie,
      mean_home = mean_home,
      sd_home = sd_home,
      ind_home = ind_home,
      y = as.integer(match_outcome)
    )
  } else {
    data_stan <- list(
      N = N,
      nteams = nteams,
      ntimes_rank = ntimes_rank,
      instants_rank = as.integer(instants_rank),
      team1 = as.integer(home_team_idx),
      team2 = as.integer(away_team_idx),
      mean_logStrength = mean_logStrength,
      sd_logStrength = sd_logStrength,
      mean_logTie = mean_logTie,
      sd_logTie = sd_logTie,
      mean_home = mean_home,
      sd_home = sd_home,
      ind_home = ind_home,
      y = as.integer(match_outcome)
    )
  }

  # Prepare data for Stan based on dynamic_rank
  model_name <- if (!dynamic_rank) "static_btd" else "dynamic_btd"

  model <- instantiate::stan_package_model(name = model_name, package = "footBayes")


  if (method == "MCMC") {
    mcmc_arg_names <- c(
      "refresh",
      "init",
      "save_latent_dynamics",
      "output_dir",
      "output_basename",
      "sig_figs",
      "chains",
      "parallel_chains",
      "chain_ids",
      "threads_per_chain",
      "opencl_ids",
      "iter_warmup",
      "iter_sampling",
      "save_warmup",
      "thin",
      "max_treedepth",
      "adapt_engaged",
      "adapt_delta",
      "step_size",
      "metric",
      "metric_file",
      "inv_metric",
      "init_buffer",
      "term_buffer",
      "window",
      "fixed_param",
      "show_messages",
      "show_exceptions",
      "diagnostics",
      "save_metric",
      "save_cmdstan_config",
      "cores",
      "num_cores",
      "num_chains",
      "num_warmup",
      "num_samples",
      "validate_csv",
      "save_extra_diagnostics",
      "max_depth",
      "stepsize"
    )

    args <- user_dots[names(user_dots) %in% mcmc_arg_names]

    fit <- do.call(
      model$sample,
      c(
        list(
          data = data_stan,
          seed = user_dots$seed
        ),
        args
      )
    )
  } else if (method == "VI") {
    vi_arg_names <- c(
      "refresh",
      "init",
      "save_latent_dynamics",
      "output_dir",
      "output_basename",
      "sig_figs",
      "threads",
      "opencl_ids",
      "algorithm",
      "iter",
      "grad_samples",
      "elbo_samples",
      "eta",
      "adapt_engaged",
      "adapt_iter",
      "tol_rel_obj",
      "eval_elbo",
      "output_samples",
      "draws",
      "show_messages",
      "show_exceptions",
      "save_cmdstan_config"
    )

    args <- user_dots[names(user_dots) %in% vi_arg_names]

    fit <- do.call(
      model$variational,
      c(
        list(
          data = data_stan,
          seed = user_dots$seed
        ),
        args
      )
    )
  } else if (method == "pathfinder") {
    pf_arg_names <- c(
      "refresh",
      "init",
      "save_latent_dynamics",
      "output_dir",
      "output_basename",
      "sig_figs",
      "opencl_ids",
      "num_threads",
      "init_alpha",
      "tol_obj",
      "tol_rel_obj",
      "tol_grad",
      "tol_rel_grad",
      "tol_param",
      "history_size",
      "single_path_draws",
      "draws",
      "num_paths",
      "max_lbfgs_iters",
      "num_elbo_draws",
      "save_single_paths",
      "psis_resample",
      "calculate_lp",
      "show_messages",
      "show_exceptions",
      "save_cmdstan_config"
    )

    args <- user_dots[names(user_dots) %in% pf_arg_names]

    fit <- do.call(
      model$pathfinder,
      c(
        list(
          data = data_stan,
          seed = user_dots$seed
        ),
        args
      )
    )
  } else if (method == "laplace") {
    laplace_arg_names <- c(
      "refresh",
      "init",
      "save_latent_dynamics",
      "output_dir",
      "output_basename",
      "sig_figs",
      "threads",
      "opencl_ids",
      "mode",
      "opt_args",
      "jacobian",
      "draws",
      "show_messages",
      "show_exceptions",
      "save_cmdstan_config"
    )

    args <- user_dots[names(user_dots) %in% laplace_arg_names]

    fit <- do.call(
      model$laplace,
      c(
        list(
          data = data_stan,
          seed = user_dots$seed
        ),
        args
      )
    )
  }

  # Extract posterior draws using posterior package for easier manipulation
  draws <- posterior::as_draws_df(fit$draws())

  results_df <- data.frame()

  if (!dynamic_rank) {
    # Static model
    for (team in teams) {
      team_index <- which(teams == team)

      # Extract samples for logStrength of the team
      param_name <- paste0("logStrength[", team_index, "]")
      if (!param_name %in% colnames(draws)) {
        stop(paste("Parameter", param_name, "not found in the model."))
      }
      logStrength_samples <- as.numeric(draws[[param_name]])

      # Compute the summary statistic based on rank_measure
      rank_point <- switch(rank_measure,
        median = median(logStrength_samples),
        mean = mean(logStrength_samples),
        map = compute_MAP(logStrength_samples)
      )

      df <- data.frame(
        periods = 1,
        team = team,
        log_strengths = rank_point,
        stringsAsFactors = FALSE
      )

      results_df <- rbind(results_df, df)
    }
    BTDrank <- results_df
  } else {
    # Dynamic model
    for (team in teams) {
      team_index <- which(teams == team)

      # Compute the summary statistic for each period
      rank_point <- sapply(1:ntimes_rank, function(k) {
        param_name <- paste0("logStrength[", k, ",", team_index, "]")
        if (!param_name %in% colnames(draws)) {
          stop(paste("Parameter", param_name, "not found in the model."))
        }
        logStrength_samples <- as.numeric(draws[[param_name]])
        switch(rank_measure,
          median = median(logStrength_samples),
          mean = mean(logStrength_samples),
          map = compute_MAP(logStrength_samples)
        )
      })

      df <- data.frame(
        periods = c(unique(data$periods)),
        team = team, # Use team name
        log_strengths = rank_point,
        stringsAsFactors = FALSE
      )

      results_df <- rbind(results_df, df)
    }
    BTDrank <- results_df[, c("periods", "team", "log_strengths")]
  }

  # Final Output
  output <- list(
    fit = fit,
    rank = BTDrank,
    data = data,
    stan_data = data_stan,
    stan_code = fit$code(),
    # prior_par = priors_output,
    stan_args = user_dots,
    rank_measure = rank_measure,
    alg_method = method
  )

  class(output) <- "btdFoot"
  return(output)
}

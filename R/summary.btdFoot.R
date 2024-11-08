#' Summary Method for btdFoot Objects
#'
#' Provides detailed posterior summaries for the Bayesian Bradley-Terry-Davidson model parameters.
#'
#' @param object An object of class \code{btdFoot}.
#' @param pars Optional character vector specifying parameters to include in the summary. This can be specific parameter names or macro parameter groups (e.g., \code{"logStrength"}, \code{"logTie"}, \code{"home"}, \code{"log_lik"}, and \code{"y_rep"}). If \code{NULL}, all parameters are included.
#' @param digits Number of significant digits to use when printing numeric values.
#' @param true_names Logical value indicating whether to display team names in parameter summaries. Default is \code{TRUE}.
#' @param ... Additional arguments passed.
#' @method summary btdFoot
#' @export
summary.btdFoot <- function(object, pars = NULL, digits = 2, true_names = TRUE, ...) {
  if (!inherits(object, "btdFoot")) {
    stop("The object must be of class 'btdFoot'.")
  }

  cat("Summary of Bayesian Bradley-Terry-Davidson Model\n")
  cat("------------------------------------------------\n")
  cat("Rank Measure Used:", object$rank_measure, "\n\n")

  # Display the ranking table
  cat("Top Teams Based on Ranking Points:\n")
  top_teams <- object$rank[order(-object$rank$rank_points), ]
  print(utils::head(top_teams, 10), digits = digits)
  cat("\n")

  # Ensure 'object$fit' is a 'stanfit' object
  if (!inherits(object$fit, "stanfit")) {
    stop("The 'fit' component must be a 'stanfit' object.")
  }

  # Extract all parameter names
  all_param_names <- object$fit@sim$pars_oi

  # Define valid parameter groups
  valid_macros <- c("logStrength", "logTie", "home", "log_lik", "y_rep")

  # Build the final list of parameters to include
  if (is.null(pars)) {
    final_pars <- NULL  # Include all parameters
  } else {
    final_pars <- c()
    for (p in pars) {
      if (p %in% valid_macros) {
        # Include all parameters starting with the macro name
        matched_pars <- grep(paste0("^", p), all_param_names, value = TRUE)
        if (length(matched_pars) == 0) {
          warning("No parameters found for macro parameter group '", p, "'.")
        }
        final_pars <- c(final_pars, matched_pars)
      } else if (p %in% all_param_names) {
        # Include the specific parameter name
        final_pars <- c(final_pars, p)
      } else {
        warning("Parameter '", p, "' not found among valid parameters.")
      }
    }
    if (length(final_pars) == 0) {
      stop("No valid parameters specified in 'pars'.")
    }
  }

  # Extract posterior summaries
  cat("Posterior Summaries for Model Parameters:\n")
  if (is.null(final_pars)) {
    stan_summary <- rstan::summary(object$fit, ...)$summary
  } else {
    stan_summary <- rstan::summary(object$fit, pars = unique(final_pars), ...)$summary
  }

  # Replace parameter names with team names if true_names is TRUE
  if (true_names) {
    # Extract unique team names from data
    teams <- sort(unique(c(object$data$home_team, object$data$away_team)))
    team_map <- stats::setNames(teams, seq_along(teams))

    # Parameters to exclude from name replacement
    exclude_params <- c("log_lik", "y_rep")

    # Apply the function to row names
    new_param_names <- rep_ind_with_team_names(rownames(stan_summary), exclude_params, team_map)

    # Check if dynamic_rank is FALSE and remove trailing commas if necessary
    if (!isTRUE(object$dynamic_rank)) {
      # Remove trailing commas inside square brackets
      new_param_names <- gsub(",\\]", "]", new_param_names)
    }

    rownames(stan_summary) <- new_param_names
  }

  print(round(stan_summary, digits = digits))

  invisible(object)
}

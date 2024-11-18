#' Print Method for stanFoot Objects
#'
#' Provides detailed posterior summaries for the Stan football model parameters.
#'
#' @param x An object of class \code{stanFoot}.
#' @param pars Optional character vector specifying parameters to include in the summary. This can be specific parameter names or macro parameter groups (e.g., \code{"att"}, \code{"def"}, \code{"att_raw"}, \code{"def_raw"}, \code{"home"}, \code{"sigma_att"}, \code{"sigma_def"}, \code{"Sigma_att"}, \code{"Sigma_def"}, \code{"rho"}, \code{"beta"}, and \code{"sigma_y"}). If \code{NULL}, all parameters are included.
#' @param digits Number of significant digits to use when printing numeric values.
#' @param true_names Logical value indicating whether to display team names in parameter summaries. Default is \code{TRUE}.
#' @param ... Additional arguments passed.
#' @method print stanFoot
#' @export
print.stanFoot <- function(x, pars = NULL, digits = 3, true_names = TRUE, ...) {
  if (!inherits(x, "stanFoot")) {
    stop("The object must be of class 'stanFoot'.")
  }

  cat("Summary of Stan Football Model\n")
  cat("------------------------------\n")


  # Ensure 'object$fit' is a 'stanfit' object
  if (!inherits(x$fit, "stanfit")) {
    stop("The 'fit' component must be a 'stanfit' object.")
  }

  # Extract all parameter names
  all_param_names <- x$fit@sim$pars_oi

  # Build the final list of parameters to include
  if (is.null(pars)) {
    final_pars <- NULL
  } else {
    final_pars <- c()
    for (p in pars) {
      if (p %in% all_param_names) {
        # Include all parameters
        matched_pars <- grep(paste0("^", p, "(\\[|$)"), all_param_names, value = TRUE)
        if (length(matched_pars) == 0) {
          warning("No parameters found for group '", p, "'.")
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
    stan_summary <- rstan::summary(x$fit, ...)$summary
  } else {
    stan_summary <- rstan::summary(x$fit, pars = unique(final_pars), ...)$summary
  }

  # Replace parameter names with team names if true_names is TRUE
  if (true_names) {
    # Extract unique team names from data
    teams <- sort(unique(c(x$data$home_team, x$data$away_team)))
    team_map <- stats::setNames(teams, seq_along(teams))

    # Parameters to exclude from name replacement
    exclude_params <- all_param_names[!all_param_names %in% c("att_raw", "def_raw", "att", "def")]


    # Apply the function to row names
    new_param_names <- team_names(rownames(stan_summary), exclude_params, team_map)

    # Check if dynamic_type argument is FALSE and remove commas if necessary
    if (is.null(x$dynamic_type)) {
      new_param_names <- gsub(",\\]", "]", new_param_names)
    }

    rownames(stan_summary) <- new_param_names
  }

  print(round(stan_summary, digits = digits))

  invisible(x)
}





#' Print Method for btdFoot Objects
#'
#' Provides detailed posterior summaries for the Bayesian Bradley-Terry-Davidson model parameters.
#'
#' @param x An object of class \code{btdFoot}.
#' @param pars Optional character vector specifying parameters to include in the summary. This can be specific parameter names or macro parameter groups (e.g., \code{"logStrength"}, \code{"logTie"}, \code{"home"}, \code{"log_lik"}, and \code{"y_rep"}). If \code{NULL}, all parameters are included.
#' @param digits Number of significant digits to use when printing numeric values.
#' @param true_names Logical value indicating whether to display team names in parameter summaries. Default is \code{TRUE}.
#' @param ... Additional arguments passed.
#' @method print btdFoot
#' @export
print.btdFoot <- function(x, pars = NULL, digits = 3, true_names = TRUE, ...) {
  if (!inherits(x, "btdFoot")) {
    stop("The object must be of class 'btdFoot'.")
  }

  cat("Summary of Bayesian Bradley-Terry-Davidson Model\n")
  cat("------------------------------------------------\n")
  cat("Rank Measure Used:", x$rank_measure, "\n\n")

  # Display the ranking table
  cat("Top Teams Based on Ranking Points:\n")
  top_teams <- x$rank[order(-x$rank$rank_points), ]
  print(utils::head(top_teams, 10), digits = digits)
  cat("\n")

  # Ensure 'object$fit' is a 'stanfit' object
  if (!inherits(x$fit, "stanfit")) {
    stop("The 'fit' component must be a 'stanfit' object.")
  }

  # Extract all parameter names
  all_param_names <- x$fit@sim$pars_oi


  # Build the final list of parameters to include
  if (is.null(pars)) {
    final_pars <- NULL
  } else {
    final_pars <- c()
    for (p in pars) {
      if (p %in% all_param_names) {
        # Include all parameters
        matched_pars <- grep(paste0("^", p), all_param_names, value = TRUE)
        if (length(matched_pars) == 0) {
          warning("No parameters found for group '", p, "'.")
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
    stan_summary <- rstan::summary(x$fit, ...)$summary
  } else {
    stan_summary <- rstan::summary(x$fit, pars = unique(final_pars), ...)$summary
  }

  # Replace parameter names with team names if true_names is TRUE
  if (true_names) {
    # Extract unique team names from data
    teams <- sort(unique(c(x$data$home_team, x$data$away_team)))
    team_map <- stats::setNames(teams, seq_along(teams))

    # Parameters to exclude from name replacement
    exclude_params <- c("log_lik", "y_rep")

    # Apply the function to row names
    new_param_names <- team_names(rownames(stan_summary), exclude_params, team_map)

    # Check if dynamic_type argument is FALSE and remove commas if necessary
    if (!isTRUE(x$dynamic_rank)) {
      # Remove trailing commas inside square brackets
      new_param_names <- gsub(",\\]", "]", new_param_names)
    }

    rownames(stan_summary) <- new_param_names
  }

  print(round(stan_summary, digits = digits))

  invisible(x)
}




#' Print method for compareFoot objects
#'
#' Provides a formatted output when printing objects of class \code{compareFoot}, displaying the predictive performance metrics and, if available, the confusion matrices for each model or probability matrix.
#'
#' @param x An object of class \code{compareFoot} returned by \code{\link{compare_foot}}.
#' @param ... Additional arguments passed to \code{print}.
#' @method print compareFoot
#' @export
#'
print.compareFoot <- function(x, ...) {
  cat("Predictive performance metrics\n")
  print(x$metrics)
  if (!is.null(x$confusion_matrix)) {
    cat("\nConfusion Matrices\n")
    for (model_name in names(x$confusion_matrix)) {
      cat("Model:", model_name, "\n \n",sep = " ")
      print(x$confusion_matrix[[model_name]])
      cat("\n")
    }
  }
}

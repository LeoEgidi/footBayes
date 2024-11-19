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
print.stanFoot <- function(x, pars = NULL, teams = NULL, digits = 3, true_names = TRUE,
                           display = c("parameters", "both"), ...) {
  # Check if the object is of class 'stanFoot'
  if (!inherits(x, "stanFoot")) {
    stop("The object must be of class 'stanFoot'.")
  }

  # Validate 'digits' argument
  if (!is.numeric(digits) || digits <= 0) {
    stop("'digits' must be a positive numeric value.")
  }

  # Match the 'display' argument
  display <- match.arg(display)

  cat("Summary of Stan football model\n")
  cat("------------------------------\n\n")

  # Ensure 'x$fit' is a 'stanfit' object
  if (!inherits(x$fit, "stanfit")) {
    stop("The 'fit' component must be a 'stanfit' object.")
  }

  # Extract all parameter names from the 'stanfit' object
  all_param_names <- x$fit@sim$pars_oi

  # Initialize 'final_pars' based on 'pars' argument
  if (is.null(pars)) {
    final_pars <- NULL
  } else {
    final_pars <- c()
    for (p in pars) {
      if (p %in% all_param_names) {
        # Include all parameters matching the pattern (e.g., att[1], att[2], etc.)
        matched_pars <- grep(paste0("^", p, "(\\[|$)"), all_param_names, value = TRUE)
        if (length(matched_pars) == 0) {
          warning("No parameters found for group '", p, "'.")
        }
        final_pars <- c(final_pars, matched_pars)
      } else {
        warning("Parameter '", p, "' not found among valid parameters.")
      }
    }
    # Remove duplicates
    final_pars <- unique(final_pars)
    if (length(final_pars) == 0) {
      stop("No valid parameters specified in 'pars'.")
    }
  }

  # Extract unique team names from data
  teams_all <- sort(unique(c(as.character(x$data$home_team), as.character(x$data$away_team))))

  # If 'teams' is specified, validate and filter them
  if (!is.null(teams)) {
    invalid_teams <- setdiff(teams, teams_all)
    if (length(invalid_teams) > 0) {
      warning("The following teams are not in the data: ", paste(invalid_teams, collapse = ", "))
      teams <- setdiff(teams, invalid_teams)
    }
    if (length(teams) == 0) {
      stop("No valid teams specified in 'teams'.")
    }
    # Create a mapping from team names to indices
    team_map <- setNames(seq_along(teams_all), teams_all)
  }

  # Parameters to exclude from name replacement
  exclude_params <- all_param_names[!all_param_names %in% c("att_raw", "def_raw", "att", "def")]

  # Extract posterior summaries
  cat("Posterior summaries for model parameters:\n")

  if (is.null(final_pars)) {
    stan_summary <- rstan::summary(x$fit, ...)$summary
  } else {
    stan_summary <- rstan::summary(x$fit, pars = unique(final_pars), ...)$summary
  }

  # If 'teams' is specified, filter parameters related to these teams
  if (!is.null(teams)) {
    # Generate regex patterns for the specified teams and relevant parameters
    patterns <- unlist(lapply(teams, function(team) {
      idx <- team_map[[team]]
      c(
        paste0("^att_raw\\[", idx, "\\]$"),
        paste0("^def_raw\\[", idx, "\\]$"),
        paste0("^att\\[", idx, "\\]$"),
        paste0("^def\\[", idx, "\\]$")
      )
    }))
    # Filter 'stan_summary' to include only the specified teams' parameters
    stan_summary <- stan_summary[grep(paste(patterns, collapse = "|"), rownames(stan_summary)), , drop = FALSE]
  }

  # Replace parameter names with team names if 'true_names' is TRUE
  if (true_names && !is.null(stan_summary) && nrow(stan_summary) > 0) {
    # Create reverse mapping from index to team names
    team_map_rev <- setNames(teams_all, as.character(seq_along(teams_all)))

    # Apply the provided 'team_names_2' function
    stan_summary_names <- team_names(rownames(stan_summary), exclude_params, team_map_rev)

    # Remove trailing commas before the closing bracket
    stan_summary_names <- gsub(",\\]", "]", stan_summary_names)

    # Assign the cleaned names back to the summary
    rownames(stan_summary) <- stan_summary_names
  }

  # Check if there are parameters to display
  if (!is.null(stan_summary) && nrow(stan_summary) > 0) {
    # Round the summary to the specified number of digits and print
    print(round(stan_summary, digits = digits))
  } else {
    cat("No parameters to display.\n")
  }

  invisible(x)
}





#' Print Method for btdFoot Objects
#'
#' Provides detailed posterior summaries for the Bayesian Bradley-Terry-Davidson model parameters.
#'
#' @param x An object of class \code{btdFoot}.
#' @param pars Optional character vector specifying parameters to include in the summary (e.g., \code{"logStrength"}, \code{"logTie"}, \code{"home"}, \code{"log_lik"}, and \code{"y_rep"}).
#' @param teams Optional character vector specifying team names whose \code{logStrength} parameters should be displayed.
#' @param digits Number of significant digits to use when printing numeric values.
#' @param true_names Logical value indicating whether to display team names in parameter summaries. Default is \code{TRUE}.
#' @param display Character string specifying which parts of the output to display. Options are \code{"both"}, \code{"rankings"}, or \code{"parameters"}. Default is \code{"both"}.
#' @param ... Additional arguments passed.
#' @method print btdFoot
#' @export

print.btdFoot <- function(x, pars = NULL, teams = NULL, digits = 3, true_names = TRUE,
                          display = c("both", "rankings", "parameters"), ...) {
  if (!inherits(x, "btdFoot")) {
    stop("The object must be of class 'btdFoot'.")
  }

  if (!is.numeric(digits) || digits <= 0) {
    stop("'digits' must be a positive numeric value.")
  }

  display <- match.arg(display)

  cat("Bayesian Bradley-Terry-Davidson model\n")
  cat("------------------------------------------------\n")
  cat("Rank measure used:", x$rank_measure, "\n\n")

  if (display %in% c("both", "rankings")) {
    # Display the ranking table
    cat("Top teams based on relative log-strengths:\n")
    top_teams <- x$rank[order(-x$rank$log_strengths), ]
    print(utils::head(top_teams, 10), digits = digits)
    cat("\n")
  }

  if (display %in% c("both", "parameters")) {
    # Ensure 'x$fit' is a 'stanfit' object
    if (!inherits(x$fit, "stanfit")) {
      stop("The 'fit' component must be a 'stanfit' object.")
    }

    # Extract unique team names from data
    teams_all <- sort(unique(c(as.character(x$data$home_team), as.character(x$data$away_team))))
    team_map <- stats::setNames(seq_along(teams_all), teams_all)  # Names: team names, Values: indices
    team_map_rev <- stats::setNames(teams_all, seq_along(teams_all))  # Names: indices, Values: team names

    # Parameters to exclude from name replacement
    exclude_params <- c("log_lik", "y_rep")

    # Extract posterior summaries
    cat("Posterior summaries for model parameters:\n")
    stan_summary <- rstan::summary(x$fit, ...)$summary

    # Filter by 'pars' if provided
    if (!is.null(pars)) {
      pattern <- paste0("^", pars, collapse = "|")
      stan_summary <- stan_summary[grep(pattern, rownames(stan_summary)), , drop = FALSE]
    }

    # Split stan_summary into logStrength_params and other_params
    param_names <- rownames(stan_summary)
    logStrength_pattern <- "^logStrength"

    is_logStrength <- grepl(logStrength_pattern, param_names)
    logStrength_params <- stan_summary[is_logStrength, , drop = FALSE]
    other_params <- stan_summary[!is_logStrength, , drop = FALSE]

    # Determine if the model is dynamic or static
    is_dynamic <- !is.null(x$stan_data$ntimes_rank)

    # Filter logStrength_params by 'teams' if provided
    if (!is.null(teams) && nrow(logStrength_params) > 0) {
      # Ensure teams are valid
      invalid_teams <- setdiff(teams, teams_all)
      if (length(invalid_teams) > 0) {
        warning("The following teams are not in the data: ", paste(invalid_teams, collapse = ", "))
        teams <- setdiff(teams, invalid_teams)
      }

      if (length(teams) > 0) {
        # Map team names to indices
        team_indices <- team_map[teams]
        # Create patterns to match logStrength parameters for the specified teams
        if (is_dynamic) {
          # Dynamic model
          patterns <- paste0("logStrength\\[", team_indices, ",\\d+\\]")
        } else {
          # Static model
          patterns <- paste0("logStrength\\[", team_indices, "\\]")
        }

        # Combine patterns
        pattern <- paste0("^", patterns, collapse = "|")

        # Filter logStrength_params
        logStrength_param_names <- rownames(logStrength_params)
        logStrength_params <- logStrength_params[grep(pattern, logStrength_param_names), , drop = FALSE]
      } else {
        logStrength_params <- NULL
      }
    }

    # Combine logStrength_params and other_params back
    if (!is.null(logStrength_params) && !is.null(other_params)) {
      stan_summary <- rbind(logStrength_params, other_params)
    } else if (!is.null(logStrength_params)) {
      stan_summary <- logStrength_params
    } else if (!is.null(other_params)) {
      stan_summary <- other_params
    } else {
      stan_summary <- NULL
    }

    # Replace parameter names with team names if true_names is TRUE
    if (true_names && !is.null(stan_summary) && nrow(stan_summary) > 0) {
      # Apply the function to row names
      new_param_names <- team_names(rownames(stan_summary), exclude_params, team_map_rev)

      # Remove trailing commas in static model parameter names
      if (!is_dynamic) {
        new_param_names <- gsub(",\\]", "]", new_param_names)
      }

      rownames(stan_summary) <- new_param_names
    }

    if (!is.null(stan_summary) && nrow(stan_summary) > 0) {
      print(round(stan_summary, digits = digits))
    } else {
      cat("No parameters to display.\n")
    }
  }

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

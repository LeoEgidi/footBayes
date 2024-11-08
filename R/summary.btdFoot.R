#' Summary Method for btdFoot Objects
#'
#' Provides detailed posterior summaries for the Bayesian Bradley-Terry-Davidson model parameters.
#'
#' @param object An object of class \code{btdFoot}.
#' @param pars Optional character vector specifying parameters to include in the summary. If \code{NULL}, all parameters are included.
#' @param digits Number of significant digits to use when printing numeric values.
#' @param ... Additional arguments.
#' @method summary btdFoot
#' @export
summary.btdFoot <- function(object, pars = NULL, digits = 2, ...) {
  if (!inherits(object, "btdFoot")) {
    stop("The object must be of class 'btdFoot'.")
  }

  cat("Summary of Bayesian Bradley-Terry-Davidson Model\n")
  cat("------------------------------------------------\n")
  cat("Rank Measure Used:", object$rank_measure, "\n\n")

  # Display the ranking table
  cat("Top Teams Based on Ranking Points:\n")
  print(utils::head(object$rank[order(-object$rank$rank_points), ], 10), digits = digits)
  cat("\n")

  # Ensure that 'object$fit' is a 'stanfit' object
  if (!inherits(object$fit, "stanfit")) {
    stop("The 'fit' component must be a 'stanfit' object.")
  }

  # Extract posterior summaries
  cat("Posterior Summaries for Model Parameters:\n")
  stan_summary <- rstan::summary(object$fit, pars = pars, ...)$summary

  # Check if any parameters were not found
  if (!is.null(pars)) {
    available_pars <- rownames(stan_summary)
    missing_pars <- setdiff(pars, available_pars)
    if (length(missing_pars) > 0) {
      warning("The following parameters were not found in the model output: ",
              paste(missing_pars, collapse = ", "))
    }
  }

  # Format and print the summary table
  print(round(stan_summary, digits = digits))


  invisible(object)
}




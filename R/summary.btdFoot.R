#' Summary Method for btdFoot Objects
#'
#' Provides detailed posterior summaries for the Bayesian Bradley-Terry-Davidson model parameters.
#'
#' @param object An object of class \code{btdFoot}.
#' @param ... Additional arguments.
#' @method summary btdFoot
#' @export
summary.btdFoot <- function(object, ...) {
  if (!inherits(object, "btdFoot")) {
    stop("Object must be of class 'btdFoot'.")
  }

  cat("Summary of Bayesian Bradley-Terry-Davidson Model\n")
  cat("Rank Measure:", object$rank_measure, "\n\n")

  print(utils::head(object$rank, 10))


  cat("Posterior Summaries for Model Parameters:\n")

  # Ensure that 'object$fit' is a 'stanfit' object
  if (!inherits(object$fit, "stanfit")) {
    stop("'fit' component must be a 'stanfit' object.")
  }


  # Extract additional arguments
  args <- list(...)
  pars <- NULL

  if (!is.null(args$pars)) {
    pars <- args$pars
  }

  stan_summary <- rstan::summary(object$fit)$summary

  if (!is.null(pars)) {
    # Check if specified pars exist in the summary
    missing_pars <- setdiff(pars, rownames(stan_summary))
    if (length(missing_pars) > 0) {
      warning("The following parameters are not found in the stanfit object: ",
              paste(missing_pars, collapse = ", "))
    }
    # Subset to include only existing pars
    stan_summary <- stan_summary[rownames(stan_summary) %in% pars, , drop = FALSE]
  }

  print(stan_summary)

  invisible(object)
}

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

  cat("Posterior Summaries for Model Parameters:\n")

  # Ensure that 'object$fit' is a 'stanfit' object
  if (!inherits(object$fit, "stanfit")) {
    stop("'fit' component must be a 'stanfit' object.")
  }

  stan_summary <- rstan::summary(object$fit)$summary
  print(stan_summary)

  invisible(object)
}

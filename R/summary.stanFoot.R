#' Summary Method for stanFoot Objects
#'
#' Provides detailed posterior summaries for the Stan football model parameters.
#'
#' @param object An object of class \code{stanFoot}.
#' @param ... Additional arguments.
#' @method summary stanFoot
#' @export
summary.stanFoot <- function(object, ...) {
  if (!inherits(object, "stanFoot")) {
    stop("Object must be of class 'stanFoot'.")
  }

  cat("Summary of Stan Football Model\n\n")

  cat("Posterior Summaries for Model Parameters:\n")

  # Ensure that 'object$fit' is a 'stanfit' object
  if (!inherits(object$fit, "stanfit")) {
    stop("'fit' component must be a 'stanfit' object.")
  }

  stan_summary <- rstan::summary(object$fit)$summary
  print(stan_summary)

  invisible(object)
}


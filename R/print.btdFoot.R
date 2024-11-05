#' Print Method for btdFoot Objects
#'
#' Displays a summary of the Bayesian Bradley-Terry-Davidson model fit, including rank measures and a summary of the Stan fit.
#'
#' @param x An object of class \code{btdFoot}.
#' @param ... Additional arguments passed to or from other methods.
#' @method print btdFoot
#' @export
print.btdFoot <- function(x, ...) {
  if (!inherits(x, "btdFoot")) {
    stop("Object must be of class 'btdFoot'.")
  }

  cat("Bayesian Bradley-Terry-Davidson Model Fit\n")
  cat("Rank Measure:", x$rank_measure, "\n\n")

  cat("Team Rankings:\n")
  print(utils::head(x$rank, 10))  # Display first 10 rankings for brevity

  cat("Posterior Summaries for Model Parameters:\n")

  # Ensure that 'x$fit' is a 'stanfit' object
  if (!inherits(x$fit, "stanfit")) {
    stop("'fit' component must be a 'stanfit' object.")
  }

  stan_summary <- rstan::summary(x$fit)$summary
  print(stan_summary[, c("mean", "sd", "2.5%", "97.5%", "Rhat")])

  invisible(x)
}


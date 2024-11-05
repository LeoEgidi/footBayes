#' Print Method for stanFoot Objects
#'
#' Displays a summary of the Stan football model fit, including posterior summaries of parameters.
#'
#' @param x An object of class \code{stanFoot}.
#' @param ... Additional arguments passed to or from other methods.
#' @method print stanFoot
#' @export
print.stanFoot <- function(x, ...) {
  if (!inherits(x, "stanFoot")) {
    stop("Object must be of class 'stanFoot'.")
  }

  cat("Stan Football Model Fit\n\n")

  cat("Stan Fit Summary:\n")

  # Ensure that 'x$fit' is a 'stanfit' object
  if (!inherits(x$fit, "stanfit")) {
    stop("'fit' component must be a 'stanfit' object.")
  }

  stan_summary <- rstan::summary(x$fit)$summary
  print(stan_summary[, c("mean", "sd", "2.5%", "97.5%", "Rhat")])

  invisible(x)
}

#' Print Method for stanFoot Objects
#'
#' Displays a summary of the Stan football model fit, including posterior summaries of parameters.
#'
#' @param x An object of class \code{stanFoot}.
#' @param ... Additional arguments passed to or from other methods, including \code{pars} to specify parameters.
#' @method print stanFoot
#'@author Roberto Macrì Demartino \email{roberto.macridemartino@phd.unipd.it}.
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

  # Extract additional arguments
  args <- list(...)
  pars <- NULL

  if (!is.null(args$pars)) {
    pars <- args$pars
  }

  # Get the summary from stanfit
  stan_summary <- rstan::summary(x$fit)$summary

  # If 'pars' is specified, filter the summary
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

  # Select desired columns to display
  desired_cols <- c("mean", "sd", "2.5%", "97.5%", "Rhat")

  # Check if desired columns exist
  missing_cols <- setdiff(desired_cols, colnames(stan_summary))
  if (length(missing_cols) > 0) {
    warning("The following summary columns are missing: ",
            paste(missing_cols, collapse = ", "))
    desired_cols <- intersect(desired_cols, colnames(stan_summary))
  }

  # Print the summary table
  print(stan_summary[, desired_cols, drop = FALSE])

  invisible(x)
}

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

  # Ensure that 'object$fit' is a 'stanfit' object
  if (!inherits(object$fit, "stanfit")) {
    stop("'fit' component must be a 'stanfit' object.")
  }


  cat("Posterior Summaries for Model Parameters:\n")

  # Extract additional arguments
  args <- list(...)
  pars <- NULL

  if (!is.null(args$pars)) {
    pars <- args$pars
  }

  # Get the summary from stanfit
  stan_summary <- rstan::summary(object$fit)$summary

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

  invisible(object)
}


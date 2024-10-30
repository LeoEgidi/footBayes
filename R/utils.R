#' Check Prior Value (Internal function)
#'
#' Validates the prior value to ensure it meets the necessary requirements.
#'
#' This function checks if the provided prior value is numeric and of length one.
#' If `positive` is set to \code{TRUE}, it will also ensure that the value is positive.
#'
#' @param prior_value The prior value to validate. Must be a numeric value of length one.
#' @param prior_name A string representing the name of the prior, used for error messages.
#' @param positive Logical value indicating whether the prior must be positive. Defaults to \code{FALSE}.
#' @return No return value. The function will throw an error if validation fails.
#' @examples
#' check_prior(5, "alpha", positive = TRUE)
#' check_prior(-1, "beta", positive = FALSE)
#' @noRd

check_prior <- function(prior_value, prior_name, positive = FALSE) {
  # Check if the prior is numeric and of length 1
  if (!is.numeric(prior_value) || length(prior_value) != 1) {
    stop(sprintf("The prior '%s' must be a single numeric value.", prior_name))
  }
  # Check if the prior is positive if required
  if (positive && prior_value <= 0) {
    stop(sprintf("The prior '%s' must be a positive numeric value.", prior_name))
  }
}


#' Compute the Maximum A Posteriori (MAP) Estimate (Internal function)
#'
#' Computes the Maximum A Posteriori (MAP) estimate from a set of samples using kernel density estimation.
#'
#' This function calculates the MAP estimate, which is the mode of the estimated density, for a given vector of samples.
#'
#' @param samples A numeric vector of samples from which to compute the MAP estimate.
#' @return A numeric value representing the MAP estimate.
#' @examples
#' samples <- rnorm(1000, mean = 5, sd = 2)
#' compute_MAP(samples)
#' @importFrom stats density
#' @noRd

compute_MAP <- function(samples) {
  dens <- stats::density(samples)
  MAP <- dens$x[which.max(dens$y)]
  return(MAP)
}



compute_MAP <- function(samples) {
  dens <- stats::density(samples)
  MAP <- dens$x[which.max(dens$y)]
  return(MAP)
}

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



#' Normalize Rank Points (Internal Function)
#'
#' Normalizes a vector of rank points using the specified method.
#'
#' This function applies one of several normalization methods to a numeric vector of rank points.
#' Available methods are:
#' \itemize{
#'   \item \code{"none"}: No normalization is applied.
#'   \item \code{"standard"}: Standardizes the data to have mean 0 and standard deviation 0.5.
#'   \item \code{"mad"}: Normalizes using the median absolute deviation.
#'   \item \code{"min_max"}: Scales the data to be between 0 and 1.
#' }
#'
#' @param rank_points A numeric vector of rank points to normalize.
#' @param method A string specifying the normalization method. Options are \code{"none"}, \code{"standard"}, \code{"mad"}, or \code{"min_max"}.
#'
#' @return A numeric vector of normalized rank points.
#'
#' @noRd
normalize_rank_points <- function(rank_points, method) {
  if (method == "none") {
    rank_points
  } else if (method == "standard") {
    s <- stats::sd(rank_points, na.rm = TRUE)
    m <- mean(rank_points, na.rm = TRUE)
    if (s == 0) {
      rep(0, length(rank_points))
    } else {
      (rank_points - m) / (2 * s)
    }
  } else if (method == "mad") {
    md <- stats::mad(rank_points, na.rm = TRUE)
    med <- stats::median(rank_points, na.rm = TRUE)
    if (md == 0) {
      rep(0, length(rank_points))
    } else {
      (rank_points - med) / md
    }
  } else if (method == "min_max") {
    min_rp <- min(rank_points, na.rm = TRUE)
    max_rp <- max(rank_points, na.rm = TRUE)
    if (max_rp == min_rp) {
      rep(0, length(rank_points))
    } else {
      (rank_points - min_rp) / (max_rp - min_rp)
    }
  } else {
    stop("Invalid normalization method specified.")
  }
}














#' Replace Indices with Team Names (Internal Function)
#'
#' Substitutes numeric indices in parameter names with corresponding team names based on a predefined mapping.
#'
#'
#' @param param_names A character vector of parameter names that may contain numeric indices to be replaced.
#' @param exclude_params A character vector of parameter name prefixes that should be excluded from renaming. Parameters starting with any of these prefixes will remain unchanged.
#' @param index_map A named character vector where names correspond to numeric indices and values correspond to the team names they should be replaced with.
#'
#' @return A character vector with numeric indices replaced by their corresponding team names where applicable.
#'
#' @noRd
team_names <- function(param_names, exclude_params, team_map) {
  sapply(param_names, function(name) {
    # Skip parameters that should not have their names changed
    if (any(startsWith(name, exclude_params))) {
      return(name)
    }

    # Match patterns like 'logStrength[1]', 'logStrength[1,1]', etc.
    pattern <- "\\[(\\d+)(?:,(\\d+))*\\]"
    if (grepl(pattern, name)) {
      matches <- regmatches(name, regexec(pattern, name))
      index1 <- matches[[1]][2]
      index2 <- matches[[1]][3]  # May be NULL if there's no second index
      new_name <- name
      if (!is.na(index1) && index1 %in% names(team_map)) {
        team_name <- team_map[[index1]]
        if (!is.na(index2)) {
          new_name <- sub(pattern, paste0("[", team_name, ",", index2, "]"), name)
        } else {
          new_name <- sub(pattern, paste0("[", team_name, "]"), name)
        }
      }
      return(new_name)
    }
    return(name)
  }, USE.NAMES = FALSE)
}

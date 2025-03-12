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
team_names <- function(param_names, exclude_params, team_map_rev) {
  sapply(param_names, function(name) {
    # Skip parameters that should not have their names changed
    if (any(startsWith(name, exclude_params))) {
      return(name)
    }

    # Attempt to match two indices: [time, team]
    pattern_two <- "\\[(\\d+),(\\d+)\\]"
    if (grepl(pattern_two, name)) {
      matches <- regmatches(name, regexec(pattern_two, name))


      if (length(matches[[1]]) >= 3) {
        time_index <- matches[[1]][2] # First index (time)
        team_index <- matches[[1]][3] # Second index (team)

        # Check if the second index corresponds to a team
        if (team_index %in% names(team_map_rev)) {
          team_name <- team_map_rev[[team_index]]
          # Replace the second index with the team name

          new_name <- sub(paste0(",", team_index, "\\]"), paste0(", ", team_name, "]"), name)
          return(new_name)
        }
      }
    }

    # If not matched with two indices, attempt to match single index: [team]
    pattern_one <- "\\[(\\d+)\\]"
    if (grepl(pattern_one, name)) {
      matches <- regmatches(name, regexec(pattern_one, name))
      # Ensure that the single index is captured
      if (length(matches[[1]]) >= 2) {
        team_index <- matches[[1]][2] # Single index (team)

        # Check if the index corresponds to a team
        if (team_index %in% names(team_map_rev)) {
          team_name <- team_map_rev[[team_index]]
          # Replace the index with the team name
          new_name <- sub(paste0("\\[", team_index, "\\]"), paste0("[", team_name, "]"), name)
          return(new_name)
        }
      }
    }

    return(name)
  }, USE.NAMES = FALSE)
}

#' Compute the Ranked Probability Score (RPS) (Internal Function)
#'
#' Computes the Ranked Probability Score (RPS) from cumulative predicted probabilities
#' and actual outcomes. The RPS is calculated as the mean of the squared differences
#' between the cumulative predicted probabilities (for the first two outcome categories)
#' and the corresponding cumulative observed probabilities.
#'
#' @param cum_pred A numeric matrix of cumulative predicted probabilities for each observation.
#'   It should have at least two columns; the probability for the final outcome is assumed to be 1.
#' @param actual A character vector of actual outcomes. Each element should be one of
#'   \code{"Home Win"}, \code{"Draw"}, or \code{"Away Win"}.
#'
#' @return A numeric value representing the mean Ranked Probability Score.
#'
#' @noRd
compute_RPS <- function(cum_pred, actual) {
  # Create a matrix of cumulative observed probabilities
  acum <- matrix(0, nrow = length(actual), ncol = 3)
  acum[actual == "Home Win", ] <- matrix(rep(c(1, 1, 1), sum(actual == "Home Win")),
                                         ncol = 3, byrow = TRUE
  )
  acum[actual == "Draw", ] <- matrix(rep(c(0, 1, 1), sum(actual == "Draw")),
                                     ncol = 3, byrow = TRUE
  )
  acum[actual == "Away Win", ] <- matrix(rep(c(0, 0, 1), sum(actual == "Away Win")),
                                         ncol = 3, byrow = TRUE
  )
  squared_diff <- (cum_pred[, 1:2] - acum[, 1:2])^2
  rps_per_obs <- rowSums(squared_diff) / 2 # for 3 outcome categories: (3 - 1)
  mean(rps_per_obs)
}


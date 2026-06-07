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


#' Simulate Goals from MLE Model Parameters (Internal Function)
#'
#' Generates simulated home and away goal counts from fitted MLE model parameters.
#' This is the core prediction engine used by \code{foot_prob} and other downstream functions.
#'
#' @param model A character string specifying the model type. One of
#'   \code{"double_pois"}, \code{"biv_pois"}, \code{"dixon_coles"},
#'   \code{"neg_bin"}, \code{"skellam"}, or \code{"student_t"}.
#' @param team1_prev Integer vector of home team indices for out-of-sample matches.
#' @param team2_prev Integer vector of away team indices for out-of-sample matches.
#' @param att A matrix of attack estimates (rows = teams, columns = 2.5\%, mle, 97.5\%).
#' @param def A matrix of defence estimates (rows = teams, columns = 2.5\%, mle, 97.5\%).
#' @param home A matrix of home effect estimates (1 row, columns = 2.5\%, mle, 97.5\%).
#' @param n_prev Integer, the number of out-of-sample matches.
#' @param n_iter Integer, the number of Monte Carlo simulations per match.
#' @param corr Optional matrix of bivariate Poisson correlation estimates (for \code{"biv_pois"}).
#' @param ability Optional matrix of combined ability estimates (for \code{"student_t"}).
#' @param sigma_y Optional numeric scale parameter (for \code{"student_t"}).
#' @param rho_par Optional matrix of Dixon-Coles rho estimates (for \code{"dixon_coles"}).
#' @param overdispersion Optional matrix of NB overdispersion estimates (for \code{"neg_bin"}).
#'
#' @return A list with components:
#' \itemize{
#'   \item \code{x}: Matrix of simulated home goals (\code{n_iter} x \code{n_prev}).
#'     For \code{"skellam"} and \code{"student_t"}, this contains goal differences.
#'   \item \code{y}: Matrix of simulated away goals (\code{n_iter} x \code{n_prev}).
#'     For \code{"skellam"} and \code{"student_t"}, this is a zero matrix.
#' }
#'
#' @importFrom extraDistr rbvpois rskellam
#' @importFrom metRology rt.scaled
#' @importFrom stats rpois rnbinom
#' @noRd
simulate_goals_mle <- function(model,
                               team1_prev,
                               team2_prev,
                               att,
                               def,
                               home,
                               n_prev,
                               n_iter,
                               corr = NULL,
                               ability = NULL,
                               sigma_y = NULL,
                               rho_par = NULL,
                               overdispersion = NULL) {
  # Compute expected goals from MLE point estimates
  mean_home <- exp(home[1, 2] + att[team1_prev, 2] + def[team2_prev, 2])
  mean_away <- exp(att[team2_prev, 2] + def[team1_prev, 2])

  if (model == "double_pois") {
    x <- y <- matrix(NA, n_iter, n_prev)
    for (n in seq_len(n_prev)) {
      x[, n] <- rpois(n_iter, mean_home[n])
      y[, n] <- rpois(n_iter, mean_away[n])
    }
  } else if (model == "biv_pois") {
    couple <- array(NA, c(n_iter, n_prev, 2))
    for (n in seq_len(n_prev)) {
      couple[, n, ] <- rbvpois(n_iter,
        a = mean_home[n],
        b = mean_away[n],
        c = corr[1, 2]
      )
    }
    x <- couple[, , 1]
    y <- couple[, , 2]
  } else if (model == "dixon_coles") {
    # Dixon-Coles: simulate from independent Poissons,
    # then apply tau-based weighted resampling
    rho_val <- rho_par[1, 2]
    x <- y <- matrix(NA, n_iter, n_prev)
    for (n in seq_len(n_prev)) {
      # Over-sample to account for resampling
      n_sample <- n_iter * 3
      x_cand <- rpois(n_sample, mean_home[n])
      y_cand <- rpois(n_sample, mean_away[n])

      # Compute Dixon-Coles tau weights
      tau <- rep(1, n_sample)
      idx_00 <- (x_cand == 0) & (y_cand == 0)
      idx_10 <- (x_cand == 1) & (y_cand == 0)
      idx_01 <- (x_cand == 0) & (y_cand == 1)
      idx_11 <- (x_cand == 1) & (y_cand == 1)
      tau[idx_00] <- 1 - mean_home[n] * mean_away[n] * rho_val
      tau[idx_10] <- 1 + mean_away[n] * rho_val
      tau[idx_01] <- 1 + mean_home[n] * rho_val
      tau[idx_11] <- 1 - rho_val
      tau <- pmax(tau, 0)

      # Weighted resampling
      if (sum(tau) > 0) {
        sel <- sample(n_sample, n_iter, replace = TRUE, prob = tau)
        x[, n] <- x_cand[sel]
        y[, n] <- y_cand[sel]
      } else {
        # Fallback to plain Poisson if tau is degenerate
        x[, n] <- rpois(n_iter, mean_home[n])
        y[, n] <- rpois(n_iter, mean_away[n])
      }
    }
  } else if (model == "neg_bin") {
    phi1 <- overdispersion[1, 2] # home dispersion (size parameter)
    phi2 <- overdispersion[2, 2] # away dispersion (size parameter)
    x <- y <- matrix(NA, n_iter, n_prev)
    for (n in seq_len(n_prev)) {
      x[, n] <- rnbinom(n_iter, mu = mean_home[n], size = phi1)
      y[, n] <- rnbinom(n_iter, mu = mean_away[n], size = phi2)
    }
  } else if (model == "skellam") {
    diff_y <- matrix(NA, n_iter, n_prev)
    for (n in seq_len(n_prev)) {
      diff_y[, n] <- rskellam(n_iter,
        mu1 = mean_home[n],
        mu2 = mean_away[n]
      )
    }
    x <- diff_y
    y <- matrix(0, n_iter, n_prev)
  } else if (model == "student_t") {
    diff_y <- matrix(NA, n_iter, n_prev)
    for (n in seq_len(n_prev)) {
      diff_y[, n] <- rt.scaled(n_iter,
        df = 7,
        mean = home[1, 2] + ability[team1_prev[n], 2] - ability[team2_prev[n], 2],
        sd = sigma_y
      )
    }
    x <- round(diff_y)
    y <- matrix(0, n_iter, n_prev)
  } else {
    stop(sprintf("Unknown model '%s' in simulate_goals_mle.", model))
  }

  return(list(x = x, y = y))
}



#   ____________________________________________________________________________
#   Functions used by mle_foot()                                            ####

# The helpers below implement the parameter unpacking, the model-specific
# negative log-likelihoods, the conditional-likelihood interval and the output
# tables used by mle_foot(). They are kept here, alongside the other internal
# helpers of the package, and are not exported.

#' Rebuild a team-level parameter block (Internal function)
#'
#' Reconstructs a named, length-\code{nteams} vector for a team-level block
#' (attack, defence or ability) from the \code{optim} parameter vector, adding
#' the dropped reference team via the sum-to-zero constraint. Matching on the
#' \code{"<prefix>."} stem prevents team names that contain the substrings
#' \code{"att"}/\code{"def"} from being captured by accident.
#'
#' @param parameters Named numeric vector of free parameters.
#' @param prefix Character stem of the block (\code{"att"}, \code{"def"} or \code{"ability"}).
#' @param teams Character vector of team names (full length, reference team first).
#' @return A named numeric vector of length \code{nteams}, or \code{NULL} if the
#'   block is absent from \code{parameters}.
#' @noRd
mle_team_block <- function(parameters, prefix, teams) {
  vals <- parameters[startsWith(names(parameters), paste0(prefix, "."))]
  if (length(vals) == 0) {
    return(NULL)
  }
  setNames(c(-sum(vals), vals), teams)
}

#' Unpack the optim parameter vector into a named list (Internal function)
#'
#' @param parameters Named numeric vector of free parameters.
#' @param teams Character vector of team names.
#' @return A list with elements \code{att}, \code{def}, \code{ability},
#'   \code{home}, \code{const}, \code{rho}, \code{phi1}, \code{phi2} (absent
#'   blocks are \code{NULL}).
#' @noRd
mle_relist_params <- function(parameters, teams) {
  list(
    att     = mle_team_block(parameters, "att", teams),
    def     = mle_team_block(parameters, "def", teams),
    ability = mle_team_block(parameters, "ability", teams),
    home    = parameters["home"],
    const   = parameters["const"],
    rho     = parameters["rho"],
    phi1    = parameters["phi1"],
    phi2    = parameters["phi2"]
  )
}

#' Double Poisson negative log-likelihood (Internal function)
#' @importFrom stats dpois
#' @noRd
mle_double_pois_lik <- function(parameters, y1, y2, team1, team2, teams) {
  p <- mle_relist_params(parameters, teams)
  theta_1 <- exp(p$home + p$att[team1] + p$def[team2])
  theta_2 <- exp(p$att[team2] + p$def[team1])
  -sum(dpois(y1, theta_1, log = TRUE) + dpois(y2, theta_2, log = TRUE))
}

#' Bivariate Poisson negative log-likelihood (Internal function)
#' @importFrom extraDistr dbvpois
#' @noRd
mle_biv_pois_lik <- function(parameters, y1, y2, team1, team2, teams) {
  p <- mle_relist_params(parameters, teams)
  theta_1 <- exp(p$home + p$att[team1] + p$def[team2])
  theta_2 <- exp(p$att[team2] + p$def[team1])
  theta_3 <- exp(p$const)
  -sum(dbvpois(y1, y2, a = theta_1, b = theta_2, c = theta_3, log = TRUE))
}

#' Skellam negative log-likelihood (Internal function)
#' @importFrom extraDistr dskellam
#' @noRd
mle_skellam_lik <- function(parameters, y1, y2, team1, team2, teams) {
  p <- mle_relist_params(parameters, teams)
  theta_1 <- exp(p$home + p$att[team1] + p$def[team2])
  theta_2 <- exp(p$att[team2] + p$def[team1])
  -sum(dskellam(y1 - y2, mu1 = theta_1, mu2 = theta_2, log = TRUE))
}

#' Student's t negative log-likelihood (Internal function)
#'
#' Uses a single ability rating per team; \code{sigma_y} is a fixed scale.
#' @importFrom metRology dt.scaled
#' @noRd
mle_student_t_lik <- function(parameters, y1, y2, team1, team2, teams, sigma_y) {
  p <- mle_relist_params(parameters, teams)
  ability <- p$ability
  -sum(dt.scaled(
    x = y1 - y2, df = 7,
    mean = p$home + ability[team1] - ability[team2],
    sd = sigma_y, log = TRUE
  ))
}

#' Dixon-Coles negative log-likelihood (Internal function)
#'
#' Double Poisson with the low-score dependence adjustment.
#' @importFrom stats dpois
#' @noRd
mle_dixon_coles_lik <- function(parameters, y1, y2, team1, team2, teams) {
  p <- mle_relist_params(parameters, teams)
  rho <- as.numeric(p$rho)
  theta_1 <- exp(p$home + p$att[team1] + p$def[team2])
  theta_2 <- exp(p$att[team2] + p$def[team1])

  tau <- rep(1, length(y1))
  idx_00 <- (y1 == 0) & (y2 == 0)
  idx_10 <- (y1 == 1) & (y2 == 0)
  idx_01 <- (y1 == 0) & (y2 == 1)
  idx_11 <- (y1 == 1) & (y2 == 1)
  tau[idx_00] <- 1 - theta_1[idx_00] * theta_2[idx_00] * rho
  tau[idx_10] <- 1 + theta_2[idx_10] * rho
  tau[idx_01] <- 1 + theta_1[idx_01] * rho
  tau[idx_11] <- 1 - rho
  tau <- pmax(tau, .Machine$double.eps) # guard against log of non-positive tau

  -sum(dpois(y1, theta_1, log = TRUE) + dpois(y2, theta_2, log = TRUE) + log(tau))
}

#' Negative binomial negative log-likelihood (Internal function)
#'
#' NB2 parameterisation with separate dispersion per margin (log scale).
#' @importFrom stats dnbinom
#' @noRd
mle_neg_bin_lik <- function(parameters, y1, y2, team1, team2, teams) {
  p <- mle_relist_params(parameters, teams)
  phi1 <- exp(as.numeric(p$phi1))
  phi2 <- exp(as.numeric(p$phi2))
  theta_1 <- exp(p$home + p$att[team1] + p$def[team2])
  theta_2 <- exp(p$att[team2] + p$def[team1])
  -sum(dnbinom(y1, mu = theta_1, size = phi1, log = TRUE) +
    dnbinom(y2, mu = theta_2, size = phi2, log = TRUE))
}

#' Conditional-likelihood interval for one parameter (Internal function)
#'
#' Varies the \code{j}-th parameter on a fixed grid while holding the others at
#' their MLE, and returns the 95\% interval defined by the
#' \code{qchisq(0.95, 1) / 2} log-likelihood drop.
#'
#' @param j Index of the parameter of interest in \code{par_hat}.
#' @param par_hat Named numeric vector of MLEs.
#' @param fn Negative log-likelihood with signature \code{fn(parameters, team1, team2, y1, y2)}.
#' @param mle_value Maximised log-likelihood at \code{par_hat}.
#' @param team1,team2 Integer team indices for the in-sample matches.
#' @param y1,y2 Numeric goal vectors for the in-sample matches.
#' @return A numeric vector \code{c(lower, upper)} (\code{NA} if the threshold is never met).
#' @importFrom stats qchisq
#' @noRd
mle_conditional_interval <- function(j, par_hat, fn, mle_value, team1, team2, y1, y2) {
  g <- Vectorize(function(x) {
    p <- par_hat
    p[j] <- x
    -fn(p, team1 = team1, team2 = team2, y1 = y1, y2 = y2)
  }, "x")
  h <- mle_value - qchisq(0.95, 1) / 2
  grid <- seq(-5, 5, 0.01)
  valid <- grid[g(grid) >= h]
  if (length(valid) == 0) c(NA, NA) else c(min(valid), max(valid))
}

#' Team-level confidence-interval table (Internal function)
#'
#' Builds an \code{nteams x 3} table (lower, MLE, upper) for a team-level block.
#' The reference team carries no interval, so its bounds equal the point estimate.
#'
#' @param prefix Character stem of the block (\code{"att"}, \code{"def"} or \code{"ability"}).
#' @param par_hat Named numeric vector of MLEs.
#' @param par_names Names of \code{par_hat}.
#' @param ci Two-column matrix of lower/upper bounds, rows named as \code{par_names}.
#' @param teams Character vector of team names.
#' @param digits Number of decimals for rounding.
#' @return A numeric matrix with columns \code{"2.5\%"}, \code{"mle"}, \code{"97.5\%"}.
#' @noRd
mle_loc_table <- function(prefix, par_hat, par_names, ci, teams, digits = 2) {
  sel <- startsWith(par_names, paste0(prefix, "."))
  vals <- par_hat[sel]
  point <- c(-sum(vals), vals)
  lo <- c(point[1], ci[sel, 1])
  hi <- c(point[1], ci[sel, 2])
  tab <- cbind(round(lo, digits), round(point, digits), round(hi, digits))
  dimnames(tab) <- list(teams, c("2.5%", "mle", "97.5%"))
  tab
}

#' Scalar-parameter confidence-interval table (Internal function)
#'
#' Builds a \code{1 x 3} table for a scalar parameter, optionally transformed
#' (e.g. \code{exp()} to map a log-scale estimate back to its natural scale).
#'
#' @param name Name of the parameter in \code{par_hat}/\code{ci}.
#' @param par_hat Named numeric vector of MLEs.
#' @param ci Two-column matrix of lower/upper bounds, rows named as \code{par_hat}.
#' @param transform Function applied to estimate and bounds (default \code{identity}).
#' @param digits Number of decimals for rounding.
#' @param rows Optional row name(s) for the returned table.
#' @return A numeric matrix with columns \code{"2.5\%"}, \code{"mle"}, \code{"97.5\%"}.
#' @noRd
mle_scalar_table <- function(name, par_hat, ci, transform = identity, digits = 2, rows = NULL) {
  out <- rbind(c(
    round(transform(ci[name, 1]), digits),
    round(transform(par_hat[name]), digits),
    round(transform(ci[name, 2]), digits)
  ))
  dimnames(out) <- list(rows, c("2.5%", "mle", "97.5%"))
  out
}

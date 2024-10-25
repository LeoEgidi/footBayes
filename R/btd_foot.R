#' Bayesian Bradley-Terry-Davidson Model
#'
#' This function fits a Bayesian (time-dependent) Bradley-Terry-Davidson model using Stan.
#'
#' @param data A data frame containing the observations with columns:
#'   \itemize{
#'     \item \code{periods}: Time point of each observation (integer >= 1)
#'     \item \code{team1}: Index of team 1 in each observation (integer >= 1)
#'     \item \code{team2}: Index of team 2 in each observation (integer >= 1)
#'     \item \code{match_outcome}: Outcome (\code{1} if team1 beats team2, \code{2} for tie, and \code{3} if team2 beats team1)
#'   }
#' @param dynamic_rank Logical; if \code{TRUE}, uses a dynamic ranking model (default is \code{FALSE}).
#' @param priors A list containing prior parameters with elements:
#'   \itemize{
#'     \item \code{mean_psi}: Initial mean for psi (numeric, default is 0)
#'     \item \code{std_psi}: Standard deviation of the AR(1) process (positive numeric, default is 3)
#'     \item \code{mean_gamma}: Mean for gamma (numeric, default is 0)
#'     \item \code{std_gamma}: Standard deviation for gamma (positive numeric, default is 0.3)
#'   }
#' @param stan_code Stan model code as a string.
#' @param ... Additional arguments passed to \code{\link[rstan]{stan}} (e.g., \code{iter}, \code{chains}, \code{control})
#'
#' @return An object of class \code{stanfit} returned by \code{\link[rstan]{stan}}
#'
#' @examples
#' \dontrun{
#' # Example data
#' set.seed(123)
#' N <- 100
#' K <- 5
#' time_points <- 10
#' data <- data.frame(
#'   periods = sample(1:time_points, N, replace = TRUE),
#'   team1 = sample(1:K, N, replace = TRUE),
#'   team2 = sample(1:K, N, replace = TRUE),
#'   match_outcome = sample(1:3, N, replace = TRUE)
#' )
#' # Define your Stan model code
#' stan_code <- "data { /* Stan code goes here */ }"
#' fit <- btd_stan(
#'   data,
#'   dynamic_rank = TRUE,
#'   priors = list(mean_psi = 0, std_psi = 1, mean_gamma = 0, std_gamma = 1),
#'   stan_code = stan_code,
#'   iter = 1000,
#'   chains = 2
#' )
#' }


btd_stan <- function(data,
                     dynamic_rank = FALSE,
                     priors = list(),
                     ...) {
  
  # Default values for priors if not provided
  default_mean_psi <- 0
  default_std_psi <- 3
  default_mean_gamma <- 0
  default_std_gamma <- 0.3
  
  # Extract prior parameters from the priors list or assign defaults
  mean_psi <- if (is.null(priors$mean_psi)) default_mean_psi else priors$mean_psi
  std_psi <- if (is.null(priors$std_psi)) default_std_psi else priors$std_psi
  mean_gamma <- if (is.null(priors$mean_gamma)) default_mean_gamma else priors$mean_gamma
  std_gamma <- if (is.null(priors$std_gamma)) default_std_gamma else priors$std_gamma
  
  # Check that data is a data frame
  if (!is.data.frame(data)) {
    stop("Data must be a data frame.")
  }
  
  # Check that required columns are present
  required_cols <- c("periods", "team1", "team2", "match_outcome")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Data is missing required columns:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
  
  # Extract variables
  instants_rank <- data$periods
  team1 <- data$team1
  team2 <- data$team2
  match_outcome <- data$match_outcome
  
  N <- nrow(data)
  
  # Define nteams and time_points
  nteams <- length(unique(c(team1, team2)))
  time_points <- max(instants_rank)
  ntimes_rank <- length(unique(instants_rank))
  
  
  # NAs
  if (any(is.na(data))) {
    stop("Data contains missing values. Please handle them before fitting the model.")
  }
  
  # Check periods
  if (!is.numeric(instants_rank) || any(instants_rank != as.integer(instants_rank))) {
    stop("Column 'periods' must contain integer values.")
  }
  if (any(instants_rank < 1 | instants_rank > time_points)) {
    stop(paste("Values in 'periods' must be between 1 and", time_points, "."))
  }
  
  # Check team1 and team2
  if (!is.numeric(team1) || any(team1 != as.integer(team1))) {
    stop("Column 'team1' must contain integer values.")
  }
  if (any(team1 < 1 | team1 > nteams)) {
    stop(paste("Values in 'team1' must be between 1 and", nteams, "."))
  }
  if (!is.numeric(team2) || any(team2 != as.integer(team2))) {
    stop("Column 'team2' must contain integer values.")
  }
  if (any(team2 < 1 | team2 > nteams)) {
    stop(paste("Values in 'team2' must be between 1 and", nteams, "."))
  }
  
  # Check match_outcome
  if (!is.numeric(match_outcome) || any(match_outcome != as.integer(match_outcome))) {
    stop("Column 'match_outcome' must contain integer values.")
  }
  if (any(!match_outcome %in% c(1, 2, 3))) {
    stop("Values in 'match_outcome' must be 1, 2, or 3.")
  }
  
  # Check mean_psi
  if (!is.numeric(mean_psi) || length(mean_psi) != 1) {
    stop("'mean_psi' must be a numeric value.")
  }
  
  # Check std_psi
  if (!is.numeric(std_psi) || length(std_psi) != 1 || std_psi <= 0) {
    stop("'std_psi' must be a positive numeric value.")
  }
  
  # Check mean_gamma
  if (!is.numeric(mean_gamma) || length(mean_gamma) != 1) {
    stop("'mean_gamma' must be a numeric value.")
  }
  
  # Check std_gamma
  if (!is.numeric(std_gamma) || length(std_gamma) != 1 || std_gamma <= 0) {
    stop("'std_gamma' must be a positive numeric value.")
  }
  
  # Prepare data for Stan based on dynamic_rank
  if (!dynamic_rank) {
    stan_data <- list(
      N = N,
      nteams = nteams,
      team1 = as.integer(team1),
      team2 = as.integer(team2),
      mean_psi = mean_psi,
      std_psi = std_psi,
      mean_gamma = mean_gamma,
      std_gamma = std_gamma,
      match_outcome = as.integer(match_outcome)
    )
  } else {
    stan_data <- list(
      N = N,
      nteams = nteams,
      ntimes_rank = ntimes_rank,
      instants_rank = as.integer(instants_rank),
      team1 = as.integer(team1),
      team2 = as.integer(team2),
      mean_psi = mean_psi,
      std_psi = std_psi,
      mean_gamma = mean_gamma,
      std_gamma = std_gamma,
      match_outcome = as.integer(match_outcome)
    )
  }
  
  # Fit the model using Stan
  fit <- rstan::stan(model_code = stan_code, data = stan_data, ...)
  
  result <- list(
    fit = fit,
    data = data,
    stan_data = stan_data,
    priors = list(
      mean_psi = mean_psi,
      std_psi = std_psi,
      mean_gamma = mean_gamma,
      std_gamma = std_gamma
    )
  )
  class(result) <- "footBayesBTD"
  return(result)
}



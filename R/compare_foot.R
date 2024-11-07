#' Compare Football Models using Various Metrics
#'
#' Compares multiple football models or directly provided probability matrices based on specified metrics (accuracy, Brier score, Ranked Probability Score (RPS), Cox-Snell Pseudo $R^2$, Average Coverage Probability, and/or McFadden's Pseudo $R^2$), using a test dataset.
#'
#' @param source A named list containing either:
#'   \itemize{
#'     \item Fitted model objects (of class \code{stanFoot} or \code{stanfit}), each representing a football model.
#'     \item Matrices where each matrix contains the estimated probabilities for "Home Win," "Draw," and "Away Win" in its columns.
#'   }
#' @param test_data A data frame containing the test dataset, with columns:
#'   \itemize{
#'     \item \code{home_team}: Home team's name (character string).
#'     \item \code{away_team}: Away team's name (character string).
#'     \item \code{homegoals}: Goals scored by the home team (integer >= 0).
#'     \item \code{awaygoals}: Goals scored by the away team (integer >= 0).
#'   }
#' @param metric A character vector specifying the metrics to use for comparison. Options are:
#'   \itemize{
#'     \item \code{"accuracy"}: Computes the accuracy of each model.
#'     \item \code{"brier"}: Computes the Brier score of each model.
#'     \item \code{"RPS"}: Computes the Ranked Probability Score (RPS) for each model.
#'     \item \code{"ACP"}: Computes the Average Coverage Probability (ACP) for each model.
#'     \item \code{"pseudoR2"}: Computes the Pseudo $R^2$, defined as the geometric mean of the probabilities assigned to the actual results.
#'   }
#'   Default is \code{c("accuracy", "brier", "ACP", "pseudoR2", "RPS")}, computing the specified metrics.
#'
#' @return A data frame containing the metric values for each model or probability matrix.
#'
#' @details The function extracts predictions from each model or directly uses the provided probability matrices and computes the chosen metrics on the test dataset.
#'
#' @examples
#' \dontrun{
#'
#' require(dplyr)
#'
#' data("italy")
#' italy_2000 <- italy %>%
#'  dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
#'  dplyr::filter(Season == "2000")
#'
#' # Example with a fitted model
#' fit <- stan_foot(data = italy_2000,
#'                  model = "double_pois", predict = 18)  # double poisson model
#'
#' italy_2000_test <- italy_2000[289:306, ]
#'
#' colnames(italy_2000_test) <- c("season", "home_team", "away_team", "homegoals", "awaygoals")
#'
#' compare_results_models <- compare_foot(
#'   source = list(model_1 = fit),
#'   test_data = italy_2000_test,
#'   metric = c("accuracy", "brier", "ACP", "pseudoR2", "RPS")
#' )
#'
#' print(compare_results_models)
#'
#' # Example with probability matrices
#'
#' home_team <- c("AC Milan", "Inter", "Juventus", "AS Roma", "Napoli",
#'                "Lazio", "Atalanta", "Fiorentina", "Torino", "Sassuolo", "Udinese")
#'
#' away_team <- c("Juventus", "Napoli", "Inter", "Atalanta", "Lazio",
#'                "AC Milan", "Sassuolo", "Torino", "Fiorentina", "Udinese", "AS Roma")
#'
#' # Home and Away goals based on given data
#' homegoals <- c(2, 0, 2, 2, 3, 1, 4, 2, 1, 1, 2)
#' awaygoals <- c(1, 0, 1, 3, 2, 1, 1, 2, 1, 1, 2)
#'
#' # Combine into a data frame
#' test_data <- data.frame(home_team, away_team, homegoals, awaygoals)
#'
#' # Define the data for each column
#' pW <- c(0.51, 0.45, 0.48, 0.53, 0.56, 0.39, 0.52, 0.55, 0.61, 0.37, 0.35)
#' pD <- c(0.27, 0.25, 0.31, 0.18, 0.23, 0.30, 0.24, 0.26, 0.18, 0.19, 0.22)
#' pL <- c(0.22, 0.30, 0.21, 0.29, 0.21, 0.31, 0.24, 0.19, 0.21, 0.44, 0.43)
#'
#' # Create the data frame table_prob
#' table_prob <- data.frame(pW, pD, pL)
#' matrix_prob <- as.matrix(table_prob)
#'
#' # Use compare_foot function
#' compare_results_matrices <- compare_foot(
#'   source = list(matrix_1 = matrix_prob),
#'   test_data = test_data,
#'   metric = c("accuracy", "brier", "pseudoR2", "ACP", "RPS")
#' )
#'
#' # Print the results
#' print(compare_results_matrices)
#' }
#' @export

compare_foot <- function(source, test_data, metric = c("accuracy", "brier", "ACP", "pseudoR2", "RPS")) {

  # Validate metric
  allowed_metrics <- c("accuracy", "brier", "ACP", "pseudoR2", "RPS")
  metric <- match.arg(metric, choices = allowed_metrics, several.ok = TRUE)

  # Validate data
  required_cols <- c("home_team", "away_team", "homegoals", "awaygoals")
  missing_cols <- setdiff(required_cols, names(test_data))
  if (length(missing_cols) > 0) {
    stop(paste("test_data is missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  test_data$outcome <- ifelse(test_data$homegoals > test_data$awaygoals, 1,
                              ifelse(test_data$homegoals == test_data$awaygoals, 2, 3))
  test_data$outcome <- factor(test_data$outcome, levels = 1:3, labels = c("Home Win", "Draw", "Away Win"))

  N_prev <- nrow(test_data)

  # Function to compute RPS
  compute_RPS <- function(cum_pred, actual) {
    # cum_pred: matrix of cumulative predicted probabilities
    # actual: vector of actual outcomes as factors


    # Cumulative observed probabilities
    acum <- matrix(0, nrow = N_prev, ncol = 3)
    acum[test_data$outcome == "Home Win", ] <- matrix(rep(c(1, 1, 1), sum(test_data$outcome == "Home Win")), ncol = 3, byrow = TRUE)
    acum[test_data$outcome == "Draw", ]      <- matrix(rep(c(0, 1, 1), sum(test_data$outcome == "Draw")), ncol = 3, byrow = TRUE)
    acum[test_data$outcome == "Away Win", ] <- matrix(rep(c(0, 0, 1), sum(test_data$outcome == "Away Win")), ncol = 3, byrow = TRUE)

    # Squared differences for m=1 and m=2
    squared_diff <- (cum_pred[, 1:2] - acum[, 1:2])^2

    # RPS per observation
    rps_per_obs <- rowSums(squared_diff) / 2  # For 3 categories: n_cat - 1

    # Average RPS over all observations
    mean(rps_per_obs)
  }

  results <- list()

  for (item_name in names(source)) {
    item <- source[[item_name]]

    model_results <- list()

    if (inherits(item, c("stanFoot", "stanfit"))) {

      # Check that model is of class stanFoot or stanfit
      if (!inherits(item, c("stanFoot", "stanfit"))) {
        warning(paste("Source item", item_name, "is not of class 'stanFoot' or 'stanfit'. Skipping."))
        next
      }

      if (inherits(item, "stanFoot")) {
        sims <- rstan::extract(item$fit)
      } else if (inherits(item, "stanfit")) {
        sims <- rstan::extract(item)
      }

      # Check 'y_prev' is in the samples
      if (!"y_prev" %in% names(sims)) {
        warning(paste("Model", item_name, "does not contain 'y_prev' in its samples. Ensure the model was fitted with 'predict' set appropriately. Skipping."))
        next
      }

      S <- dim(sims$y_prev)[1]
      N_model <- dim(sims$y_prev)[2]

      if (N_model != N_prev) {
        warning(paste("Number of matches in 'y_prev' for model", item_name, "does not match the number of matches in 'test_data'. Skipping."))
        next
      }

      # Predicted probabilities matrix
      prob_q_model <- matrix(NA, N_prev, 3)

      for (n in 1:N_prev) {
        prob_q_model[n, 1] <- sum(sims$y_prev[, n, 1] > sims$y_prev[, n, 2]) / S
        prob_q_model[n, 2] <- sum(sims$y_prev[, n, 1] == sims$y_prev[, n, 2]) / S
        prob_q_model[n, 3] <- sum(sims$y_prev[, n, 1] < sims$y_prev[, n, 2]) / S
      }

      # Ensure probabilities sum to 1
      prob_q_model <- prob_q_model / rowSums(prob_q_model)

    } else if (is.matrix(item)) {
      # Probability matrices
      prob_q_model <- item

      # Validate matrix dimensions
      if (nrow(prob_q_model) != N_prev) {
        warning(paste("Probability matrix", item_name, "does not have the correct number of rows. Expected", N_prev, "but got", nrow(prob_q_model), ". Skipping."))
        next
      }

      if (ncol(prob_q_model) != 3) {
        warning(paste("Probability matrix", item_name, "does not have exactly 3 columns (Home Win, Draw, Away Win). Skipping."))
        next
      }

      # Ensure probabilities sum to 1
      row_sums <- rowSums(prob_q_model)
      if (any(abs(row_sums - 1) > 1e-6)) {
        warning(paste("Probabilities in matrix", item_name, "do not sum to 1. Normalizing the probabilities."))
        prob_q_model <- prob_q_model / row_sums
      }

    } else {
      warning(paste("Source item", item_name, "is neither a model object nor a probability matrix. Skipping."))
      next
    }


    # Compute cumulative predicted probabilities for RPS
    cum_pred <- t(apply(prob_q_model, 1, cumsum))  # N_prev x 3

    # Compute RPS if requested
    if ("RPS" %in% metric) {
      RPS <- compute_RPS(cum_pred, test_data$outcome)
      model_results$RPS <- round(RPS, 4)
    }

    # Compute Accuracy if requested
    if ("accuracy" %in% metric) {
      predicted_outcomes <- apply(prob_q_model, 1, which.max)
      accuracy <- mean(predicted_outcomes == as.numeric(test_data$outcome))
      model_results$accuracy <- round(accuracy, 4)
    }

    # Compute Brier Score if requested
    if ("brier" %in% metric) {
      brier_res <- matrix(0, N_prev, 3)
      for (n in 1:N_prev) {
        brier_res[n, as.numeric(test_data$outcome[n])] <- 1
      }
      brier <- mean(rowSums((brier_res - prob_q_model)^2))
      model_results$brier <- round(brier, 4)
    }

    # Compute Pseudo R2 if requested
    if ("pseudoR2" %in% metric) {
      log_prob_sum <- 0
      for (n in 1:N_prev) {
        prob_n <- prob_q_model[n, as.numeric(test_data$outcome[n])]
        prob_n <- max(prob_n, .Machine$double.eps)  # Avoid log(0)
        log_prob_sum <- log_prob_sum + log(prob_n)
      }
      pseudoR2 <- exp(log_prob_sum / N_prev)
      model_results$pseudoR2 <- round(pseudoR2, 4)
    }

    # Compute Average Coverage Probability if requested
    if ("ACP" %in% metric) {
      true_probs <- numeric(N_prev)
      for (n in 1:N_prev) {
        true_class <- as.numeric(test_data$outcome[n])
        true_probs[n] <- prob_q_model[n, true_class]
      }
      ACP <- mean(true_probs)
      model_results$ACP <- round(ACP, 4)
    }

    results[[item_name]] <- model_results
  }

  if (length(results) == 0) {
    stop("No valid models or probability matrices were provided in 'source'.")
  }

  # Convert results to data frame
  results_df <- do.call(rbind, lapply(names(results), function(item_name) {
    cbind(Model = item_name, as.data.frame(results[[item_name]]))
  }))

  rownames(results_df) <- NULL

  return(results_df)
}

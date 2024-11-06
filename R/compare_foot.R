#' Compare Football Models using Various Metrics
#'
#' Compares multiple football models based on specified metrics (accuracy, Brier score, Cox-Snell Pseudo $R^2$, Average Coverage Probability, and/or McFadden's Pseudo $R^2$), using a test dataset.
#'
#' @param models A named list of fitted model objects (of class \code{stanFoot} or \code{stanfit}), each representing a football model.
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
#'     \item \code{"mcFaddenR2"}: Computes McFadden's Pseudo $R^2$ for each model.
#'     \item \code{"coxSnellR2"}: Computes the Cox-Snell Pseudo $R^2$ for each model.
#'     \item \code{"ACP"}: Computes the Average Coverage Probability (ACP) for each model.
#'   }
#'   Default is \code{c("accuracy", "brier", "mcFaddenR2", "coxSnellR2", "ACP")}, computing the specified metrics.
#'
#' @return A data frame containing the metric values for each model.
#'
#' @details The function extracts predictions from each model and computes the chosen metrics on the test dataset.
#'
#' @examples
#' \dontrun{
#'
#' # (The existing examples remain unchanged)
#'
#' # Compare the two models with all metrics including Cox-Snell R² and ACP
#' compare_results <- compare_foot(
#'   models = compare_models,
#'   test_data = italy_2021_test,
#'   metric = c("accuracy", "brier", "mcFaddenR2", "coxSnellR2", "ACP")
#' )
#'
#' print(compare_results)
#' }
#' @export


compare_foot <- function(models, test_data, metric = c("accuracy", "brier", "mcFaddenR2", "coxSnellR2", "ACP")) {

  # Validate metric
  allowed_metrics <- c("accuracy", "brier", "mcFaddenR2", "coxSnellR2", "ACP")
  metric <- match.arg(metric, choices = allowed_metrics, several.ok = TRUE)

  # Validate data
  required_cols <- c("home_team", "away_team", "homegoals", "awaygoals")
  missing_cols <- setdiff(required_cols, names(test_data))
  if (length(missing_cols) > 0) {
    stop(paste("test_data is missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Create outcome variable
  test_data$outcome <- ifelse(test_data$homegoals > test_data$awaygoals, 1,
                              ifelse(test_data$homegoals == test_data$awaygoals, 2, 3))
  test_data$outcome <- factor(test_data$outcome, levels = 1:3, labels = c("Home Win", "Draw", "Away Win"))

  N_prev <- nrow(test_data)

  # Pre-compute log_lik_0 for mcFaddenR2 and Cox-Snell R2
  # The null model assumes that all matches have the same probabilities for each
  # outcome (home win, draw, away win) based only on their empirical frequencies in the test data.

  if (any(c("mcFaddenR2", "coxSnellR2") %in% metric)) {
    p_k <- prop.table(table(test_data$outcome))
    p_k_vector <- numeric(3)
    for (k in 1:3) {
      p_k_vector[k] <- ifelse(!is.na(p_k[as.character(k)]), p_k[as.character(k)], 1e-10)
    }

    prob_observed_null <- numeric(N_prev)
    for (n in 1:N_prev) {
      k <- as.numeric(test_data$outcome[n])
      prob_n <- p_k_vector[k]
      prob_n <- max(prob_n, .Machine$double.eps)
      prob_observed_null[n] <- prob_n
    }
    log_lik_0 <- sum(log(prob_observed_null))
  }

  results <- list()

  for (model_name in names(models)) {
    model <- models[[model_name]]

    # Check that model is of class stanFoot or stanfit
    if (!inherits(model, c("stanFoot", "stanfit"))) {
      warning(paste("Model", model_name, "is not of class 'stanFoot' or 'stanfit'. Skipping."))
      next
    }

    # Extract samples
    if (inherits(model, "stanFoot")) {
      sims <- rstan::extract(model$fit)
    } else if (inherits(model, "stanfit")) {
      sims <- rstan::extract(model)
    }

    # Check 'y_prev' is in the samples
    if (!"y_prev" %in% names(sims)) {
      warning(paste("Model", model_name, "does not contain 'y_prev' in its samples. Ensure the model was fitted with 'predict' set appropriately. Skipping."))
      next
    }

    S <- dim(sims$y_prev)[1]
    N_model <- dim(sims$y_prev)[2]

    if (N_model != N_prev) {
      warning(paste("Number of matches in 'y_prev' for model", model_name, "does not match the number of matches in 'test_data'. Skipping."))
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

    model_results <- list()

    if ("accuracy" %in% metric) {
      predicted_outcomes <- apply(prob_q_model, 1, which.max)
      accuracy <- mean(predicted_outcomes == as.numeric(test_data$outcome))
      model_results$accuracy <- accuracy
    }

    if ("brier" %in% metric) {
      brier_res <- matrix(0, N_prev, 3)
      for (n in 1:N_prev) {
        brier_res[n, as.numeric(test_data$outcome[n])] <- 1
      }
      brier <- mean(rowSums((brier_res - prob_q_model)^2))
      model_results$brier <- brier
    }

    if (any(c("mcFaddenR2", "coxSnellR2") %in% metric)) {
      # Compute log_lik_model
      prob_observed <- numeric(N_prev)
      for (n in 1:N_prev) {
        prob_n <- prob_q_model[n, as.numeric(test_data$outcome[n])]
        prob_n <- max(prob_n, .Machine$double.eps)  # Avoid log(0)
        prob_observed[n] <- prob_n
      }
      log_lik_model <- sum(log(prob_observed))
    }

    if ("mcFaddenR2" %in% metric) {
      # Compute McFadden's Pseudo R^2
      mcFaddenR2 <- 1 - (log_lik_model / log_lik_0)
      model_results$mcFaddenR2 <- mcFaddenR2
    }

    if ("coxSnellR2" %in% metric) {
      # Compute Cox-Snell Pseudo R^2
      coxSnellR2 <- 1 - exp((log_lik_0 - log_lik_model) / N_prev)
      model_results$coxSnellR2 <- coxSnellR2
    }

    if ("ACP" %in% metric) {
      # Compute Average Coverage Probability (ACP)
      # ACP is the average of the predicted probabilities assigned to the true outcomes
      true_probs <- numeric(N_prev)
      for (n in 1:N_prev) {
        true_class <- as.numeric(test_data$outcome[n])
        true_probs[n] <- prob_q_model[n, true_class]
      }
      ACP <- mean(true_probs)
      model_results$ACP <- ACP
    }

    results[[model_name]] <- model_results
  }

  # Convert results to data frame
  results_df <- do.call(rbind, lapply(names(results), function(model_name) {
    cbind(Model = model_name, as.data.frame(results[[model_name]]))
  }))

  rownames(results_df) <- NULL

  return(results_df)
}

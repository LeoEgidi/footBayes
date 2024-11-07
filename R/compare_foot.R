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
#'     \item \code{"ACP"}: Computes the Average Coverage Probability (ACP) for each model.
#'     \item \code{"pseudoR2"}: Computes the Pseudo $R^2$, defined as the geometric mean of the probabilities assigned to the actual results.
#'   }
#'   Default is \code{c("accuracy", "brier", "ACP", "pseudoR2")}, computing the specified metrics.
#'
#' @return A data frame containing the metric values for each model.
#'
#' @details The function extracts predictions from each model and computes the chosen metrics on the test dataset.
#'
#' @examples
#' \dontrun{
#'
#' require(dplyr)
#'
#' data("italy")
#' italy_2000 <- italy %>%
#'  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
#'  dplyr::filter(Season=="2000")
#'
#' fit <- stan_foot(data = italy_2000,
#'                  model="double_pois", predict =18)  # double pois
#'
#' italy_2000_test <- italy_2000[289:306,]
#'
#' colnames(italy_2000_test) <- c("season", "home_team",  "away_team", "homegoals", "awaygoals")
#'
#' compare_results <- compare_foot(
#'   models = list(model_1 = fit),
#'   test_data = italy_2000_test,
#'   metric = c("accuracy", "brier", "ACP", "pseudoR2")
#' )
#'
#' print(compare_results)
#' }
#' @export


compare_foot <- function(models, test_data, metric = c("accuracy", "brier", "ACP", "pseudoR2")) {

  # Validate metric
  allowed_metrics <- c("accuracy", "brier", "ACP", "pseudoR2")
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
      model_results$accuracy <- round(accuracy, 4)
    }

    if ("brier" %in% metric) {
      brier_res <- matrix(0, N_prev, 3)
      for (n in 1:N_prev) {
        brier_res[n, as.numeric(test_data$outcome[n])] <- 1
      }
      brier <- mean(rowSums((brier_res - prob_q_model)^2))
      model_results$brier <- round(brier, 4)
    }

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

    if ("ACP" %in% metric) {
      # Compute Average Coverage Probability (ACP)
      # ACP is the average of the predicted probabilities assigned to the true outcomes
      true_probs <- numeric(N_prev)
      for (n in 1:N_prev) {
        true_class <- as.numeric(test_data$outcome[n])
        true_probs[n] <- prob_q_model[n, true_class]
      }
      ACP <- mean(true_probs)
      model_results$ACP <- round(ACP, 4)
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

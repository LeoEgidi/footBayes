#' Compare Football Models using Various Metrics
#'
#' Compares multiple football models or directly provided probability matrices based on specified metrics (accuracy, Brier score, ranked probability score, Pseudo \eqn{R^2}, average coverage probability), using a test dataset. Additionally, computes the confusion matrices. The function returns an object of class \code{compareFoot}.
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
#'     \item \code{home_goals}: Goals scored by the home team (integer >= 0).
#'     \item \code{away_goals}: Goals scored by the away team (integer >= 0).
#'   }
#' @param metric A character vector specifying the metrics to use for comparison. Options are:
#'   \itemize{
#'     \item \code{"accuracy"}: Computes the accuracy of each model.
#'     \item \code{"brier"}: Computes the Brier score of each model.
#'     \item \code{"RPS"}: Computes the ranked probability score (RPS) for each model.
#'     \item \code{"ACP"}: Computes the average coverage probability (ACP) for each model.
#'     \item \code{"pseudoR2"}: Computes the Pseudo \eqn{R^2}, defined as the geometric mean of the probabilities assigned to the actual results.
#'   }
#'   Default is \code{c("accuracy", "brier", "ACP", "pseudoR2", "RPS")}, computing the specified metrics.
#' @param conf_matrix Logical indicating whether to generate a confusion matrix comparing predicted outcomes against actual outcomes for each model or probability matrix. Default is \code{FALSE}.
#' @return An object of class \code{compare_foot_output}, which is a list containing:
#'   \itemize{
#'     \item \code{metrics}: A data frame containing the metric values for each model or probability matrix.
#'     \item \code{confusion_matrix}: (Optional) A list of confusion matrices for each model or probability matrix, included if \code{conf_matrix = TRUE}.
#'   }
#'   The object has a custom print method for formatted output.
#' @details The function extracts predictions from each model or directly uses the provided probability matrices and computes the chosen metrics on the test dataset. It also possible to compute confusion matrices.
#'
#' @author Roberto Macrì Demartino \email{roberto.macridemartino@phd.unipd.it}
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' data("italy")
#' italy_2000 <- italy %>%
#'   dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
#'   dplyr::filter(Season == "2000")
#'
#' colnames(italy_2000) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")
#'
#' # Example with fitted models
#' fit_1 <- stan_foot(data = italy_2000,
#'                    model = "double_pois", predict = 18)  # Double Poisson model
#' fit_2 <- stan_foot(data = italy_2000,
#'                    model = "biv_pois", predict = 18)     # Bivariate Poisson model
#'
#' italy_2000_test <- italy_2000[289:306, ]
#'
#'
#' compare_results_models <- compare_foot(
#'   source = list(double_poisson = fit_1,
#'                 bivariate_poisson = fit_2),
#'   test_data = italy_2000_test,
#'   metric = c("accuracy", "brier", "ACP", "pseudoR2", "RPS"),
#'   conf_matrix = TRUE
#' )
#'
#' print(compare_results_models)
#' }
#' @export
compare_foot <- function(source, test_data, metric = c("accuracy", "brier", "ACP", "pseudoR2", "RPS"), conf_matrix = FALSE) {

  # Validate metric
  allowed_metrics <- c("accuracy", "brier", "ACP", "pseudoR2", "RPS")
  metric <- match.arg(metric, choices = allowed_metrics, several.ok = TRUE)

  # Validate data
  required_cols <- c("home_team", "away_team", "home_goals", "away_goals")
  missing_cols <- setdiff(required_cols, names(test_data))
  if (length(missing_cols) > 0) {
    stop(paste("test_data is missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Encode actual outcomes
  test_data$outcome <- ifelse(test_data$home_goals > test_data$away_goals, 1,
                              ifelse(test_data$home_goals == test_data$away_goals, 2, 3))
  test_data$outcome <- factor(test_data$outcome, levels = 1:3, labels = c("Home Win", "Draw", "Away Win"))

  N_prev <- nrow(test_data)

  # Function to compute RPS
  compute_RPS <- function(cum_pred, actual) {
    # cum_pred: matrix of cumulative predicted probabilities
    # actual: vector of actual outcomes as factors

    # Cumulative observed probabilities
    acum <- matrix(0, nrow = length(actual), ncol = 3)
    acum[actual == "Home Win", ] <- matrix(rep(c(1, 1, 1), sum(actual == "Home Win")), ncol = 3, byrow = TRUE)
    acum[actual == "Draw", ]      <- matrix(rep(c(0, 1, 1), sum(actual == "Draw")), ncol = 3, byrow = TRUE)
    acum[actual == "Away Win", ] <- matrix(rep(c(0, 0, 1), sum(actual == "Away Win")), ncol = 3, byrow = TRUE)

    # Squared differences for m=1 and m=2
    squared_diff <- (cum_pred[, 1:2] - acum[, 1:2])^2

    # RPS per observation
    rps_per_obs <- rowSums(squared_diff) / 2  # For 3 categories: n_cat - 1

    # Average RPS over all observations
    mean(rps_per_obs)
  }

  # Initialize result containers
  metrics_results <- list()
  confusion_matrices_results <- list()

  for (item_name in names(source)) {
    item <- source[[item_name]]

    model_results <- list()
    confusion_matrix <- NULL

    if (inherits(item, c("stanFoot", "stanfit"))) {

      # Check that model is of class stanFoot or stanfit
      if (!inherits(item, c("stanFoot", "stanfit"))) {
        warning(paste("Source item", item_name, "is not of class 'stanFoot' or 'stanfit'. Skipping."))
        next
      }

      # Extract samples
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

      # Check for NAs
      na_rows <- apply(prob_q_model, 1, function(x) any(is.na(x)))
      if (any(na_rows)) {
        num_na <- sum(na_rows)
        warning(paste("Probability matrix", item_name, "contains", num_na, "rows with NAs. These rows will be removed from evaluation."))
        prob_q_model <- prob_q_model[!na_rows, , drop = FALSE]
        test_data_subset <- test_data[!na_rows, , drop = FALSE]
        current_N_prev <- nrow(prob_q_model)

        if (current_N_prev == 0) {
          warning(paste("After removing NA rows, no data remains for matrix", item_name, ". Skipping."))
          next
        }

        # Update outcome for remaining data
        actual_outcomes <- test_data_subset$outcome
      } else {
        actual_outcomes <- test_data$outcome
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

    # If item is a matrix and rows were removed, use 'test_data_subset'
    if (is.matrix(item) && exists("test_data_subset")) {
      outcomes <- actual_outcomes
    } else {
      outcomes <- test_data$outcome
    }

    # Compute cumulative predicted probabilities for RPS
    cum_pred <- t(apply(prob_q_model, 1, cumsum))  # N_prev x 3

    # Compute Metrics
    if ("RPS" %in% metric) {
      RPS <- compute_RPS(cum_pred, outcomes)
      model_results$RPS <- round(RPS, 4)
    }

    if ("accuracy" %in% metric) {
      predicted_outcomes <- apply(prob_q_model, 1, which.max)
      accuracy <- mean(predicted_outcomes == as.numeric(outcomes))
      model_results$accuracy <- round(accuracy, 4)
    }

    if ("brier" %in% metric) {
      brier_res <- matrix(0, nrow = nrow(prob_q_model), ncol = 3)
      for (n in 1:nrow(prob_q_model)) {
        brier_res[n, as.numeric(outcomes[n])] <- 1
      }
      brier <- mean(rowSums((brier_res - prob_q_model)^2))
      model_results$brier <- round(brier, 4)
    }

    if ("pseudoR2" %in% metric) {
      log_prob_sum <- 0
      for (n in 1:nrow(prob_q_model)) {
        prob_n <- prob_q_model[n, as.numeric(outcomes[n])]
        prob_n <- max(prob_n, .Machine$double.eps)  # Avoid log(0)
        log_prob_sum <- log_prob_sum + log(prob_n)
      }
      pseudoR2 <- exp(log_prob_sum / nrow(prob_q_model))
      model_results$pseudoR2 <- round(pseudoR2, 4)
    }

    if ("ACP" %in% metric) {
      true_probs <- numeric(nrow(prob_q_model))
      for (n in 1:nrow(prob_q_model)) {
        true_class <- as.numeric(outcomes[n])
        true_probs[n] <- prob_q_model[n, true_class]
      }
      ACP <- mean(true_probs)
      model_results$ACP <- round(ACP, 4)
    }

    # Confusion Matrix
    if (conf_matrix) {
      # Recalculate predicted_classes for each model
      predicted_classes <- apply(prob_q_model, 1, which.max)
      confusion_matrix <- table(
        Predicted = factor(predicted_classes, levels = 1:3, labels = c("Home Win", "Draw", "Away Win")),
        Actual = outcomes
      )
      confusion_matrices_results[[item_name]] <- confusion_matrix
    }

    # Store metrics
    metrics_results[[item_name]] <- model_results
  }

  if (length(metrics_results) == 0) {
    stop("No valid models or probability matrices were provided in 'source'.")
  }

  # Convert metrics to data frame
  metrics_df <- do.call(rbind, lapply(names(metrics_results), function(item_name) {
    cbind(Model = item_name, as.data.frame(metrics_results[[item_name]]))
  }))

  rownames(metrics_df) <- NULL

  # Output list
  output_list <- list(
    metrics = metrics_df
  )

  if (conf_matrix && length(confusion_matrices_results) > 0) {
    output_list$confusion_matrix <- confusion_matrices_results
  }

  # Assign custom class to the output
  class(output_list) <- "compareFoot"

  return(output_list)
}







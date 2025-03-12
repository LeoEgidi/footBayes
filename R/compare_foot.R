#' Compare Football Models using Various Metrics
#'
#' Compares multiple football models or directly provided probability matrices based on specified metrics (accuracy, Brier score, ranked probability score, Pseudo \eqn{R^2}, average coverage probability), using a test dataset. Additionally, computes the confusion matrices. The function returns an object of class \code{compareFoot}.
#'
#' @param source A named list containing either:
#'   \itemize{
#'     \item Fitted model objects (of class  \code{stanFoot}, \code{CmdStanFit}, \code{\link[rstan]{stanfit}}), each representing a football model.
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
#' @param conf_matrix A logical value indicating whether to generate a confusion matrix comparing predicted outcomes against actual outcomes for each model or probability matrix. Default is \code{FALSE}.
#' @return An object of class \code{compare_foot_output}, which is a list containing:
#'   \itemize{
#'     \item \code{metrics}: A data frame containing the metric values for each model or probability matrix.
#'     \item \code{confusion_matrix}: Confusion matrices for each model or probability matrix.
#'   }
#' @details The function extracts predictions from each model or directly uses the provided probability matrices and computes the chosen metrics on the test dataset. It also possible to compute confusion matrices.
#'
#' @author Roberto Macrì Demartino \email{roberto.macridemartino@deams.units.it}
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
#' fit_1 <- stan_foot(
#'   data = italy_2000,
#'   model = "double_pois", predict = 18
#' ) # Double Poisson model
#' fit_2 <- stan_foot(
#'   data = italy_2000,
#'   model = "biv_pois", predict = 18
#' ) # Bivariate Poisson model
#'
#' italy_2000_test <- italy_2000[289:306, ]
#'
#'
#' compare_results_models <- compare_foot(
#'   source = list(
#'     double_poisson = fit_1,
#'     bivariate_poisson = fit_2
#'   ),
#'   test_data = italy_2000_test,
#'   metric = c("accuracy", "brier", "ACP", "pseudoR2", "RPS"),
#'   conf_matrix = TRUE
#' )
#'
#' print(compare_results_models)
#'
#'
#' # Example with probability matrices
#'
#' home_team <- c(
#'   "AC Milan", "Inter", "Juventus", "AS Roma", "Napoli",
#'   "Lazio", "Atalanta", "Fiorentina", "Torino", "Sassuolo", "Udinese"
#' )
#'
#' away_team <- c(
#'   "Juventus", "Napoli", "Inter", "Atalanta", "Lazio",
#'   "AC Milan", "Sassuolo", "Torino", "Fiorentina", "Udinese", "AS Roma"
#' )
#'
#' # Home and Away goals based on given data
#' home_goals <- c(2, 0, 2, 2, 3, 1, 4, 2, 1, 1, 2)
#' away_goals <- c(1, 0, 1, 3, 2, 1, 1, 2, 1, 1, 2)
#'
#' # Combine into a data frame
#' test_data <- data.frame(home_team, away_team, home_goals, away_goals)
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
#' # Print the results
#' print(compare_results_matrices)
#' }
#' @export

compare_foot <- function(source,
                         test_data,
                         metric = c("accuracy", "brier", "ACP", "pseudoR2", "RPS"),
                         conf_matrix = FALSE) {
  #   ____________________________________________________________________________
  #   Data Checks                                                             ####

  required_cols <- c("home_team", "away_team", "home_goals", "away_goals")
  missing_cols <- setdiff(required_cols, names(test_data))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "test_data is missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  #   ____________________________________________________________________________
  #   Metrics' Name Checks                                                    ####

  allowed_metrics <- c("accuracy", "brier", "ACP", "pseudoR2", "RPS")
  metric <- match.arg(metric, choices = allowed_metrics, several.ok = TRUE)


  # Encode actual outcomes: 1 = Home Win, 2 = Draw, 3 = Away Win
  test_data$outcome <- ifelse(test_data$home_goals > test_data$away_goals, 1,
    ifelse(test_data$home_goals == test_data$away_goals, 2, 3)
  )
  test_data$outcome <- factor(test_data$outcome,
    levels = 1:3,
    labels = c("Home Win", "Draw", "Away Win")
  )
  actual_outcomes <- test_data$outcome

  N_prev <- nrow(test_data)

  # Initialize list for results
  metrics_results <- list()
  confusion_matrices_results <- list()



  #   ____________________________________________________________________________
  #   Extract Predicted Outcomes                                              ####

  # Iterate over each model/probability matrix in 'source'
  for (item_name in names(source)) {
    item <- source[[item_name]]
    model_results <- list()

    if (inherits(item, c("stanFoot", "stanfit", "CmdStanFit"))) {
      if (inherits(item, "stanfit")) {
        # For stanfit objects, use rstan::extract
        sims <- rstan::extract(item)
        if (!("y_prev" %in% names(sims) || "diff_y_prev" %in% names(sims))) {
          warning(sprintf("Model '%s' does not contain 'y_prev' or 'diff_y_prev' in its samples. Ensure the model was fitted with 'predict' set appropriately. Skipping.", item_name))
          next
        }
      } else {
        # For stanFoot and CmdStanFit objects, extract draws using posterior
        draws <- if (inherits(item, "stanFoot")) {
          item$fit$draws()
        } else {
          item$draws() # for CmdStanFit objects
        }
        draws <- posterior::as_draws_rvars(draws)
        if (!("y_prev" %in% names(draws) || "diff_y_prev" %in% names(draws))) {
          warning(sprintf("Model '%s' does not contain 'y_prev' or 'diff_y_prev' in its samples. Ensure the model was fitted with 'predict' set appropriately. Skipping.", item_name))
          next
        }
        sims <- list()
        if ("y_prev" %in% names(draws)) {
          sims$y_prev <- posterior::draws_of(draws[["y_prev"]])
        }
        if ("diff_y_prev" %in% names(draws)) {
          sims$diff_y_prev <- posterior::draws_of(draws[["diff_y_prev"]])
        }
      }

      # Check that the number of matches agrees with N_prev
      if ("y_prev" %in% names(sims)) {
        S <- dim(sims$y_prev)[1]
        N_model <- dim(sims$y_prev)[2]
      } else if ("diff_y_prev" %in% names(sims)) {
        S <- dim(sims$diff_y_prev)[1]
        N_model <- dim(sims$diff_y_prev)[2]
      }
      if (N_model != N_prev) {
        warning(sprintf("Number of matches in the predictions for model '%s' (%d) does not match the expected number (%d). Skipping.", item_name, N_model, N_prev))
        next
      }

      #   ____________________________________________________________________________
      #   Compute predicted probabilities per match for each outcome              ####

      # Calculate probabilities using column means
      if ("y_prev" %in% names(sims)) {
        # Use y_prev: compare home and away goals from predicted samples.
        prob_home <- colMeans(sims$y_prev[, , 1, drop = FALSE] > sims$y_prev[, , 2, drop = FALSE])
        prob_draw <- colMeans(sims$y_prev[, , 1, drop = FALSE] == sims$y_prev[, , 2, drop = FALSE])
        prob_away <- colMeans(sims$y_prev[, , 1, drop = FALSE] < sims$y_prev[, , 2, drop = FALSE])
        prob_q_model <- cbind(prob_home, prob_draw, prob_away)
      } else if ("diff_y_prev" %in% names(sims)) {
        # Use diff_y_prev: predicted difference between home and away goals.
        # If the difference is:
        #   - > 0: the home team won,
        #   - == 0: the match is drawn,
        #   - < 0: the away team won.
        diff_matrix <- round(sims$diff_y_prev, 0)
        if (length(dim(diff_matrix)) > 2) {
          diff_matrix <- drop(diff_matrix)
        }
        prob_home <- colMeans(diff_matrix > 0)
        prob_draw <- colMeans(diff_matrix == 0)
        prob_away <- colMeans(diff_matrix < 0)
        prob_q_model <- cbind(prob_home, prob_draw, prob_away)
      }

      # Normalize each row
      row_totals <- rowSums(prob_q_model)
      prob_q_model <- prob_q_model / row_totals

    } else if (is.matrix(item)) {
      # For objects that are already probability matrices
      prob_q_model <- item
      if (nrow(prob_q_model) != N_prev) {
        warning(sprintf("Probability matrix '%s' does not have the expected number of rows (%d). Skipping.", item_name, N_prev))
        next
      }
      if (ncol(prob_q_model) != 3) {
        warning(sprintf("Probability matrix '%s' does not have exactly 3 columns (expected: Home Win, Draw, Away Win). Skipping.", item_name))
        next
      }

      # Remove rows with any NA values and update the actual outcomes accordingly
      na_rows <- apply(prob_q_model, 1, function(x) any(is.na(x)))
      if (any(na_rows)) {
        num_na <- sum(na_rows)
        warning(sprintf("Probability matrix '%s' contains %d rows with NAs. These rows will be removed from evaluation.", item_name, num_na))
        prob_q_model <- prob_q_model[!na_rows, , drop = FALSE]
        actual_outcomes <- actual_outcomes[!na_rows]
        if (nrow(prob_q_model) == 0) {
          warning(sprintf("After removing NA rows, no data remains for matrix '%s'. Skipping.", item_name))
          next
        }
      }

      # Ensure each row sums to 1; normalize if necessary
      row_totals <- rowSums(prob_q_model)
      if (any(abs(row_totals - 1) > 1e-6)) {
        warning(sprintf("Probabilities in matrix '%s' do not sum to 1. Normalizing.", item_name))
        prob_q_model <- prob_q_model / row_totals
      }
    } else {
      warning(sprintf("Source item '%s' is neither a model object nor a probability matrix. Skipping.", item_name))
      next
    }


    # Compute cumulative predicted probabilities for RPS
    cum_pred <- t(apply(prob_q_model, 1, cumsum))

    n <- nrow(prob_q_model)
    idx <- cbind(seq_len(n), as.numeric(actual_outcomes))

    #   ____________________________________________________________________________
    #   Compute selected metrics                                               ####

    if ("RPS" %in% metric) {
      model_results$RPS <- round(compute_RPS(cum_pred, actual_outcomes), 4)
    }
    if ("accuracy" %in% metric) {
      predicted_outcomes <- apply(prob_q_model, 1, which.max)
      model_results$accuracy <- round(mean(predicted_outcomes == as.numeric(actual_outcomes)), 4)
    }
    if ("brier" %in% metric) {
      brier_res <- matrix(0, n, 3)
      brier_res[idx] <- 1
      model_results$brier <- round(mean(rowSums((brier_res - prob_q_model)^2)), 4)
    }

    if ("pseudoR2" %in% metric) {
      log_probs <- log(pmax(prob_q_model[idx], .Machine$double.eps))
      model_results$pseudoR2 <- round(exp(mean(log_probs)), 4)
    }

    if ("ACP" %in% metric) {
      true_probs <- prob_q_model[idx]
      model_results$ACP <- round(mean(true_probs), 4)
    }


    #   ____________________________________________________________________________
    #   Confusion matrix                                                        ####

    if (conf_matrix) {
      predicted_classes <- apply(prob_q_model, 1, which.max)
      confusion_matrices_results[[item_name]] <- table(
        Predicted = factor(predicted_classes,
          levels = 1:3,
          labels = c("Home Win", "Draw", "Away Win")
        ),
        Actual = actual_outcomes
      )
    }

    metrics_results[[item_name]] <- model_results
  }

  if (length(metrics_results) == 0) {
    stop("No valid models or probability matrices were provided in 'source'.")
  }


  #   ____________________________________________________________________________
  #   Combine the metrics into a data frame                                   ####

  metrics_df <- do.call(rbind, lapply(names(metrics_results), function(item_name) {
    cbind(Model = item_name, as.data.frame(metrics_results[[item_name]]))
  }))
  rownames(metrics_df) <- NULL

  out_list <- list(metrics = metrics_df)
  if (conf_matrix && length(confusion_matrices_results) > 0) {
    out_list$confusion_matrix <- confusion_matrices_results
  }
  class(out_list) <- "compareFoot"
  return(out_list)
}

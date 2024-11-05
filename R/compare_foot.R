#' Compare Football Models using Accuracy and Brier Score
#'
#' Compares multiple football models based on specified metrics (accuracy and/or Brier score), using a test dataset.
#'
#' @param models A named list of fitted model objects (of class \code{stanFoot}), each representing a football model.
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
#'     \item \code{"pseudoR2"}: Computes the McFadden's Pseudo $R^2$ for each model.
#'   }
#'   Default is \code{c("accuracy", "brier", "pseudoR2")}, computing both metrics.
#'
#' @return A data frame containing the metric values for each model.
#'
#' @details The function extracts predictions from each model and computes the chosen metrics on the test dataset. For accuracy, it computes the proportion of correct predictions. For the Brier score, it computes the mean squared difference between the predicted probabilities and the actual outcomes.
#'
#' @examples
#' \dontrun{
#'
#' Load the dataset
#' data("italy")
#' italy <- as_tibble(italy)
#'
#' # Filter and select the required columns
#' italy_2021_test <- italy %>%
#'   select(Season, home, visitor, hgoal, vgoal) %>%
#'   filter(Season == "2021")
#'
#' # Set the first 340 rows as they are and replace hgoal and vgoal with NA for remaining rows
#' italy_2021 <- italy_2021 %>%
#'   mutate(
#'     hgoal = ifelse(row_number() > 340, NA, hgoal),
#'     vgoal = ifelse(row_number() > 340, NA, vgoal)
#'   )
#'
#' # Create a list of unique teams
#' teams <- unique(italy_2021$home)
#' n_rows <- 20
#'
#' # Create a fake ranking
#' ranking <- data.frame(
#'   periods = rep(1, n_rows),
#'   team = sample(teams, n_rows, replace = FALSE),
#'   rank_points = sample(0:60, n_rows, replace = FALSE)
#' )
#'
#' ranking <- ranking %>%
#'   arrange(periods, desc(rank_points))
#'
#' # Fit a model using stan_foot
#' fit_with_ranking <- stan_foot(
#'   data = italy_2021,
#'   model = "diag_infl_biv_pois",
#'   predict = 40,
#'   ranking = ranking,
#'   prior_par = list(
#'     ability = student_t(4, 0, NULL),
#'     ability_sd = cauchy(0, 3),
#'     home = normal(1, 10)
#'   ),
#'   home_effect = TRUE,
#'   norm_method = "mad",
#'   iter = 1000,
#'   chains = 2,
#'   cores = 2,
#'   control = list(adapt_delta = 0.95, max_treedepth = 15)
#' )
#'
#' # Print a summary of the model fit
#' print(fit_with_ranking$fit)
#'
#' # Create another fake ranking
#' ranking2 <- data.frame(
#'   periods = rep(1, n_rows),
#'   team = sample(teams, n_rows, replace = FALSE),
#'   rank_points = sample(0:100, n_rows, replace = FALSE)
#' )
#'
#' ranking2 <- ranking2 %>%
#'   arrange(periods, desc(rank_points))
#'
#' # Fit another model with a different ranking
#' fit_with_ranking2 <- stan_foot(
#'   data = italy_2021,
#'   model = "biv_pois",
#'   predict = 40,
#'   ranking = ranking2,
#'   prior_par = list(
#'     ability = student_t(4, 0, NULL),
#'     ability_sd = cauchy(0, 3),
#'     home = normal(1, 10)
#'   ),
#'   home_effect = TRUE,
#'   norm_method = "mad",
#'   iter = 1000,
#'   chains = 2,
#'   cores = 2,
#'   control = list(adapt_delta = 0.95, max_treedepth = 15)
#' )
#'
#' # Prepare the test data for comparison
#' data("italy")
#' italy <- as_tibble(italy)
#'
#' italy_2021_test <- italy %>%
#'   select(Season, home, visitor, hgoal, vgoal) %>%
#'   filter(Season == "2021")
#'
#' italy_2021_test <- italy_2021_test[341:380,]
#'
#' # Compare the two models
#' compare_models <- list(model1 = fit_with_ranking,
#'                        model2 = fit_with_ranking2)
#'
#' compare_foot(compare_models, test_data = italy_2021_test)
#' }
#' @export


compare_foot <- function(models, test_data, metric = c("accuracy", "brier", "pseudoR2")) {

  # Validate metric
  allowed_metrics <- c("accuracy", "brier", "pseudoR2")
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
  test_data$outcome <- factor(test_data$outcome, levels = 1:3)

  N_prev <- nrow(test_data)

  # Pre-compute log_lik_0 for pseudo R^2
  # The null model assumes that all matches have the same probabilities for each
  # outcome (home win, draw, away win) based only on their empirical frequencies in the test data.

  if ("pseudoR2" %in% metric) {
    p_k <- prop.table(table(test_data$outcome))
    p_k_vector <- numeric(3)
    for (k in 1:3) {
      p_k_vector[k] <- p_k[as.character(k)]
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

    model_results <- list()

    if ("accuracy" %in% metric) {
      predicted_outcomes <- apply(prob_q_model, 1, function(x) which.max(x))
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

    if ("pseudoR2" %in% metric) {
      # Compute log_lik_model
      prob_observed <- numeric(N_prev)
      for (n in 1:N_prev) {
        prob_n <- prob_q_model[n, as.numeric(test_data$outcome[n])]
        prob_n <- max(prob_n, .Machine$double.eps)  # Avoid log(0)
        prob_observed[n] <- prob_n
      }
      log_lik_model <- sum(log(prob_observed))

      # Compute pseudo R^2
      pseudoR2 <- 1 - (log_lik_model / log_lik_0)

      model_results$pseudoR2 <- pseudoR2
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


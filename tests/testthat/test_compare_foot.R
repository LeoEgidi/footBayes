# all the test PASSED (also the skipped ones!)


#   ____________________________________________________________________________
#   Common matrix data                                                      ####

home_team <- c(
  "AC Milan", "Inter", "Juventus", "AS Roma", "Napoli",
  "Lazio", "Atalanta", "Fiorentina", "Torino", "Sassuolo", "Udinese"
)

away_team <- c(
  "Juventus", "Napoli", "Inter", "Atalanta", "Lazio",
  "AC Milan", "Sassuolo", "Torino", "Fiorentina", "Udinese", "AS Roma"
)

# Home and Away goals based on given data
home_goals <- c(2, 0, 2, 2, 3, 1, 4, 2, 1, 1, 2)
away_goals <- c(1, 0, 1, 3, 2, 1, 1, 2, 1, 1, 2)

# Combine into a data frame
test_data <- data.frame(home_team, away_team, home_goals, away_goals)

# Define the data for each column
pW <- c(0.51, 0.45, 0.48, 0.53, 0.56, 0.39, 0.52, 0.55, 0.61, 0.37, 0.35)
pD <- c(0.27, 0.25, 0.31, 0.18, 0.23, 0.30, 0.24, 0.26, 0.18, 0.19, 0.22)
pL <- c(0.22, 0.30, 0.21, 0.29, 0.21, 0.31, 0.24, 0.19, 0.21, 0.44, 0.43)

# Create the data frame table_prob
table_prob <- data.frame(pW, pD, pL)
matrix_prob <- as.matrix(table_prob)



#   ____________________________________________________________________________
#   Data tests                                                              ####

test_that("Error when test_data is missing required columns", {
  bad_test_data <- test_data[, -1] # Remove 'home_team'
  expect_error(
    compare_foot(source = list(prob1 = matrix_prob), test_data = bad_test_data),
    "test_data is missing required columns"
  )
})

test_that("Error when probability matrix has wrong number of rows", {
  # Create a matrix with fewer rows than test_data
  bad_matrix <- matrix_prob[1:4, ]
  expect_warning(
    expect_error(
      compare_foot(source = list(bad = bad_matrix), test_data = test_data),
      "No valid models or probability matrices were provided"
    )
  )
})

test_that("Error when probability matrix has wrong number of columns", {
  # Create a matrix with only 2 columns
  bad_matrix2 <- matrix_prob[, 1:2]
  expect_warning(
    compare_foot(source = list(good = matrix_prob, bad2 = bad_matrix2), test_data = test_data),
    "does not have exactly 3 columns"
  )
})

test_that("compare_foot removes NA rows from probability matrix", {
  # Introduce NA in row 2 (one of the entries)
  matrix_prob_na <- matrix_prob
  matrix_prob_na[2, 2] <- NA

  expect_warning(
    result <- compare_foot(
      source = list(prob_with_na = matrix_prob_na),
      test_data = test_data
    ),
    "contains 1 rows with NAs"
  )
})


test_that("compare_foot normalizes probability matrix rows not summing to 1", {
  # Create a probability matrix that does NOT sum to 1 by scaling each row by 2.
  matrix_prob_scaled <- matrix_prob * 2

  expect_warning(
    result <- compare_foot(source = list(prob_scaled = matrix_prob_scaled), test_data = test_data),
    "Probabilities in matrix 'prob_scaled' do not sum to 1. Normalizing"
  )
})

# test_that("compare_foot warns and errors when probability matrix becomes empty after removing NA rows", {
#   # Create a probability matrix where every row contains NA values.
#   matrix_all_na <- matrix(NA, nrow = 11, ncol = 3)
#
#   expect_warning(
#     expect_error(
#       compare_foot(source = list(all_na = matrix_all_na), test_data = test_data),
#       "No valid models or probability matrices were provided"
#     ),
#     "After removing NA rows, no data remains for matrix 'all_na'"
#   )
# })

#   ____________________________________________________________________________
#   Models and arguments tests                                              ####

test_that("compare_foot warns and skips stan_foot object without y_prev or diff_y_prev", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####

  data("italy")
  italy_2000 <- italy %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2000")

  colnames(italy_2000) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")

  ##  ............................................................................
  ##  Tests                                                                   ####

  fit_1 <- stan_foot(
    data = italy_2000,
    model = "double_pois", predict = 18,
    iter_sampling = 200,
    chains = 2,
    seed = 433
  ) # Double Poisson model
  fit_2 <- stan_foot(
    data = italy_2000,
    model = "biv_pois",
    iter_sampling = 200,
    chains = 2,
    seed = 433
  ) # Bivariate Poisson model

  fit_3 <- stan_foot(
    data = italy_2000,
    model = "student_t", predict = 18,
    iter_sampling = 200,
    chains = 2,
    seed = 433
  ) # Skellam model

  italy_2000_test <- italy_2000[289:306, ]


  expect_warning(
    expect_error(
      compare_foot(
        source = list(bivariate_poisson = fit_2),
        test_data = italy_2000_test,
        metric = c("accuracy", "brier", "ACP", "pseudoR2", "RPS"),
        conf_matrix = TRUE
      ),
      "No valid models or probability matrices were provided"
    ),
    "does not contain 'y_prev' or 'diff_y_prev'"
  )


  expect_warning(
    compare_foot(
      source = list(double_poisson = fit_1$fit, bivariate_poisson = fit_2, student = fit_3),
      test_data = italy_2000_test,
      metric = c("accuracy", "brier", "ACP", "pseudoR2", "RPS"),
      conf_matrix = TRUE
    ),
    "does not contain 'y_prev' or 'diff_y_prev'"
  )
})


test_that("compare_foot warns and skips source object with different predicted matches and/or not allowed type", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####

  data("italy")
  italy_2000 <- italy %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2000")

  colnames(italy_2000) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")

  ##  ............................................................................
  ##  Tests                                                                   ####

  fit_1 <- stan_foot(
    data = italy_2000,
    model = "double_pois", predict = 18,
    iter_sampling = 200,
    chains = 2,
    seed = 433
  ) # Double Poisson model

  fit_2 <- c(1,2,3,1)

  fit_3 <- stan_foot(
    data = italy_2000,
    model = "student_t", predict = 15,
    iter_sampling = 200,
    chains = 2,
    seed = 433
  ) # Skellam model

  italy_2000_test <- italy_2000[289:306, ]


  expect_warning(
    compare_foot(
      source = list(double_poisson = fit_1$fit, student = fit_3),
      test_data = italy_2000_test,
      metric = c("accuracy", "brier", "ACP", "pseudoR2", "RPS"),
      conf_matrix = TRUE
    ))

  expect_warning(
    compare_foot(
      source = list(double_poisson = fit_1$fit, bivariate_poisson = fit_2),
      test_data = italy_2000_test,
      metric = c("accuracy", "brier", "ACP", "pseudoR2", "RPS"),
      conf_matrix = TRUE
    ))
})

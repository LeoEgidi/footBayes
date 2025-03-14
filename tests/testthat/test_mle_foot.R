## all the test PASSED (also the skipped ones!)

#   ____________________________________________________________________________
#   Data tests                                                              ####

test_that("errors if data is not a matrix or data frame", {
  expect_error(
    mle_foot(123, model = "double_pois"),
    "Data are not stored in matrix/data frame"
  )
})

test_that("errors if required columns are missing", {
  ##  ............................................................................
  ##  Data                                                                    ####

  df_missing <- data.frame(
    periods   = 1:5,
    home_team = rep("TeamA", 5),
    away_team = rep("TeamB", 5),
    home_goals = rep(1, 5) # missing away_goals
  )

  ##  ............................................................................
  ##  Tests                                                                   ####

  expect_error(
    mle_foot(df_missing, model = "double_pois"),
    "data is missing required columns"
  )
})


test_that("errors if goals are not numeric", {
  ##  ............................................................................
  ##  Data                                                                    ####

  df_non_numeric <- data.frame(
    periods    = 1:5,
    home_team  = rep("TeamA", 5),
    away_team  = rep("TeamB", 5),
    home_goals = as.character(rep(1, 5)),
    away_goals = as.character(rep(0, 5))
  )

  ##  ............................................................................
  ##  Tests                                                                   ####

  expect_error(
    mle_foot(df_non_numeric, model = "double_pois"),
    "Goals are not numeric"
  )
})


test_that("warns if data has more than 5 columns", {
  skip_on_cran()

  ##  ............................................................................
  ##  Data                                                                    ####
  valid_data <- data.frame(
    periods    = 1:10,
    home_team  = rep(c("TeamA", "TeamB"), 5),
    away_team  = rep(c("TeamB", "TeamA"), 5),
    home_goals = sample(0:3, 10, replace = TRUE),
    away_goals = sample(0:3, 10, replace = TRUE)
  )
  df_extra <- valid_data
  df_extra$extra_col <- rnorm(nrow(valid_data))

  ##  ............................................................................
  ##  Tests                                                                   ####
  expect_warning(
    mle_foot(df_extra, model = "double_pois"),
    "Your dataset seems too large"
  )
})

#   ____________________________________________________________________________
#   Arguments tests                                                         ####

test_that("errors if predict is not a non-negative integer", {
  ##  ............................................................................
  ##  Data                                                                    ####
  valid_data <- data.frame(
    periods    = 1:10,
    home_team  = rep(c("TeamA", "TeamB"), 5),
    away_team  = rep(c("TeamB", "TeamA"), 5),
    home_goals = sample(0:3, 10, replace = TRUE),
    away_goals = sample(0:3, 10, replace = TRUE)
  )
  df_extra <- valid_data
  df_extra$extra_col <- rnorm(nrow(valid_data))

  ##  ............................................................................
  ##  Tests                                                                   ####
  expect_error(
    mle_foot(valid_data, model = "double_pois", predict = -1),
    "The argument 'predict' must be a non-negative integer"
  )
  expect_error(
    mle_foot(valid_data, model = "double_pois", predict = 1.5),
    "The argument 'predict' must be a non-negative integer"
  )
  expect_error(
    mle_foot(valid_data, model = "double_pois", predict = "2"),
    "The argument 'predict' must be a non-negative integer"
  )
})



test_that("errors if training set size is zero or negative", {
  ##  ............................................................................
  ##  Data                                                                    ####
  valid_data <- data.frame(
    periods    = 1:10,
    home_team  = rep(c("TeamA", "TeamB"), 5),
    away_team  = rep(c("TeamB", "TeamA"), 5),
    home_goals = sample(0:3, 10, replace = TRUE),
    away_goals = sample(0:3, 10, replace = TRUE)
  )
  df_extra <- valid_data
  df_extra$extra_col <- rnorm(nrow(valid_data))

  ##  ............................................................................
  ##  Tests                                                                   ####

  # If predict equals the number of rows, training set size becomes zero.
  expect_error(
    mle_foot(valid_data, model = "double_pois", predict = nrow(valid_data)),
    "The training set size is zero"
  )
  # If predict is larger than number of rows
  expect_error(
    mle_foot(valid_data, model = "double_pois", predict = nrow(valid_data) + 1),
    "The training set size is zero"
  )
})


test_that("model argument must be one of the allowed values", {
  ##  ............................................................................
  ##  Data                                                                    ####
  valid_data <- data.frame(
    periods    = 1:10,
    home_team  = rep(c("TeamA", "TeamB"), 5),
    away_team  = rep(c("TeamB", "TeamA"), 5),
    home_goals = sample(0:3, 10, replace = TRUE),
    away_goals = sample(0:3, 10, replace = TRUE)
  )
  df_extra <- valid_data
  df_extra$extra_col <- rnorm(nrow(valid_data))

  ##  ............................................................................
  ##  Tests                                                                   ####
  expect_error(
    mle_foot(valid_data, model = "invalid_model"))
})


test_that("output structure for skellam with out-of-sample prediction", {
  skip_on_cran()

  ##  ............................................................................
  ##  Data                                                                    ####
  valid_data <- data.frame(
    periods    = 1:10,
    home_team  = rep(c("TeamA", "TeamB"), 5),
    away_team  = rep(c("TeamB", "TeamA"), 5),
    home_goals = sample(0:3, 10, replace = TRUE),
    away_goals = sample(0:3, 10, replace = TRUE)
  )
  df_extra <- valid_data
  df_extra$extra_col <- rnorm(nrow(valid_data))

  ##  ............................................................................
  ##  Tests                                                                   ####
  result <- mle_foot(valid_data, model = "skellam", predict = 2)
  expect_true(all(c("att", "def", "home_effect", "model", "predict", "n.iter", "team1_prev", "team2_prev") %in% names(result)))
  expect_equal(result$predict, 2)
  expect_equal(length(result$team1_prev), 2)
  expect_equal(length(result$team2_prev), 2)
})

# require(dplyr)
#
# # data
# data("italy")
# italy <- as_tibble(italy)
# italy_2008<- italy %>%
#   dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
#   filter( Season=="2008")
#
# spain <- as_tibble(spain)
# spain_2008<- spain %>%
#   dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
#   filter( Season=="2008")
#
# germany <- as_tibble(germany)
# germany_2008<- germany %>%
#   dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
#   filter( Season=="2008")
#
#
# context("mle plain models")
# # These tests regard the performance of the mle_foot functions,
# # for methods double_pois, biv_pois, skellam and student_t.
#
# test_that("some mle models get errors/warnings", {
#
#
#   expect_warning(mle_foot(data = italy_2008,
#            model ="skellam"))
#   # warnings: "precision lost in result"
#   # ---> corrected if using "L-BFGS-B" as method
#
#   expect_warning(mle_foot(data = italy_2008,
#            model ="student_t"))
#   # warnings: log(sd)
#   # ---> corrected if using "Nelder-Mead" as method
#
#   # datasets with less than 4 columns
#   expect_error(mle_foot(data = italy_2008[,1:3],
#                         model = "biv_pois"))
#
#   # datasets different than matrix and dataframe
#   expect_error(mle_foot(data = rnorm(20),
#                         model = "biv_pois"))
#
#   # wrong model names
#   expect_error(mle_foot(data = italy_2008,
#                         model = "neg_binomial"))
#
#
# })
#
#
# test_that("prediction causes  warnings/errors", {
#
#   ## predict > N
#   expect_error(mle_foot(italy_2008,
#                          model ="student_t",
#                          predict = dim(italy_2008)[1]+1))
#
#   ## predict not a number
#   expect_error(mle_foot(italy_2008,
#                          model ="student_t",
#                          predict = "a"))
#
#   ## predict decimal number
#   # expect_warning(stan_foot(england_2004,
#   #           model ="student_t",
#   #           predict = 30.5))
# })
#
#
#
#
#
#
#
#
#

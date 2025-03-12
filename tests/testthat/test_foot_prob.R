## all the test PASSED (also the skipped ones!)


#   ____________________________________________________________________________
#   Data checks                                                             ####

test_that("foot_prob errors when data is missing required columns and when it returns a valid output", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####

  data("england")
  england <- as.data.frame(england)

  # One season only
  england_2004 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2004")

  colnames(england_2004) <- c(
    "periods", "home_team", "away_team",
    "home_goals", "away_goals"
  )

  england_2004_wrg <- england_2004[, 1:3]

  ##  ............................................................................
  ##  Tests                                                                   ####

  model_pois <- stan_foot(
    data = england_2004,
    model = "double_pois",
    predict = 30,
    iter_sampling = 200,
    chains = 2
  )

  # Missing a column in the data
  expect_error(
    foot_prob(object = model_pois, data = england_2004_wrg)
  )


  # Correct model
  result <- expect_error(
    foot_prob(object = model_pois, data = england_2004),
    NA
  )

  expect_type(result, "list")
  expect_true("prob_table" %in% names(result))
  expect_true("prob_plot" %in% names(result))
  expect_s3_class(result$prob_table, "data.frame")

  # Check that the expected columns are in the table.
  expected_cols <- c("home_team", "away_team", "prob_h", "prob_d", "prob_a", "mlo")
  expect_true(all(expected_cols %in% names(result$prob_table)))

  # Verify that the plot is a ggplot object.
  expect_s3_class(result$prob_plot, "ggplot")


  # T-student model
  model_stud <- stan_foot(
    data = england_2004,
    model = "student_t",
    predict = 30,
    iter_sampling = 200,
    chains = 2
  )

  # Correct model CmdStan
  expect_error(
    foot_prob(object = model_stud$fit, data = england_2004),
    NA
  )
})

test_that("foot_prob errors if object draws lack required elements", {
  ##  ............................................................................
  ##  Data                                                                    ####

  dummy_data <- data.frame(
    periods = rep(1, 6),
    home_team = c("TeamA", "TeamB", "TeamC", "TeamD", "TeamE", "TeamF"),
    away_team = c("TeamG", "TeamH", "TeamI", "TeamJ", "TeamK", "TeamL"),
    home_goals = c(1, 2, 0, 3, 2, 1),
    away_goals = c(0, 1, 2, 1, 1, 2),
    stringsAsFactors = FALSE
  )

  ##  ............................................................................
  ##  Tests                                                                   ####

  # Fake draws object without y_prev or diff_y_prev.
  fake_draws_empty <- list(a = 1)
  fake_fit_empty <- list(
    draws = function() {
      return(fake_draws_empty)
    }
  )
  class(fake_fit_empty) <- "CmdStanFit"
  fake_stanfoot_empty <- list(fit = fake_fit_empty)
  class(fake_stanfoot_empty) <- "stanFoot"

  expect_error(
    foot_prob(fake_fit_empty, dummy_data),
    "does not contain 'y_prev' or 'diff_y_prev'"
  )

  expect_error(
    foot_prob(fake_stanfoot_empty, dummy_data),
    "does not contain 'y_prev' or 'diff_y_prev'"
  )

  # No 'stanfit', 'CmdStanFit', 'stanFoot' or 'list'.
  class(fake_fit_empty) <- "ggplot2"

  expect_error(
    foot_prob(fake_fit_empty, dummy_data),
    "Provide one among these four model fit classes: 'stanfit', 'CmdStanFit', 'stanFoot' or 'list'."
  )
})


#   ____________________________________________________________________________
#   Arguments checks                                                        ####


test_that("foot_prob errors when home_team and away_team lengths mismatch", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####

  data("england")
  england <- as.data.frame(england)

  # One season only
  england_2004 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2004")

  colnames(england_2004) <- c(
    "periods", "home_team", "away_team",
    "home_goals", "away_goals"
  )

  ##  ............................................................................
  ##  Tests                                                                   ####

  model_pois <- stan_foot(
    data = england_2004,
    model = "double_pois",
    predict = 30,
    iter_sampling = 200,
    chains = 2
  )

  # Correct
  expect_error(
    foot_prob(
      object = model_pois, data = england_2004,
      home_team = "Arsenal",
      away_team = "Everton"
    ), NA
  )


  # There is not any out-of-sample match:Arsenal-Chelsea
  expect_error(
    foot_prob(
      object = model_pois, data = england_2004,
      home_team = "Arsenal",
      away_team = "Chelsea"
    )
  )

  # home_team and away_team lengths mismatch
  expect_error(
    foot_prob(
      object = model_pois, data = england_2004,
      home_team = "Arsenal",
      away_team = c("Chelsea", "Liverpool")
    ),
    "Please, include the same number for home and away teams."
  )

  # No predictions
  model_pois_no_pred <- stan_foot(
    data = england_2004,
    model = "double_pois",
    iter_sampling = 200,
    chains = 2
  )

  expect_error(
    foot_prob(
      object = model_pois_no_pred, data = england_2004),
  )
})


test_that("foot_prob errors when predict is zero for list objects", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####

  data("england")
  england <- as.data.frame(england)

  # One season only
  england_2004 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2004")

  colnames(england_2004) <- c(
    "periods", "home_team", "away_team",
    "home_goals", "away_goals"
  )

  ##  ............................................................................
  ##  Tests                                                                   ####

  model_pois <- stan_foot(
    data = england_2004,
    model = "double_pois",
    iter_sampling = 200,
    chains = 2
  )

  expect_error(
    foot_prob(model_pois, england_2004),
  )
})


#   ____________________________________________________________________________
#   MLE models                                                              ####

test_that("MLE models", {
  skip_on_cran()

  ##  ............................................................................
  ##  Data                                                                    ####

  data("england")
  england <- as.data.frame(england)

  # One season only
  england_2004 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2004")

  colnames(england_2004) <- c(
    "periods", "home_team", "away_team",
    "home_goals", "away_goals"
  )

  ##  ............................................................................
  ##  Tests                                                                   ####

  model <- mle_foot(england_2004, "double_pois", predict = 180, interval = "Wald")
  expect_error(foot_prob(object = model, data = england_2004), NA)

  model <- mle_foot(england_2004, "biv_pois", predict = 180)
  expect_error(foot_prob(object = model, data = england_2004), NA)

  model <- mle_foot(england_2004, "skellam", predict = 180)
  expect_error(foot_prob(object = model, data = england_2004), NA)
})
# require(dplyr)
#
# data("italy")
# italy_2000<- italy %>%
#   dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
#   filter(Season=="2000")
#
# test_that("foot_prob gets errors", {
#
#   # different numbers of home_team and away team
#   fit2 <- stan_foot(italy_2000, "biv_pois", predict = 36, iter = 200)
#   expect_error(foot_prob(fit2, italy_2000, home_team = c("Hellas Verona", "US Lecce"),
#             away_team = c("AC Perugia")))
#
#   # no predict
#   ## in stan_foot
#   fit3 <- stan_foot(italy_2000, "biv_pois", iter = 200)
#   expect_error(foot_prob(fit3, italy_2000))
#
#
#   ## mle_foot
#   fit4 <- mle_foot(italy_2000, "biv_pois")
#   expect_error(foot_prob(fit4, italy_2000))
#
#   # wrong class
#   y <- italy_2000$hgoal
#   fit5 <- lm(y~ rnorm(length(y)))
#   expect_error(foot_prob(fit5, italy_2000))
#
# })

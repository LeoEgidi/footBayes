## all the test PASSED (also the skipped ones!)


#   ____________________________________________________________________________
#   Data tests                                                             ####

test_that("foot_round_robin errors when data is missing required columns and when it returns a valid output", {
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

  model_pois_pred <- stan_foot(
    data = england_2004,
    model = "double_pois",
    predict = 30,
    iter_sampling = 200,
    chains = 2,
    seed = 433
  )

  # Missing a column in the data
  expect_error(
    foot_round_robin(object = model_pois_pred, data = england_2004_wrg)
  )


  # Correct model
  result <- expect_error(
    foot_round_robin(object = model_pois_pred, data = england_2004, output = "both"),
    NA
  )

  expect_type(result, "list")
  expect_true("round_table" %in% names(result))
  expect_true("round_plot" %in% names(result))
  expect_s3_class(result$round_table, "data.frame")

  # Check that the expected columns are in the table.
  expected_cols <- c("Home", "Away", "Home_prob", "Observed")
  expect_true(all(expected_cols %in% names(result$round_table)))

  # Verify that the plot is a ggplot object.
  expect_s3_class(result$round_plot, "ggplot")


  # T-student model
  model_stud <- stan_foot(
    data = england_2004,
    model = "student_t",
    predict = 30,
    iter_sampling = 200,
    chains = 2,
    seed = 433
  )

  # Correct model CmdStan
  expect_error(
    foot_round_robin(object = model_stud$fit, data = england_2004),
    NA
  )

})



test_that("foot_round_robin errors if object draws lack required elements", {

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
    foot_round_robin(fake_fit_empty, dummy_data),
    "Model does not contain at least one valid pair between 'y_prev' and 'y_rep' or 'diff_y_prev' and 'diff_y_rep' in its samples."
  )

  expect_error(
    foot_round_robin(fake_stanfoot_empty, dummy_data),
    "Model does not contain at least one valid pair between 'y_prev' and 'y_rep' or 'diff_y_prev' and 'diff_y_rep' in its samples."
  )

  # No 'stanfit', 'CmdStanFit', 'stanFoot' or 'list'.
  class(fake_fit_empty) <- "ggplot2"

  expect_error(
    foot_round_robin(fake_fit_empty, dummy_data),
    "Provide one among these three model fit classes: 'stanfit', 'CmdStanFit', 'stanFoot'."
  )
})


#   ____________________________________________________________________________
#   Arguments tests                                                        ####


test_that("foot_round_robin errors for teams argument", {
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
    chains = 2,
    seed = 433
  )

  # Correct
  expect_error(
    foot_round_robin(
      object = model_pois, data = england_2004,
      teams = c("Arsenal", "Everton")
    ), NA
  )


  # There is not AC Milan
  expect_error(
    foot_round_robin(
      object = model_pois, data = england_2004,
      teams = "AC Milan"
    )
  )
})



test_that("foot_round_robin works for different values of output", {
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
    chains = 2,
    seed = 433
  )

  # Correct
  expect_error(
    foot_round_robin(
      object = model_pois, data = england_2004,
      teams = c("Arsenal", "Everton"),
      output = "plot"
    ), NA
  )

  expect_error(
    foot_round_robin(
      object = model_pois, data = england_2004,
      teams = c("Arsenal", "Everton"),
      output = "table"
    ), NA
  )

  # Wrong
  expect_error(
    foot_round_robin(
      object = model_pois, data = england_2004,
      teams = c("Arsenal", "Everton"),
      output = "dataframe"
    ))
})



test_that("foot_round_robin works for the last round of the season", {
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

  # Predicting last round of the season
  model_pois <- stan_foot(
    data = england_2004,
    model = "double_pois",
    predict = 19,
    iter_sampling = 200,
    chains = 2,
    seed = 433
  )

  # Correct
  expect_error(
    foot_round_robin(
      object = model_pois, data = england_2004,
    ), NA
  )
})

test_that("foot_round_robin works with multiple seasons", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####

  data("england")
  england <- as.data.frame(england)

  # One season only
  england_04_05 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season %in% c("2004", "2005"))


  colnames(england_04_05) <- c(
    "periods", "home_team", "away_team",
    "home_goals", "away_goals"
  )

  ##  ............................................................................
  ##  Tests                                                                   ####

  # Predicting last round of the season
  model_pois <- stan_foot(
    data = england_04_05,
    model = "double_pois",
    predict = 38,
    iter_sampling = 200,
    chains = 2,
    seed = 433
  )

  # Correct
  expect_error(
    foot_round_robin(
      object = model_pois, data = england_04_05,
    ), NA
  )
})

# require(dplyr)
# data("italy")
# italy_2000<- italy %>%
#    dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
#    filter(Season=="2000")
#
# italy_1999_2000<- italy %>%
#    dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
#    filter(Season == "1999"|Season=="2000")
#
#  fit_dyn <- stan_foot(italy_2000, "double_pois", iter = 200)
#  fit_dyn_out <- stan_foot(italy_2000, "double_pois", predict = 45, iter = 200)
#
#
#  context("one season")
#  test_that("errors or warnings", {
#     #in-sample
#
#    expect_error(foot_round_robin(italy_2000, fit_dyn))
#    expect_error(foot_round_robin(italy_2000, fit_dyn, "AS Roma"))
#    expect_error(foot_round_robin(italy_2000, fit_dyn, c("AS Roma", "US Lecce")))
#    expect_error(foot_round_robin(italy_2000, fit_dyn, c("AS Roma", "US Lecce", "Parma AC")))
#     #out-of-sample
#
#    expect_error(foot_round_robin(italy_2000, fit_dyn_out), NA)
#    expect_error(foot_round_robin(italy_2000, fit_dyn_out, "AS Roma"), NA)
#    expect_error(foot_round_robin(italy_2000, fit_dyn_out, c("AS Roma", "US Lecce")), NA)
#    expect_error(foot_round_robin(italy_2000, fit_dyn_out, c("AS Roma", "US Lecce", "Parma AC")), NA)
#  })
#
#
#
#  context("more seasons")
#  fit_dyn <- stan_foot(italy_1999_2000, "double_pois", iter = 200)
#  fit_dyn_out <- stan_foot(italy_1999_2000, "double_pois", predict = 45, iter = 200)
#
#  test_that("errors or warnings", {
#     #in-sample
#    expect_error(foot_round_robin(italy_1999_2000, fit_dyn))
#    expect_error(foot_round_robin(italy_1999_2000, fit_dyn, "AS Roma"))
#    expect_error(foot_round_robin(italy_1999_2000, fit_dyn, c("AS Roma", "US Lecce")))
#    expect_error(foot_round_robin(italy_1999_2000, fit_dyn, c("AS Roma", "US Lecce", "Parma AC")))
#     #out-of-sample
#    expect_error(foot_round_robin(italy_1999_2000, fit_dyn_out), NA)
#    expect_error(foot_round_robin(italy_1999_2000, fit_dyn_out, "AS Roma"), NA)
#    expect_error(foot_round_robin(italy_1999_2000, fit_dyn_out, c("AS Roma", "US Lecce")), NA)
#    expect_error(foot_round_robin(italy_1999_2000, fit_dyn_out, c("AS Roma", "US Lecce", "Parma AC")), NA)
#  })
#

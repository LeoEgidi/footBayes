## all the test PASSED (also the skipped ones!)

#   ____________________________________________________________________________
#   Test for aggregated type                                                ####

test_that("pp_foot returns proper output for aggregated type", {
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

  model <- stan_foot(
    data = england_2004,
    model = "double_pois",
    iter_sampling = 200,
    chains = 2,
    seed = 433
  )

  result <- pp_foot(object = model, data = england_2004, type = "aggregated")

  # Expect output to be a list with components "pp_plot" and "pp_table"
  expect_type(result, "list")
  expect_true("pp_plot" %in% names(result))
  expect_true("pp_table" %in% names(result))

  # Check that the plot is a ggplot object and table is a data frame
  expect_s3_class(result$pp_plot, "ggplot")
  expect_true(is.data.frame(result$pp_table))

  # Check that the table has the expected column names
  expect_equal(names(result$pp_table), c("goal diff.", "Bayesian p-value"))
})



#   ____________________________________________________________________________
#   Test for matches type                                                   ####

test_that("pp_foot returns proper output for matches type", {
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

  model <- stan_foot(
    data = england_2004,
    model = "double_pois",
    iter_sampling = 200,
    chains = 2,
    seed = 433
  )

  result <- pp_foot(object = model, data = england_2004, type = "matches", coverage = 0.95)

  expect_type(result, "list")
  expect_true("pp_plot" %in% names(result))
  expect_true("pp_table" %in% names(result))
  expect_s3_class(result$pp_plot, "ggplot")
  expect_true(is.data.frame(result$pp_table))

  # The table for matches should have columns "1-alpha" and "emp. coverage"
  expect_equal(names(result$pp_table), c("1-alpha", "emp. coverage"))
})



#   ____________________________________________________________________________
#   Test for missing required columns in data                               ####


test_that("pp_foot errors if required columns are missing", {
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

  model <- stan_foot(
    data = england_2004,
    model = "double_pois",
    iter_sampling = 200,
    chains = 2,
    seed = 433
  )
  # Remove the 'periods' column
  data_missing <- england_2004[, -1]
  expect_error(pp_foot(object = model, data = data_missing),
               "data is missing required columns:")
})


#   ____________________________________________________________________________
#   Tests on input and different models                                     ####


test_that("expect error in input", {
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

  # Wrong model class
  model <- mle_foot(england_2004, "double_pois")
  expect_error(pp_foot(object = model, data = england_2004))

  # CmdStan model class
  model <- stan_foot(
    data = england_2004,
    model = "student_t",
    iter_sampling = 200,
    chains = 2,
    seed = 433
  )
  expect_error(pp_foot(object = model$fit, data = england_2004), NA)
})


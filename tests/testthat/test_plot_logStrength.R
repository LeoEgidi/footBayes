# all the test PASSED (also the skipped ones!)

#   ____________________________________________________________________________
#   Data tests                                                             ####

test_that("Error thrown if input object is not of class 'btdFoot'", {
  not_btdFoot <- list(a = 1)
  expect_error(
    plot_logStrength(not_btdFoot),
    "Object must be of class 'btdFoot'."
  )
})

test_that("Dynamic ranking plot returns a ggplot object with correct labels", {
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
    dplyr::filter(Season %in% c("2004", "2005")) %>%
    dplyr::mutate(match_outcome = dplyr::case_when(
      hgoal > vgoal ~ 1, # Home team wins
      hgoal == vgoal ~ 2, # Draw
      hgoal < vgoal ~ 3 # Away team wins
    )) %>% # Assign periods based on match number
    dplyr::select(
      periods = Season,
      home_team = home,
      away_team = visitor, match_outcome
    )
  ##  ............................................................................
  ##  Tests                                                                   ####

  # Correct model
  dyn_btd <- btd_foot(
    data = england_04_05, dynamic_rank = TRUE,
    method = "MCMC", iter_sampling = 200, rank_measure = "mean",
    chains = 2
  )

  p <- plot_logStrength(dyn_btd)
  expect_s3_class(p, "ggplot")
  # Check that the x- and y-axis labels match the dynamic branch
  expect_equal(p$labels$x, "Periods")
  expect_equal(p$labels$y, "Log-Strength Values")
  # Check that the data has multiple periods (thus dynamic)
  expect_true(max(dyn_btd$rank$periods, na.rm = TRUE) > 1)
})

test_that("Static ranking plot returns a ggplot object with correct labels", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  data("england")
  england <- as.data.frame(england)

  # One season only
  england_04 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season %in% c("2004")) %>%
    dplyr::mutate(match_outcome = dplyr::case_when(
      hgoal > vgoal ~ 1, # Home team wins
      hgoal == vgoal ~ 2, # Draw
      hgoal < vgoal ~ 3 # Away team wins
    )) %>% # Assign periods based on match number
    dplyr::select(
      periods = Season,
      home_team = home,
      away_team = visitor, match_outcome
    )
  ##  ............................................................................
  ##  Tests                                                                   ####

  # Correct model
  stat_btd <- btd_foot(
    data = england_04, dynamic_rank = FALSE,
    method = "MCMC", iter_sampling = 200, rank_measure = "mean",
    chains = 2
  )

  p <- plot_logStrength(stat_btd)
  expect_s3_class(p, "ggplot")
  # Check that the x- and y-axis labels match the static branch
  expect_equal(p$labels$x, "Log-Strength Values")
  expect_equal(p$labels$y, "Teams")
})

test_that("Filtering by teams works as expected and error is thrown if specified teams are not present in the data", {
  skip_if_not(stan_cmdstan_exists())

  data("england")
  england <- as.data.frame(england)

  # One season only
  england_04 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season %in% c("2004")) %>%
    dplyr::mutate(match_outcome = dplyr::case_when(
      hgoal > vgoal ~ 1, # Home team wins
      hgoal == vgoal ~ 2, # Draw
      hgoal < vgoal ~ 3 # Away team wins
    )) %>% # Assign periods based on match number
    dplyr::select(
      periods = Season,
      home_team = home,
      away_team = visitor, match_outcome
    )
  ##  ............................................................................
  ##  Tests                                                                   ####

  # Correct model
  stat_btd <- btd_foot(
    data = england_04, dynamic_rank = FALSE,
    method = "MCMC", iter_sampling = 200, rank_measure = "mean",
    chains = 2
  )

  p <- plot_logStrength(stat_btd, teams = c("Arsenal", "Chelsea"))
  # Check that the plot's data only contains the specified teams
  expect_setequal(unique(p$data$team), c("Arsenal", "Chelsea"))

  # Wrong plot
  expect_error(
    plot_logStrength(stat_btd, teams = c("Team X")),
    "The following teams are not present in the data"
  )
})

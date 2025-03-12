# all the test PASSED (also the skipped ones!)


#   ____________________________________________________________________________
#   Data tests                                                             ####

test_that("Error is thrown if input is not of class 'btdFoot'", {
  not_btdFoot <- list(a = 1)
  expect_error(
    plot_btdPosterior(not_btdFoot),
    "Object must be of class 'btdFoot'."
  )
})


#   ____________________________________________________________________________
#   Arguments tests                                                         ####

test_that("Error is thrown for invalid 'pars' argument", {
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

  btd <- btd_foot(
    data = england_04_05, dynamic_rank = FALSE,
    method = "VI"
  )

  expect_error(
    plot_btdPosterior(btd, pars = "invalid"),
    regexp = "should be one of"
  )
})




test_that("Error is thrown for invalid 'plot_type' argument", {
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

  btd <- btd_foot(
    data = england_04_05, dynamic_rank = FALSE,
    method = "VI"
  )

  expect_error(
    plot_btdPosterior(btd, plot_type = "invalid"),
    regexp = "should be one of"
  )
})

test_that("Error is thrown if specified teams are not present in the data", {
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

  btd <- btd_foot(
    data = england_04_05, dynamic_rank = FALSE,
    method = "VI"
  )

  expect_error(
    plot_btdPosterior(btd, teams = c("AC Milan")),
    regexp = "Some teams in 'teams' are not present in the data."
  )
})

test_that("Dynamic ranking: logStrength boxplot returns a ggplot object", {
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

  btd_dyn <- btd_foot(
    data = england_04_05, dynamic_rank = TRUE,
    method = "VI"
  )

  p <- plot_btdPosterior(btd_dyn, pars = "logStrength", plot_type = "boxplot")
  expect_s3_class(p, "ggplot")
  # Check facet settings indirectly by verifying that the data contains period as a factor
  plot_data <- ggplot_build(p)$data[[1]]
  expect_true("fill" %in% names(plot_data))
})

test_that("Dynamic ranking: logStrength density plot returns a ggplot object", {
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

  btd_dyn <- btd_foot(
    data = england_04_05, dynamic_rank = TRUE,
    method = "VI"
  )

  p <- plot_btdPosterior(btd_dyn, pars = "logStrength", plot_type = "density")
  expect_s3_class(p, "ggplot")
})

test_that("Static ranking: logStrength boxplot returns a ggplot object with correct labels", {
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

  btd <- btd_foot(
    data = england_04_05, dynamic_rank = FALSE,
    method = "VI"
  )

  p <- plot_btdPosterior(btd, pars = "logStrength", plot_type = "boxplot")
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "Teams")
  expect_equal(p$labels$y, "Log-Strength Values")
})

test_that("Static ranking: logStrength density plot returns a ggplot object with correct labels", {
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

  btd <- btd_foot(
    data = england_04_05, dynamic_rank = FALSE,
    method = "VI"
  )

  p <- plot_btdPosterior(btd, pars = "logStrength", plot_type = "density")
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "Log-Strength Values")
  # For static density, y-axis label should be "Teams"
  expect_equal(p$labels$y, "Teams")
})

test_that("Plot for 'logTie' parameter (boxplot) returns a ggplot object with correct labels", {
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

  btd <- btd_foot(
    data = england_04_05, dynamic_rank = FALSE,
    method = "VI"
  )

  p <- plot_btdPosterior(btd, pars = "logTie", plot_type = "boxplot")
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "")
  expect_equal(p$labels$y, "Log-Tie Values")
})

test_that("Plot for 'logTie' parameter (density) returns a ggplot object", {
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

  btd <- btd_foot(
    data = england_04_05, dynamic_rank = FALSE,
    method = "VI"
  )

  p <- plot_btdPosterior(btd, pars = "logTie", plot_type = "density")
  expect_s3_class(p, "ggplot")
  # Check that the fill label corresponds to probability
  expect_equal(p$labels$fill, "Probability")
})

test_that("Plot for 'home' parameter (boxplot) returns a ggplot object with correct labels", {
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

  btd <- btd_foot(
    data = england_04_05, dynamic_rank = FALSE, home_effect = TRUE,
    method = "VI"
  )

  p <- plot_btdPosterior(btd, pars = "home", plot_type = "boxplot")
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "")
  expect_equal(p$labels$y, "Home Effect Values")
})

test_that("Plot for 'home' parameter (density) returns a ggplot object", {
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

  btd <- btd_foot(
    data = england_04_05, dynamic_rank = FALSE, home_effect = TRUE,
    method = "VI"
  )

  p <- plot_btdPosterior(btd, pars = "home", plot_type = "density")
  expect_s3_class(p, "ggplot")
  # Check that the x-axis label corresponds to the parameter label
  expect_equal(p$labels$x, "Home Effect Values")
})

test_that("Error is thrown if parameter column is not found in draws for 'home'", {
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

  btd <- btd_foot(
    data = england_04_05, dynamic_rank = FALSE, home_effect = FALSE,
    method = "VI"
  )

  expect_error(
    plot_btdPosterior(btd, pars = "home"),
    regexp = "Parameter 'home' not found"
  )
})

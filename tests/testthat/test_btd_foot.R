# all the test PASSED (also the skipped ones!)

#   ____________________________________________________________________________
#   Data tests                                                             ####

test_that("btd_foot errors for wrong data types", {
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
    dplyr::filter(Season == "2004") %>%
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

  england_2004 <- as.matrix(england_2004)

  ##  ............................................................................
  ##  Tests                                                                   ####

  expect_error(btd_foot(
    data = england_2004, dynamic_rank = FALSE,
    method = "MCMC", iter_sampling = 200, chains = 2
  ))
})



test_that("btd_foot errors for wrong number of columns required", {
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
    dplyr::filter(Season == "2004") %>%
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

  england_2004_wrg <- england_2004[, 1:3]

  ##  ............................................................................
  ##  Tests                                                                   ####

  expect_error(btd_foot(
    data = england_2004_wrg, dynamic_rank = FALSE,
    method = "MCMC", iter_sampling = 200, chains = 2
  ))
})


test_that("btd_foot errors for NAs and wrong types", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####

  data("england")
  england <- as.data.frame(england)

  # One season only
  england_2004_NA <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2004") %>%
    dplyr::mutate(match_outcome = dplyr::case_when(
      hgoal > vgoal ~ 1, # Home team wins
      hgoal == vgoal ~ 2, # Draw
      hgoal < vgoal ~ NA # Away team wins
    )) %>% # Assign periods based on match number
    dplyr::select(
      periods = Season,
      home_team = home,
      away_team = visitor, match_outcome
    )

  england_2004_string <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2004") %>%
    dplyr::mutate(match_outcome = dplyr::case_when(
      hgoal > vgoal ~ "1", # Home team wins
      hgoal == vgoal ~ "bella", # Draw
      hgoal < vgoal ~ "ciao" # Away team wins
    )) %>% # Assign periods based on match number
    dplyr::select(
      periods = Season,
      home_team = home,
      away_team = visitor, match_outcome
    )


  ##  ............................................................................
  ##  Tests                                                                   ####

  expect_error(btd_foot(
    data = england_2004_NA, dynamic_rank = FALSE,
    method = "MCMC", iter_sampling = 200, chains = 2
  ))
})


test_that("btd_foot returns a valid output both for static models", {
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
    dplyr::filter(Season == "2004") %>%
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

  # Static models

  # MCMC
  result <- expect_error(btd_foot(
    data = england_2004, dynamic_rank = FALSE,
    method = "MCMC", iter_sampling = 200, chains = 2
  ), NA)

  expect_s3_class(result, "btdFoot")
  expect_true(is.list(result))
  expect_true(all(c(
    "fit", "rank", "data", "stan_data", "stan_code",
    "stan_args", "rank_measure", "alg_method"
  ) %in% names(result)))

  # The 'rank' data frame for static model should have a single period
  expect_true(all(result$rank$periods == 1))

  # VI
  result <- expect_error(btd_foot(
    data = england_2004, dynamic_rank = FALSE,
    method = "VI"
  ), NA)

  expect_s3_class(result, "btdFoot")
  expect_true(is.list(result))
  expect_true(all(c(
    "fit", "rank", "data", "stan_data", "stan_code",
    "stan_args", "rank_measure", "alg_method"
  ) %in% names(result)))

  # The 'rank' data frame for static model should have a single period
  expect_true(all(result$rank$periods == 1))

  # Pathfinder
  result <- expect_error(btd_foot(
    data = england_2004, dynamic_rank = FALSE,
    method = "pathfinder"
  ), NA)

  expect_s3_class(result, "btdFoot")
  expect_true(is.list(result))
  expect_true(all(c(
    "fit", "rank", "data", "stan_data", "stan_code",
    "stan_args", "rank_measure", "alg_method"
  ) %in% names(result)))

  # The 'rank' data frame for static model should have a single period
  expect_true(all(result$rank$periods == 1))

  # Laplace
  result <- expect_error(btd_foot(
    data = england_2004, dynamic_rank = FALSE,
    method = "laplace"
  ), NA)

  expect_s3_class(result, "btdFoot")
  expect_true(is.list(result))
  expect_true(all(c(
    "fit", "rank", "data", "stan_data", "stan_code",
    "stan_args", "rank_measure", "alg_method"
  ) %in% names(result)))

  # The 'rank' data frame for static model should have a single period
  expect_true(all(result$rank$periods == 1))
})



test_that("btd_foot returns a valid output both for dynamic models", {
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

  # Dynamic models

  # MCMC
  result <- btd_foot(
    data = england_04_05, dynamic_rank = TRUE,
    method = "MCMC", iter_sampling = 200, chains = 2
  )

  expect_s3_class(result, "btdFoot")
  expect_true(is.list(result))
  expect_true(all(c(
    "fit", "rank", "data", "stan_data", "stan_code",
    "stan_args", "rank_measure", "alg_method"
  ) %in% names(result)))

  # The 'rank' data frame for dynamic model should have two periods here
  expect_true(all(result$rank$periods %in% 2004:2005))

  # VI
  result <- btd_foot(
    data = england_04_05, dynamic_rank = TRUE,
    method = "VI"
  )

  expect_s3_class(result, "btdFoot")
  expect_true(is.list(result))
  expect_true(all(c(
    "fit", "rank", "data", "stan_data", "stan_code",
    "stan_args", "rank_measure", "alg_method"
  ) %in% names(result)))

  # The 'rank' data frame for dynamic model should have two periods here
  expect_true(all(result$rank$periods %in% 2004:2005))

  # Pathfinder
  result <- btd_foot(
    data = england_04_05, dynamic_rank = TRUE,
    method = "pathfinder"
  )

  expect_s3_class(result, "btdFoot")
  expect_true(is.list(result))
  expect_true(all(c(
    "fit", "rank", "data", "stan_data", "stan_code",
    "stan_args", "rank_measure", "alg_method"
  ) %in% names(result)))

  # The 'rank' data frame for dynamic model should have two periods here
  expect_true(all(result$rank$periods %in% 2004:2005))

  # Laplace
  result <- btd_foot(
    data = england_04_05, dynamic_rank = TRUE,
    method = "laplace"
  )

  expect_s3_class(result, "btdFoot")
  expect_true(is.list(result))
  expect_true(all(c(
    "fit", "rank", "data", "stan_data", "stan_code",
    "stan_args", "rank_measure", "alg_method"
  ) %in% names(result)))

  # The 'rank' data frame for static model should have a single period.
  expect_true(all(result$rank$periods %in% 2004:2005))
})


#   ____________________________________________________________________________
#   Home effect tests                                                       ####

test_that("btd_foot errors using the home effect", {
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
  expect_error(btd_foot(
    data = england_04_05, home_effect = TRUE,
    method = "MCMC", iter_sampling = 200, rank_measure = "map",
    chains = 2
  ), NA)

  # Wrong model
  expect_error(btd_foot(
    data = england_04_05, home_effect = "TRUE",
    method = "MCMC", iter_sampling = 200, chains = 2
  ))
  # Wrong model
  expect_error(btd_foot(
    data = england_04_05, home_effect = c(1, 2),
    method = "MCMC", iter_sampling = 200, chains = 2
  ))
})



#   ____________________________________________________________________________
#   Priors tests                                                            ####


test_that("btd_foot errors using the home effect", {
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
    dplyr::filter(Season == "2004") %>%
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

  # Wrong model
  expect_error(btd_foot(
    data = england_2004,
    prior_par = list(
      logStrength = cauchy(0, 3),
      logTie = normal(0, 0.3),
      home = normal(0, 5)
    ),
    method = "MCMC", iter_sampling = 200, chains = 2
  ))

  expect_error(btd_foot(
    data = england_2004,
    prior_par = list(
      home_advantage = normal(0, 5)
    ),
    method = "MCMC", iter_sampling = 200, chains = 2
  ))

  expect_error(btd_foot(
    data = england_2004,
    prior_par = cauchy(0, 3),
    method = "MCMC", iter_sampling = 200, chains = 2
  ))

  # Correct model
  expect_error(btd_foot(
    data = england_2004,
    prior_par = NULL,
    method = "MCMC", iter_sampling = 200, chains = 2
  ), NA)
})


# all the test PASSED (also the skipped ones!)


#   ____________________________________________________________________________
#   Data tests                                                             ####

test_that("Data checks", {
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


  # Wrong column type
  england_2004_wct <- england_2004
  england_2004_wct$home_goals <- as.factor(england_2004_wct$home_goals)

  # More seasons
  england_1999_2001 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2001" | Season == "2000" | Season == "1999")

  colnames(england_1999_2001) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")

  # Additional column
  england <- as.data.frame(england)
  england_2004_six <- england %>%
    dplyr::select(
      Season, home, visitor, hgoal, vgoal,
      FT
    ) %>%
    dplyr::filter(Season == "2004")

  colnames(england_2004_six) <- c("periods", "home_team", "away_team", "home_goals", "away_goals", "FT")


  # From a .csv (contained in data)
  bundes_2008 <- read.csv2(
    file = "BundesLiga07-08.csv",
    sep = ",", dec = "."
  )

  # with adjustment, but six columns, the last three as numeric
  bundes_2008_ristr <- bundes_2008[, c(
    "Date", "HomeTeam",
    "AwayTeam",
    "FTHG", "FTAG",
    "HTHG"
  )]

  ##  ............................................................................
  ##  Tests                                                                   ####

  # Wrong column type
  expect_error(stan_foot(
    data = england_2004_wct,
    model = "double_pois"
  ))

  # Three arguments
  expect_error(stan_foot(
    data = england_2004[, 1:3],
    model = "double_pois"
  ))

  # Wrong data
  expect_error(stan_foot(
    data = rnorm(20),
    model = "skellam"
  ))

  # Wrong model names
  expect_error(stan_foot(
    data = england_2004,
    model = "neg_binomial"
  ))

  # Two or more names
  expect_error(stan_foot(england_2004,
                         model = c("double_pois", "biv_pois")
  ))

  # Six arguments
  expect_warning(stan_foot(
    data = england_2004_six,
    model = "double_pois",
    iter_sampling = 200, chains = 2
  ))

  # With no adjustment
  expect_error(stan_foot(
    data = bundes_2008,
    model = "biv_pois"
  ))


  expect_error(stan_foot(
    data = bundes_2008_ristr,
    model = "double_pois"
  ))
})


#   ____________________________________________________________________________
#   Stan static models                                                      ####

test_that("Static models predictions errors", {
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

  colnames(england_2004) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")


  # More seasons
  england_1999_2001 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2001" | Season == "2000" | Season == "1999")

  colnames(england_1999_2001) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")


  ##  ............................................................................
  ##  Tests                                                                   ####

  # Correct model
  expect_error(stan_foot(england_2004,
                         model = "student_t",
                         predict = 10,
                         iter_sampling = 200,
                         chains = 2,
                         seed = 433
  ), NA)

  # Predicted games more than the number of played matches
  expect_error(stan_foot(england_2004,
                         model = "student_t",
                         predict = nrow(england_2004) + 1
  ))

  # Predict not a number
  expect_error(stan_foot(england_2004,
                         model = "student_t",
                         predict = "a"
  ))

  # Predict negative
  expect_error(stan_foot(england_2004,
                         model = "student_t",
                         predict = -25
  ))

  # Predict decimal number
  expect_error(stan_foot(england_2004,
                         model = "student_t",
                         predict = 30.6
  ))
})


#   ____________________________________________________________________________
#   Stan dynamic models                                                     ####

test_that("dynamics cause warnings/errors", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())


  ##  ............................................................................
  ##  Data                                                                    ####

  data("england")
  england <- as.data.frame(england)

  # One season
  england_2004 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2004")

  colnames(england_2004) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")

  # More seasons
  england_1999_2001 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2001" | Season == "2000" | Season == "1999")

  colnames(england_1999_2001) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")


  # Multiple league divisions
  england_2004_all <- england %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2004")

  colnames(england_2004_all) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")

  ##  ............................................................................
  ##  Tests                                                                   ####

  # Correct weekly dynamics
  expect_error(stan_foot(england_2004,
                         model = "double_pois",
                         dynamic_type = "weekly",
                         method = "VI",
                         seed = 433
  ), NA)

  # Fake dynamic for one season
  expect_warning(stan_foot(england_2004,
                           model = "double_pois",
                           dynamic_type = "seasonal",
                           method = "VI",
                           seed = 433
  ))

  # Wrong dynamic
  expect_error(stan_foot(england_2004,
                         model = "student_t",
                         dynamic_type = "annual",
                         predict = 25
  ))

  # Multiple seasons
  expect_error(stan_foot(england_1999_2001,
                         model = "double_pois",
                         dynamic_type = "weekly"
  ))

  # Number of matches different between teams
  expect_error(
    stan_foot(england_2004_all,
              model = "double_pois",
              dynamic_type = "weekly",
              iter_sampling = 200, chains = 2
    )
  )
  #
  # # seasonal dynamic with only one season
  # expect_warning(stan_foot(england_2004,
  #                          model = "double_pois",
  #                          dynamic_type = "seasonal",
  #                          iter_sampling = 200, chains = 2
  # ))
  #
  # # weekly dynamics with unequal matches
  # expect_error(stan_foot(england_2004,
  #                        model = "skellam",
  #                        dynamic_type = "weekly",
  #                        predict = 2
  # ))
})


#   ____________________________________________________________________________
#   Priors tests                                                           ####

test_that("Prior argument possible errors/warnings", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####
  data("england")

  england_1999_2001 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2001" | Season == "2000" | Season == "1999")

  colnames(england_1999_2001) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")


  ##  ............................................................................
  ##  Tests                                                                   ####

  # Null prior_par use the default one
  expect_error(stan_foot(england_1999_2001, "biv_pois",
                         prior_par = NULL,
                         method = "VI"
  ), NA)

  # Defualt prior for the abilities
  expect_error(stan_foot(england_1999_2001, "biv_pois",
                         prior_par = list(ability_sd = cauchy(0, 5)),
                         method = "VI"
  ), NA)

  # Wrong prior distribution
  expect_error(stan_foot(england_1999_2001, "biv_pois",
                         prior_par = list(ability_sd = binomial(10, 0.5)),
                         method = "VI"
  ))


  # Defualt prior for the ability_sd
  expect_error(stan_foot(england_1999_2001, "biv_pois",
                         prior_par = list(ability = normal(0, NULL)),
                         method = "VI",
                         seed = 433
  ), NA)
  # Wrong prior distribution
  expect_error(stan_foot(england_1999_2001, "biv_pois",
                         prior_par = list(ability = gaussian(10, 20)),
                         method = "VI"
  ))

  # Ability prior with fixed scale
  expect_warning(stan_foot(england_1999_2001, "biv_pois",
                           prior_par = list(ability = normal(0, 10)),
                           method = "VI",
                           seed = 433
  ))

  # It must be a list
  expect_error(stan_foot(england_1999_2001, "biv_pois",
                         prior_par = c(10, 5),
                         method = "VI"
  ))

  # Different priors
  expect_error(stan_foot(england_1999_2001, "biv_pois",
                         prior_par = list(
                           ability = student_t(4, 0, NULL),
                           ability_sd = laplace(0, 1)
                         ),
                         method = "VI",
                         seed = 433
  ), NA)

  expect_error(stan_foot(england_1999_2001, "biv_pois",
                         prior_par = list(
                           ability = cauchy(0, NULL),
                           ability_sd = normal(0, 1)
                         ),
                         method = "VI",
                         seed = 433
  ), NA)

  expect_error(stan_foot(england_1999_2001, "biv_pois",
                         prior_par = list(
                           ability = laplace(0, NULL),
                           ability_sd = student_t(2, 0, 5)
                         ),
                         method = "VI",
                         seed = 433
  ), NA)

  # Wrong input prior
  expect_error(stan_foot(england_1999_2001, "biv_pois",
                         prior = dirichlet(4, 0, 1), iter_sampling = 200
  ))

  # Wrong scale argument
  a <- "d"
  expect_error(stan_foot(england_1999_2001, "biv_pois",
                         prior = normal(0, a), iter_sampling = 200
  ))
})


#   ____________________________________________________________________________
#   Home effect                                                             ####

test_that("Home effect works", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())


  ##  ............................................................................
  ##  Data                                                                    ####

  data("england")
  england <- as.data.frame(england)

  # One season
  england_2004 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2004")

  colnames(england_2004) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")


  ##  ............................................................................
  ##  Tests                                                                   ####


  # home effect correct
  expect_error(stan_foot(england_2004,
                         model = "double_pois",
                         home_effect = TRUE,
                         iter_sampling = 200,
                         chains = 2,
                         seed = 433
  ), NA)

  expect_error(stan_foot(england_2004,
                         model = "double_pois",
                         home_effect = FALSE,
                         iter_sampling = 200,
                         chains = 2,
                         seed = 433
  ), NA)

  # Home effect wrong
  expect_error(stan_foot(england_2004,
                         model = "double_pois",
                         home_effect = "TRUE",
                         iter_sampling = 200,
                         chains = 2
  ))

  # Wrong home prior distribution
  expect_error(stan_foot(england_2004,
                         model = "double_pois",
                         home_effect = TRUE,
                         prior_par = list(home = cauchy(0, 5)),
                         iter_sampling = 200,
                         chains = 2
  ))
})

#   ____________________________________________________________________________
#   Method argument errors                                                  ####


test_that("multiple method names cause error", {
  skip_if_not(stan_cmdstan_exists())


  ##  ............................................................................
  ##  Data                                                                    ####

  # More seasons
  england_1999_2001 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2001" | Season == "2000" | Season == "1999")

  colnames(england_1999_2001) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")

  ##  ............................................................................
  ##  Tests                                                                   ####

  # Correct model with VI
  expect_error(stan_foot(england_1999_2001, "biv_pois",
                         method = "VI",
                         seed = 433
  ), NA)

  # Correct model with pathfinder
  expect_error(stan_foot(england_1999_2001, "biv_pois",
                         method = "pathfinder",
                         seed = 433
  ), NA)

  # Correct model with laplace
  expect_error(stan_foot(england_1999_2001, "double_pois",
                         method = "laplace",
                         seed = 433
  ), NA)


  # More methods
  expect_error(
    stan_foot(data = england_1999_2001, model = "double_pois", method = c("MCMC", "VI")),
    "must be of length 1"
  )

  # Wrong method name
  expect_error(
    stan_foot(data = england_1999_2001, model = "double_pois", method = "ABC")
  )
})



#   ____________________________________________________________________________
#   Optional ranking errors                                                 ####


test_that("integration between btd_foot and stan_foot", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())


  ##  ............................................................................
  ##  Data                                                                    ####


  data("england")
  england <- as.data.frame(england)

  # One season
  england_2004 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2004")

  colnames(england_2004) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")

  england_2004_rank <- england %>%
    dplyr::filter(Season == "2004") %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::mutate(match_outcome = dplyr::case_when(
      hgoal > vgoal ~ 1, # Home team wins
      hgoal == vgoal ~ 2, # Draw
      hgoal < vgoal ~ 3 # Away team wins
    )) %>%
    dplyr::mutate(periods = dplyr::case_when(
      dplyr::row_number() <= 190 ~ 1,
      dplyr::row_number() <= 380 ~ 2
    )) %>% # Assign periods based on match number
    dplyr::select(periods,
                  home_team = home,
                  away_team = visitor, match_outcome
    )


  ##  ............................................................................
  ##  Tests                                                                   ####

  fit_btd_PL <- btd_foot(
    data = england_2004_rank,
    dynamic_rank = FALSE,
    rank_measure = "median",
    iter_sampling = 200,
    chains = 2,
    seed = 433
  )


  expect_error(fit_with_ranking <- stan_foot(
    data = england_2004,
    model = "double_pois",
    ranking = fit_btd_PL,
    norm_method = "mad",
    iter_sampling = 200,
    chains = 2,
    seed = 433
  ), NA)
})

test_that("ranking with extra columns triggers warning and subsets to first three columns", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####

  # Ranking dataset with an extra column
  ranking_extra <- data.frame(
    periods = c(1, 1, 2, 2),
    team = c("TeamA", "TeamB", "TeamA", "TeamB"),
    rank_points = c(10, 20, 15, 25),
    extra_col = c("foo", "bar", "baz", "qux")
  )

  rank <- ranking_extra[, 1:3]

  # Match data set
  data_valid <- data.frame(
    periods    = c(1, 1, 2, 2),
    home_team  = c("TeamA", "TeamB", "TeamA", "TeamB"),
    away_team  = c("TeamB", "TeamA", "TeamB", "TeamA"),
    home_goals = rpois(4, 1),
    away_goals = rpois(4, 1)
  )

  ##  ............................................................................
  ##  Tests                                                                   ####

  # Warning about extra columns in ranking
  expect_warning(
    stan_foot(
      data = data_valid, model = "double_pois", dynamic_type = "seasonal",
      ranking = ranking_extra,
      iter_sampling = 200, chains = 2,
      seed = 433
    )
  )

  # Discrepancy ranking periods and league periods
  expect_error(
    stan_foot(
      data = data_valid, model = "double_pois",
      ranking = rank,
      iter_sampling = 200, chains = 2
    )
  )
})



test_that("ranking not as a data.frame/matrix or btdFoot causes error", {
  skip_if_not(stan_cmdstan_exists())


  ##  ............................................................................
  ##  Data                                                                    ####

  data_valid <- data.frame(
    periods    = rep(2004, 10),
    home_team  = rep("TeamA", 10),
    away_team  = rep("TeamB", 10),
    home_goals = rpois(10, 1),
    away_goals = rpois(10, 1)
  )

  ##  ............................................................................
  ##  Tests                                                                   ####

  expect_error(
    stan_foot(data = data_valid, model = "double_pois", ranking = "not_a_valid_ranking"),
    "Ranking must be a btdFoot class element, a matrix, or a data frame with 3 columns"
  )
})



test_that("ranking missing required columns causes error", {
  skip_if_not(stan_cmdstan_exists())


  ##  ............................................................................
  ##  Data                                                                    ####

  data_valid <- data.frame(
    periods    = rep(2004, 10),
    home_team  = rep("TeamA", 10),
    away_team  = rep("TeamB", 10),
    home_goals = rpois(10, 1),
    away_goals = rpois(10, 1)
  )

  bad_ranking <- data.frame(x = 1:5, y = letters[1:5])


  ##  ............................................................................
  ##  Tests                                                                   ####

  expect_error(
    stan_foot(data = data_valid, model = "double_pois", ranking = bad_ranking),
    "Ranking data frame must contain the following columns"
  )
})

test_that("ranking with NA in required columns causes error", {
  skip_if_not(stan_cmdstan_exists())


  ##  ............................................................................
  ##  Data                                                                    ####

  data_valid <- data.frame(
    periods    = rep(2004, 10),
    home_team  = rep("TeamA", 10),
    away_team  = rep("TeamB", 10),
    home_goals = rpois(10, 1),
    away_goals = rpois(10, 1)
  )
  bad_ranking <- data.frame(
    periods = c(1, NA, 3),
    team = c("A", "B", "C"),
    rank_points = c(10, 20, 30)
  )


  ##  ............................................................................
  ##  Tests                                                                   ####

  expect_error(
    stan_foot(data = data_valid, model = "double_pois", ranking = bad_ranking),
    "Ranking data contains NAs"
  )
})



test_that("ranking with non-numeric rank_points causes error", {
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####

  data_valid <- data.frame(
    periods    = rep(2004, 10),
    home_team  = rep("TeamA", 10),
    away_team  = rep("TeamB", 10),
    home_goals = rpois(10, 1),
    away_goals = rpois(10, 1)
  )
  bad_ranking <- data.frame(
    periods = c(1, 2, 3),
    team = c("A", "B", "C"),
    rank_points = c("a", "b", "c")
  )

  ##  ............................................................................
  ##  Tests                                                                   ####

  expect_error(
    stan_foot(data = data_valid, model = "double_pois", ranking = bad_ranking),
    "Ranking points type must be numeric"
  )
})

test_that("ranking_map with wrong length causes error", {
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####

  # Create a valid ranking with multiple periods
  valid_ranking <- data.frame(
    periods = c(1, 1, 2, 2),
    team = c("TeamA", "TeamB", "TeamA", "TeamB"),
    rank_points = c(10, 20, 15, 25)
  )
  data_valid <- data.frame(
    periods    = rep(2004, 4),
    home_team  = c("TeamA", "TeamB", "TeamA", "TeamB"),
    away_team  = c("TeamB", "TeamA", "TeamB", "TeamA"),
    home_goals = rpois(4, 1),
    away_goals = rpois(4, 1)
  )

  ##  ............................................................................
  ##  Tests                                                                   ####

  # Provide a ranking_map of incorrect length (should be of length 4)
  expect_error(
    stan_foot(
      data = data_valid, model = "double_pois",
      ranking = valid_ranking, ranking_map = c(1, 2, 3)
    ),
    "Length of 'ranking_map' must equal the number of matches"
  )
})

# Optional arguments

test_that("invalid prior in ability causes error", {
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####

  data_valid <- data.frame(
    periods    = rep(2004, 10),
    home_team  = rep("TeamA", 10),
    away_team  = rep("TeamB", 10),
    home_goals = rpois(10, 1),
    away_goals = rpois(10, 1)
  )

  ##  ............................................................................
  ##  Tests                                                                   ####

  # Assuming that 'dirichlet' is not an allowed prior for ability.
  expect_error(
    stan_foot(
      data = data_valid, model = "biv_pois",
      prior_par = list(ability = dirichlet(4, 0, 1))
    ),
  )
  # Assuming a name that  is not an allowed in prior_par.
  expect_error(
    stan_foot(
      data = data_valid, model = "biv_pois",
      prior_par = list(ability_att = normal(0, 10)),
    )
  )
})



test_that("non-numeric scale in prior causes error", {
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####

  data_valid <- data.frame(
    periods    = rep(2004, 10),
    home_team  = rep("TeamA", 10),
    away_team  = rep("TeamB", 10),
    home_goals = rpois(10, 1),
    away_goals = rpois(10, 1)
  )

  ##  ............................................................................
  ##  Tests                                                                   ####

  expect_error(
    stan_foot(
      data = data_valid, model = "biv_pois",
      prior_par = list(ability = normal(0, "d"))
    ),
    "scale should be NULL or numeric"
  )
})



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
                         model = "neg_bin",
                         predict = 10,
                         iter_sampling = 200,
                         chains = 2,
                         seed = 433
  ), NA)


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


  expect_error(fit_with_ranking2 <- stan_foot(
    data = england_2004,
    model = "double_pois",
    ranking = fit_btd_PL,
    norm_method = "min_max",
    iter_sampling = 200,
    chains = 2,
    seed = 433
  ), NA)

  expect_error(fit_with_ranking3 <- stan_foot(
    data = england_2004,
    model = "double_pois",
    ranking = fit_btd_PL,
    norm_method = "standard",
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

test_that("ranking points are matched to the teams by name", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####

  # Home teams appear in the order A, C, B, D
  data_valid <- data.frame(
    periods    = rep(1, 6),
    home_team  = c("A", "C", "B", "D", "A", "B"),
    away_team  = c("B", "D", "C", "A", "C", "D"),
    home_goals = c(1, 0, 2, 1, 3, 0),
    away_goals = c(0, 0, 1, 2, 1, 1)
  )
  # Ranking in alphabetical order, plus a team that is not in the data
  ranking_valid <- data.frame(
    periods = 1,
    team = c("A", "B", "C", "D", "E"),
    rank_points = c(10, 20, 30, 40, 50)
  )

  ##  ............................................................................
  ##  Tests                                                                   ####

  fit <- stan_foot(
    data = data_valid, model = "double_pois", ranking = ranking_valid,
    iter_sampling = 100, chains = 1, seed = 433
  )
  expect_equal(as.vector(fit$stan_data$ranking[1, ]), c(10, 30, 20, 40))
})

test_that("ranking with missing teams, duplicated rows or missing periods causes error", {
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####

  data_valid <- data.frame(
    periods    = c(1, 1, 2, 2),
    home_team  = c("A", "B", "A", "B"),
    away_team  = c("B", "A", "B", "A"),
    home_goals = c(1, 0, 2, 1),
    away_goals = c(0, 0, 1, 2)
  )

  ##  ............................................................................
  ##  Tests                                                                   ####

  # Team B has no ranking points
  expect_error(
    stan_foot(
      data = data_valid, model = "double_pois", dynamic_type = "seasonal",
      ranking = data.frame(periods = c(1, 2), team = "A", rank_points = c(10, 15))
    ),
    "have no ranking points in 'ranking': B"
  )

  # Team A has two ranking points in period 1
  expect_error(
    stan_foot(
      data = data_valid, model = "double_pois", dynamic_type = "seasonal",
      ranking = data.frame(periods = c(1, 1, 1, 2, 2), team = c("A", "A", "B", "A", "B"), rank_points = 1:5)
    ),
    "more than one row for the same team and period"
  )

  # Team B has no ranking points in period 2
  expect_error(
    stan_foot(
      data = data_valid, model = "double_pois", dynamic_type = "seasonal",
      ranking = data.frame(periods = c(1, 1, 2), team = c("A", "B", "A"), rank_points = c(10, 20, 15))
    ),
    "no ranking points in some periods"
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




#   ____________________________________________________________________________
#   Dynamic specifications: dynamic_par and dynamic_weight                  ####

test_that("dynamic_par and dynamic_weight arguments are validated", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####

  data("england")
  england <- as.data.frame(england)

  # Two seasons, each split into two halves (periods 1-4)
  england_2000_2001 <- england %>%
    dplyr::filter(division == 1, Season %in% c("2000", "2001")) %>%
    dplyr::arrange(Season, Date) %>%
    dplyr::group_by(Season) %>%
    dplyr::mutate(half = ifelse(dplyr::row_number() <= dplyr::n() / 2, 1, 2)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(periods = 2 * (as.numeric(Season) - 2000) + half) %>%
    dplyr::select(periods,
      home_team = home, away_team = visitor,
      home_goals = hgoal, away_goals = vgoal
    )

  england_2004 <- england %>%
    dplyr::filter(division == 1, Season == "2004") %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal)
  colnames(england_2004) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")

  ##  ............................................................................
  ##  Tests                                                                   ####

  # The experimental COM-Poisson model is not exposed
  expect_error(
    stan_foot(england_2000_2001, model = "com_pois", dynamic_type = "seasonal"),
    "should be one of"
  )

  # dynamic_par must be a list with known names
  expect_error(
    stan_foot(england_2000_2001,
      model = "double_pois", dynamic_type = "seasonal",
      dynamic_par = "common_sd"
    ),
    "'dynamic_par' must be a list"
  )
  expect_error(
    stan_foot(england_2000_2001,
      model = "double_pois", dynamic_type = "seasonal",
      dynamic_par = list(spike_prob = 0.2)
    ),
    "Unknown elements in 'dynamic_par'"
  )

  # common_sd / kl_variance must be single logicals
  expect_error(
    stan_foot(england_2000_2001,
      model = "double_pois", dynamic_type = "seasonal",
      dynamic_par = list(common_sd = "yes")
    ),
    "'common_sd' must be a single logical value"
  )
  expect_error(
    stan_foot(england_2000_2001,
      model = "double_pois", dynamic_type = "seasonal",
      dynamic_par = list(kl_variance = c(TRUE, FALSE))
    ),
    "'kl_variance' must be a single logical value"
  )

  # periods_per_season must be a single integer >= 2
  expect_error(
    stan_foot(england_2000_2001,
      model = "double_pois", dynamic_type = "seasonal",
      dynamic_par = list(kl_variance = TRUE, periods_per_season = 1)
    ),
    "'periods_per_season' must be a single integer"
  )
  expect_error(
    stan_foot(england_2000_2001,
      model = "double_pois", dynamic_type = "seasonal",
      dynamic_par = list(kl_variance = TRUE, periods_per_season = 2.5)
    ),
    "'periods_per_season' must be a single integer"
  )

  # spike and slab must be normal priors with a positive scale
  expect_error(
    stan_foot(england_2000_2001,
      model = "double_pois", dynamic_type = "seasonal", dynamic_weight = TRUE,
      dynamic_par = list(spike = cauchy(0, 1))
    ),
    "spike and slab must be normal priors"
  )
  expect_error(
    stan_foot(england_2000_2001,
      model = "double_pois", dynamic_type = "seasonal", dynamic_weight = TRUE,
      dynamic_par = list(slab = normal(0, NULL))
    ),
    "positive scale"
  )
  expect_error(
    stan_foot(england_2000_2001,
      model = "double_pois", dynamic_type = "seasonal", dynamic_weight = TRUE,
      dynamic_par = list(spike = normal(-1, 1))
    ),
    "greater than or equal to 0"
  )

  # dynamic_weight requires a dynamic model and is not valid for student_t
  expect_error(
    stan_foot(england_2000_2001, model = "double_pois", dynamic_weight = TRUE),
    "'dynamic_weight' requires specifying a dynamic model"
  )
  expect_error(
    stan_foot(england_2000_2001, model = "double_pois", dynamic_weight = "TRUE"),
    "'dynamic_weight' must be a single logical value"
  )
  expect_error(
    stan_foot(england_2000_2001,
      model = "student_t", dynamic_type = "seasonal", dynamic_weight = TRUE
    ),
    "`dynamic_weight = TRUE` is not valid for the `student_t` model"
  )

  # kl_variance requires seasonal dynamics with at least one summer break
  expect_error(
    stan_foot(england_2000_2001,
      model = "double_pois", dynamic_par = list(kl_variance = TRUE)
    ),
    "'kl_variance' requires specifying a dynamic model"
  )
  expect_error(
    stan_foot(england_2004,
      model = "double_pois", dynamic_type = "weekly",
      dynamic_par = list(kl_variance = TRUE)
    ),
    "requires `dynamic_type = \"seasonal\"`"
  )
  expect_error(
    stan_foot(england_2000_2001,
      model = "double_pois", dynamic_type = "seasonal",
      dynamic_par = list(kl_variance = TRUE, periods_per_season = 4)
    ),
    "requires more than 'periods_per_season'"
  )

  # Mutually exclusive options
  expect_error(
    stan_foot(england_2000_2001,
      model = "double_pois", dynamic_type = "seasonal", dynamic_weight = TRUE,
      dynamic_par = list(common_sd = TRUE)
    ),
    "not compatible with `common_sd = TRUE`"
  )
  expect_error(
    stan_foot(england_2000_2001,
      model = "double_pois", dynamic_type = "seasonal", dynamic_weight = TRUE,
      dynamic_par = list(kl_variance = TRUE)
    ),
    "not compatible with `kl_variance = TRUE`"
  )
  expect_error(
    stan_foot(england_2000_2001,
      model = "double_pois", dynamic_type = "seasonal",
      dynamic_par = list(kl_variance = TRUE, common_sd = TRUE)
    ),
    "not compatible with `common_sd = TRUE`"
  )
  expect_error(
    stan_foot(england_2000_2001,
      model = "student_t", dynamic_type = "seasonal",
      dynamic_par = list(common_sd = TRUE)
    ),
    "not valid for the `student_t` model"
  )
  expect_error(
    stan_foot(england_2000_2001,
      model = "student_t", dynamic_type = "seasonal",
      dynamic_par = list(kl_variance = TRUE)
    ),
    "not valid for the `student_t` model"
  )
})


test_that("dynamic specifications fit and expose the expected parameters", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####

  data("england")
  england <- as.data.frame(england)

  # Two seasons, each split into two halves (periods 1-4)
  england_2000_2001 <- england %>%
    dplyr::filter(division == 1, Season %in% c("2000", "2001")) %>%
    dplyr::arrange(Season, Date) %>%
    dplyr::group_by(Season) %>%
    dplyr::mutate(half = ifelse(dplyr::row_number() <= dplyr::n() / 2, 1, 2)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(periods = 2 * (as.numeric(Season) - 2000) + half) %>%
    dplyr::select(periods,
      home_team = home, away_team = visitor,
      home_goals = hgoal, away_goals = vgoal
    )

  ##  ............................................................................
  ##  Tests                                                                   ####

  # Owen (2011): common evolution sd
  fit_owen <- stan_foot(england_2000_2001,
    model = "double_pois", dynamic_type = "seasonal",
    dynamic_par = list(common_sd = TRUE),
    method = "pathfinder", seed = 433
  )
  vars_owen <- fit_owen$fit$metadata()$stan_variables
  expect_true("sigma_common" %in% vars_owen)
  expect_false("sigma_att" %in% vars_owen)
  expect_equal(fit_owen$stan_data$ind_common_sigma, 1)

  # Koopman & Lit (2015): summer break before period 3 (periods_per_season = 2)
  fit_kl <- stan_foot(england_2000_2001,
    model = "double_pois", dynamic_type = "seasonal",
    dynamic_par = list(kl_variance = TRUE),
    method = "pathfinder", seed = 433
  )
  vars_kl <- fit_kl$fit$metadata()$stan_variables
  expect_true(all(c("sigma_att_kl", "sigma_def_kl", "sigma_break") %in% vars_kl))
  expect_equal(fit_kl$stan_data$is_summer_break, c(0L, 0L, 1L, 0L))
  expect_equal(fit_kl$stan_data$ind_kl_sd, 1)

  # Summer breaks are placed after every 'periods_per_season' periods,
  # also when the last training season is incomplete (predict)
  fit_kl_pred <- stan_foot(england_2000_2001,
    model = "double_pois", dynamic_type = "seasonal", predict = 10,
    dynamic_par = list(kl_variance = TRUE, periods_per_season = 2),
    method = "pathfinder", seed = 433
  )
  expect_equal(fit_kl_pred$stan_data$is_summer_break, c(0L, 0L, 1L, 0L))

  # Weighted dynamic model (commensurate priors): the optimization-based
  # algorithms may fail on the spike-and-slab mixture, hence a short HMC run
  fit_wdm <- stan_foot(england_2000_2001,
    model = "double_pois", dynamic_type = "seasonal",
    dynamic_weight = TRUE,
    method = "MCMC", chains = 1, iter_sampling = 100, iter_warmup = 100,
    refresh = 0, seed = 433
  )
  vars_wdm <- fit_wdm$fit$metadata()$stan_variables
  expect_true(all(c("prob_spike", "comm_prec_att", "comm_sd_def") %in% vars_wdm))
  expect_equal(fit_wdm$stan_data$ind_comm_prior, 1)
  # default spike and slab hyperparameters
  expect_equal(fit_wdm$stan_data$mu_spike, 9)
  expect_equal(fit_wdm$stan_data$sd_spike, 1.5)
  expect_equal(fit_wdm$stan_data$mu_slab, 0)
  expect_equal(fit_wdm$stan_data$sd_slab, 3)

  # Dixon-Coles in Stan (static and dynamic)
  fit_dc <- stan_foot(england_2000_2001,
    model = "dixon_coles",
    method = "pathfinder", seed = 433
  )
  expect_true("rho" %in% fit_dc$fit$metadata()$stan_variables)
  fit_dc_dyn <- stan_foot(england_2000_2001,
    model = "dixon_coles", dynamic_type = "seasonal",
    method = "pathfinder", seed = 433
  )
  expect_true("rho" %in% fit_dc_dyn$fit$metadata()$stan_variables)
})

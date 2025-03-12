## all the test PASSED (also the skipped ones!)


#   ____________________________________________________________________________
#   Input validation for foot_abilities                                     ####

test_that("error when data missing required columns", {
  data("england")
  england <- as_tibble(england)

  # Prepare a season's data and rename columns as expected
  england_2004 <- england %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2004") %>%
    dplyr::rename(
      periods = Season,
      home_team = home,
      away_team = visitor,
      home_goals = hgoal,
      away_goals = vgoal
    )

  # Remove one required column
  data_missing <- dplyr::select(england_2004, -home_goals)

  # Create a fake MLE object (static branch)
  fake_mle <- list(
    att = matrix(c(0.2, 0.1, 0.3, -0.1, -0.2, 0.0), nrow = 2, ncol = 3, byrow = TRUE),
    def = matrix(c(-0.1, 0.0, 0.1, 0.3, 0.2, 0.4), nrow = 2, ncol = 3, byrow = TRUE)
  )

  expect_error(
    foot_abilities(fake_mle, data_missing),
    "data is missing required columns"
  )
})

test_that("error for invalid team names", {
  data("england")
  england <- as_tibble(england)

  england_2004 <- england %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2004") %>%
    dplyr::rename(
      periods = Season,
      home_team = home,
      away_team = visitor,
      home_goals = hgoal,
      away_goals = vgoal
    )

  fake_mle <- list(
    att = matrix(c(0.2, 0.1, 0.3, -0.1, -0.2, 0.0), nrow = 2, ncol = 3, byrow = TRUE),
    def = matrix(c(-0.1, 0.0, 0.1, 0.3, 0.2, 0.4), nrow = 2, ncol = 3, byrow = TRUE)
  )

  # Passing a team name not present in the data should trigger an error.
  expect_error(
    foot_abilities(fake_mle, england_2004, teams = "Nonexistent"),
    "Select only valid teams' names!"
  )
})

test_that("error for invalid type argument", {
  data("england")
  england <- as_tibble(england)

  england_2004 <- england %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2004") %>%
    dplyr::rename(
      periods = Season,
      home_team = home,
      away_team = visitor,
      home_goals = hgoal,
      away_goals = vgoal
    )

  fake_mle <- list(
    att = matrix(c(0.2, 0.1, 0.3, -0.1, -0.2, 0.0), nrow = 2, ncol = 3, byrow = TRUE),
    def = matrix(c(-0.1, 0.0, 0.1, 0.3, 0.2, 0.4), nrow = 2, ncol = 3, byrow = TRUE)
  )

  # Providing an invalid type should produce an error.
  expect_error(
    foot_abilities(fake_mle, england_2004, type = "invalid"),
    "arg"
  )
})

# Static models - Bayesian and MLE

test_that("MLE model returns ggplot for both, attack, and defense", {
  data("england")
  england <- as_tibble(england)

  england_2004 <- england %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2004") %>%
    dplyr::rename(
      periods = Season,
      home_team = home,
      away_team = visitor,
      home_goals = hgoal,
      away_goals = vgoal
    )

  teams_used <- unique(c(england_2004$home_team, england_2004$away_team))
  # For testing, if there are more than 2 teams, use only the first two.
  teams_test <- if (length(teams_used) >= 2) teams_used[1:2] else teams_used

  fake_mle <- list(
    att = matrix(c(0.2, 0.1, 0.3, -0.1, -0.2, 0.0), nrow = length(teams_test), ncol = 3, byrow = TRUE),
    def = matrix(c(-0.1, 0.0, 0.1, 0.3, 0.2, 0.4), nrow = length(teams_test), ncol = 3, byrow = TRUE)
  )

  p_both <- foot_abilities(fake_mle, england_2004, teams = teams_test)
  expect_s3_class(p_both, "ggplot")

  p_attack <- foot_abilities(fake_mle, england_2004, teams = teams_test, type = "attack")
  expect_s3_class(p_attack, "ggplot")

  p_defense <- foot_abilities(fake_mle, england_2004, teams = teams_test, type = "defense")
  expect_s3_class(p_defense, "ggplot")
})



test_that("Bayesian model returns ggplot for both, attack, and defense", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  data("england")
  england <- as_tibble(england)

  england_2004 <- england %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2004") %>%
    dplyr::rename(
      periods = Season,
      home_team = home,
      away_team = visitor,
      home_goals = hgoal,
      away_goals = vgoal
    )

  # Poisson models
  fit_static <- stan_foot(england_2004, "double_pois",
                          iter_sampling = 200, chains = 2)

  teams_used <- unique(c(england_2004$home_team, england_2004$away_team))
  # For testing, if there are more than 2 teams, use only the first two.
  teams_test <- if (length(teams_used) >= 2) teams_used[1:2] else teams_used

  p_both <- foot_abilities(fit_static, england_2004, teams = teams_test)
  expect_s3_class(p_both, "ggplot")

  p_attack <- foot_abilities(fit_static, england_2004, teams = teams_test, type = "attack")
  expect_s3_class(p_attack, "ggplot")

  p_defense <- foot_abilities(fit_static, england_2004, teams = teams_test, type = "defense")
  expect_s3_class(p_defense, "ggplot")

  # t-student model
  fit_static_t_stud <- stan_foot(england_2004, "student_t",
                                 iter_sampling = 200, chains = 2)

  # Returning object
  p_static_t_stud <- foot_abilities(fit_static_t_stud, england_2004, teams = c("Arsenal", "Everton"))
  expect_s3_class(p_static_t_stud, "ggplot")
})


#   ____________________________________________________________________________
#   Static models - Wrong object type                                       ####

test_that("object argument gives errors for wrong class", {
  data("england")
  england <- as_tibble(england)

  england_2004 <- england %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2004") %>%
    dplyr::rename(
      periods = Season,
      home_team = home,
      away_team = visitor,
      home_goals = hgoal,
      away_goals = vgoal
    )

  y <- england_2004$home_goals
  fit_lm <- lm(y ~ rnorm(length(y)))
  expect_error(foot_abilities(fit_lm, england_2004))
})

# Dynamic models

test_that("Dynamic models return ggplot without errors", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  data("england")
  england <- as_tibble(england)

  england_1999_2000 <- england %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season %in% c("1999", "2000")) %>%
    dplyr::rename(
      periods = Season,
      home_team = home,
      away_team = visitor,
      home_goals = hgoal,
      away_goals = vgoal
    )

  # Poisson models
  fit_dynamic <- stan_foot(england_1999_2000, "biv_pois", dynamic_type = "seasonal",
                           iter_sampling = 200, chains = 2)

  # Test that no errors occur with various teams selections
  expect_error(foot_abilities(fit_dynamic, england_1999_2000), NA)
  expect_error(foot_abilities(fit_dynamic, england_1999_2000, teams = "Arsenal"), NA)
  expect_error(foot_abilities(fit_dynamic, england_1999_2000, teams = c("Arsenal", "Everton")), NA)
  # Test wrong name in the teams
  expect_error(foot_abilities(fit_dynamic, england_1999_2000, teams = "Arzignano"))

  # Returning object
  p_dynamic_both <- foot_abilities(fit_dynamic, england_1999_2000, teams = c("Arsenal", "Everton"))
  expect_s3_class(p_dynamic_both, "ggplot")

  p_dynamic_attack <- foot_abilities(fit_dynamic, england_1999_2000, teams = c("Arsenal", "Everton"), type = "attack")
  expect_s3_class(p_dynamic_attack, "ggplot")

  p_dynamic_defense <- foot_abilities(fit_dynamic, england_1999_2000, teams = c("Arsenal", "Everton"), type = "defense")
  expect_s3_class(p_dynamic_defense, "ggplot")

  # t-student model
  fit_dynamic_t_stud <- stan_foot(england_1999_2000, "student_t", dynamic_type = "seasonal",
                                  iter_sampling = 200, chains = 2)

  # Returning object
  p_dynamic_t_stud <- foot_abilities(fit_dynamic_t_stud, england_1999_2000, teams = c("Arsenal", "Everton"))
  expect_s3_class(p_dynamic_t_stud, "ggplot")

  # CmdStan model
  p_dynamic_t_stud_cmd <- foot_abilities(fit_dynamic_t_stud$fit, england_1999_2000, teams = c("Arsenal", "Everton"))
  expect_s3_class(p_dynamic_t_stud_cmd, "ggplot")
})



#   ____________________________________________________________________________
#   t-student list object                                                   ####

test_that("student_t case for list object returns ggplot when abilities matrix has at least two teams", {
  data("england")
  england <- as_tibble(england)

  england_2004 <- england %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2004") %>%
    dplyr::rename(
      periods = Season,
      home_team = home,
      away_team = visitor,
      home_goals = hgoal,
      away_goals = vgoal
    )

  teams_all <- unique(c(england_2004$home_team, england_2004$away_team))
  # Use two teams for the test
  teams_test <- teams_all[1:2]

  # Create a fake student_t list object with an 'abilities' matrix
  # Each row corresponds to a team: [lower, mean, upper]
  fake_student_t <- list(
    abilities = matrix(c(
      -0.5, 0.2, 0.6,
      -0.3, 0.1, 0.4
    ), nrow = length(teams_test), ncol = 3, byrow = TRUE)
  )

  p <- foot_abilities(fake_student_t, england_2004, teams = teams_test)
  expect_s3_class(p, "ggplot")
})

test_that("student_t case for list object errors when abilities is a vector (only one team selected)", {
  data("england")
  england <- as_tibble(england)

  england_2004 <- england %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "2004") %>%
    dplyr::rename(
      periods = Season,
      home_team = home,
      away_team = visitor,
      home_goals = hgoal,
      away_goals = vgoal
    )

  teams_all <- unique(c(england_2004$home_team, england_2004$away_team))
  # Use only one team so that abilities becomes a vector
  teams_test <- teams_all[1]

  fake_student_t <- list(
    abilities = matrix(c(-0.5, 0.2, 0.6), nrow = 1, ncol = 3, byrow = TRUE)
  )

  expect_error(
    foot_abilities(fake_student_t, england_2004, teams = teams_test),
    "Please, select at least two teams"
  )
})


## all the test PASSED (also the skipped ones!)

test_that("foot_rank errors when required columns are missing", {
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
    method = "VI",
    seed = 433
  )


  # Remove a required column from the data
  expect_error(
    foot_rank(model_pois, england_2004_wrg),
    regexp = "data is missing required columns:"
  )
})



test_that("foot_rank errors for invalid 'visualize' or 'output' argument", {
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
    method = "VI",
    seed = 433
  )

  expect_error(
    foot_rank(model_pois, england_2004, visualize = "invalid")
  )

  expect_error(
    foot_rank(model_pois, england_2004, output = "invalid")
  )
})



test_that("foot_rank returns aggregated output as a list with rank_table and rank_plot poisson", {
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
    method = "VI",
    seed = 433
  )
  res <- foot_rank(model_pois$fit, england_2004, visualize = "aggregated")
  expect_type(res, "list")
  expect_true(all(c("rank_table", "rank_plot") %in% names(res)))
  expect_true(inherits(res$rank_plot, "ggplot"))
  expect_true(is.data.frame(res$rank_table))


  model_pois_pred <- stan_foot(
    data = england_2004,
    model = "double_pois",
    predict = 30,
    method = "VI",
    seed = 433
  )
  res <- foot_rank(model_pois_pred, england_2004, visualize = "aggregated")
  expect_type(res, "list")
  expect_true(all(c("rank_table", "rank_plot") %in% names(res)))
  expect_true(inherits(res$rank_plot, "ggplot"))
  expect_true(is.data.frame(res$rank_table))
})



test_that("foot_rank returns aggregated output as a list with rank_table and rank_plot skellam", {
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

  model_skellam <- stan_foot(
    data = england_2004,
    model = "skellam",
    method = "VI",
    seed = 433
  )
  res <- foot_rank(model_skellam, england_2004, visualize = "aggregated")
  expect_type(res, "list")
  expect_true(all(c("rank_table", "rank_plot") %in% names(res)))
  expect_true(inherits(res$rank_plot, "ggplot"))
  expect_true(is.data.frame(res$rank_table))


  model_skellam_pred <- stan_foot(
    data = england_2004,
    model = "skellam",
    predict = 30,
    method = "VI",
    seed = 433
  )
  res <- foot_rank(model_skellam_pred, england_2004, visualize = "aggregated")
  expect_type(res, "list")
  expect_true(all(c("rank_table", "rank_plot") %in% names(res)))
  expect_true(inherits(res$rank_plot, "ggplot"))
  expect_true(is.data.frame(res$rank_table))
})



test_that("foot_rank returns aggregated output as a list with rank_table and rank_plot student_t", {
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

  model_student <- stan_foot(
    data = england_2004,
    model = "student_t",
    method = "VI",
    seed = 433
  )
  res <- foot_rank(model_student, england_2004, visualize = "aggregated")
  expect_type(res, "list")
  expect_true(all(c("rank_table", "rank_plot") %in% names(res)))
  expect_true(inherits(res$rank_plot, "ggplot"))
  expect_true(is.data.frame(res$rank_table))


  model_student_pred <- stan_foot(
    data = england_2004,
    model = "student_t",
    predict = 30,
    method = "VI",
    seed = 433
  )
  res <- foot_rank(model_student_pred, england_2004, visualize = "aggregated")
  expect_type(res, "list")
  expect_true(all(c("rank_table", "rank_plot") %in% names(res)))
  expect_true(inherits(res$rank_plot, "ggplot"))
  expect_true(is.data.frame(res$rank_table))
})




test_that("foot_rank returns a ggplot object for individual visualization for skellam", {
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

  # In-sample
  model_skellam <- stan_foot(
    data = england_2004,
    model = "skellam",
    method = "VI",
    seed = 433
  )

  res <- foot_rank(model_skellam, england_2004, visualize = "individual")
  expect_true(inherits(res, "ggplot"))

  # Out-of-sample
  model_skellam_pred <- stan_foot(
    data = england_2004,
    model = "skellam",
    predict = 30,
    method = "VI",
    seed = 433
  )

  res <- foot_rank(model_skellam_pred, england_2004, visualize = "individual")
  expect_true(inherits(res, "ggplot"))
})

test_that("foot_rank returns a ggplot object for individual visualization for student_t", {
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

  # In-sample
  model_student <- stan_foot(
    data = england_2004,
    model = "student_t",
    method = "VI",
    seed = 433
  )

  res <- foot_rank(model_student, england_2004, visualize = "individual")
  expect_true(inherits(res, "ggplot"))

  # Out-of-sample
  model_student_pred <- stan_foot(
    data = england_2004,
    model = "student_t",
    predict = 30,
    method = "VI",
    seed = 433
  )

  res <- foot_rank(model_student_pred, england_2004, visualize = "individual")
  expect_true(inherits(res, "ggplot"))
})



test_that("foot_rank returns a ggplot object for individual visualization for poisson", {
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

  # In-sample
  model_pois <- stan_foot(
    data = england_2004,
    model = "student_t",
    method = "VI",
    seed = 433
  )

  res <- foot_rank(model_pois, england_2004, visualize = "individual")
  expect_true(inherits(res, "ggplot"))

  model_pois_pred <- stan_foot(
    data = england_2004,
    model = "student_t",
    predict = 30,
    method = "VI",
    seed = 433
  )

  res <- foot_rank(model_pois_pred, england_2004, visualize = "individual")
  expect_true(inherits(res, "ggplot"))

  res <- foot_rank(model_pois_pred, england_2004, visualize = "individual", teams = "Arsenal")
  expect_true(inherits(res, "ggplot"))

  res <- foot_rank(model_pois_pred, england_2004,
    visualize = "individual",
    teams = c("Arsenal", "Chelsea")
  )
  expect_true(inherits(res, "ggplot"))
})

test_that("foot_rank warns when provided team names are not in the test set", {
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
    method = "VI",
    seed = 433
  )
  expect_warning(
    foot_rank(model_pois, england_2004, teams = c("NoTrueTeam")),
    regexp = "is not in the test set"
  )
})


test_that("foot_rank errors when out-of-sample matches belong to multiple seasons", {
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
  model_pois <- stan_foot(
    data = england_04_05,
    model = "double_pois",
    predict = 390,
    method = "VI",
    seed = 433
  )

  expect_error(
    foot_rank(model_pois, england_04_05),
    regexp = "Out-of-sample matches must belong to one season"
  )
})


test_that("foot_rank errors when the number of out-of-sample matches is too small", {
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
  model_pois <- stan_foot(
    data = england_04_05,
    model = "double_pois",
    predict = 2,
    method = "VI",
    seed = 433
  )

  expect_error(
    foot_rank(model_pois, england_04_05)
  )
})



test_that("foot_rank ensure that more out-of-sample matches (at least two complete match-days) for predictions", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####

  data("england")
  england <- as.data.frame(england)

  # One season only
  england_04 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season %in% c("2004"))


  colnames(england_04) <- c(
    "periods", "home_team", "away_team",
    "home_goals", "away_goals"
  )

  ##  ............................................................................
  ##  Tests                                                                   ####
  model_pois <- stan_foot(
    data = england_04,
    model = "double_pois",
    predict = 19,
    method = "VI",
    seed = 433
  )

  expect_error(
    foot_rank(model_pois, england_04)
  )


  model_pois <- stan_foot(
    data = england_04,
    model = "double_pois",
    predict = 38,
    method = "VI",
    seed = 433
  )

  expect_error(
    foot_rank(model_pois, england_04), NA
  )
})


test_that("foot_rank when the teams of out-of-sample matches are different", {
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



  england_04_05_na <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season %in% c("2004", "2005")) %>%
    dplyr::mutate(
      hgoal = ifelse(Season == "2005", NA, hgoal),
      vgoal = ifelse(Season == "2005", NA, vgoal)
    )

  colnames(england_04_05_na) <- c(
    "periods", "home_team", "away_team",
    "home_goals", "away_goals"
  )
  ##  ............................................................................
  ##  Tests                                                                   ####
  model_pois <- stan_foot(
    data = england_04_05,
    model = "double_pois",
    predict = 380,
    method = "VI",
    seed = 433
  )

  expect_error(
    foot_rank(model_pois, england_04_05), NA
  )

  # Unknown future results
  model_pois_na <- stan_foot(
    data = england_04_05_na,
    model = "double_pois",
    predict = 380,
    method = "VI",
    seed = 433
  )

  expect_error(
    foot_rank(model_pois_na, england_04_05_na), NA
  )
})


test_that("foot_rank when the teams of in-sample matches", {
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
  model_pois <- stan_foot(
    data = england_04_05,
    model = "double_pois",
    method = "VI",
    seed = 433
  )

  expect_error(
    foot_rank(model_pois, england_04_05, visualize = "individual"), NA
  )
  expect_error(
    foot_rank(model_pois, england_04_05, visualize = "aggregated"), NA
  )

  model_skellam <- stan_foot(
    data = england_04_05,
    model = "skellam",
    method = "VI",
    seed = 433
  )

  expect_error(
    foot_rank(model_skellam, england_04_05, visualize = "individual"), NA
  )
  expect_error(
    foot_rank(model_skellam, england_04_05, visualize = "aggregated"), NA
  )


  model_student <- stan_foot(
    data = england_04_05,
    model = "student_t",
    method = "VI",
    seed = 433
  )

  expect_error(
    foot_rank(model_student, england_04_05, visualize = "individual"), NA
  )
  expect_error(
    foot_rank(model_student, england_04_05, visualize = "aggregated"), NA
  )
})




test_that("foot_rank works for cond_2", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())


  data_train <- tibble(
    periods = rep(1, 13),
    home_team = c(
      "Juventus", "Inter", "AC Milan", "Napoli", "Roma",
      "Juventus", "Inter", "AC Milan", "Napoli", "Roma",
      "Juventus", "Inter", "AC Milan"
    ),
    away_team = c(
      "Inter", "AC Milan", "Napoli", "Roma", "Juventus",
      "AC Milan", "Roma", "Roma", "Juventus", "Inter",
      "Napoli", "Juventus", "Roma"
    ),
    home_goals = c(2, 1, 0, 3, 1, 2, 1, 1, 2, 0, 3, 1, 2),
    away_goals = c(1, 1, 2, 0, 2, 0, 0, 1, 2, 1, 1, 1, 0)
  )


  data_test <- tibble(
    periods = rep(2, 4),
    home_team = c("Juventus", "Inter", "AC Milan", "Napoli"),
    away_team = c("Inter", "AC Milan", "Napoli", "Juventus"),
    home_goals = c(2, 1, 0, 1),
    away_goals = c(1, 2, 1, 1)
  )


  data_full <- bind_rows(data_train, data_test)


  fit <- stan_foot(
    data = data_full,
    model = "double_pois",
    predict = 4
  )

  expect_error(foot_rank(fit, data_full, visualize = "aggregated"), NA)
})



test_that("foot_rank works for cond_2 individual", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  training_data <- tibble(
    periods = rep(1, 19),
    home_team = c(
      "Juventus", "Inter", "AC Milan", "Napoli", "Roma",
      "Juventus", "Inter", "AC Milan", "Napoli", "Roma",
      "Juventus", "Inter", "AC Milan", "Napoli", "Roma",
      "Juventus", "Inter", "AC Milan", "Roma"
    ),
    away_team = c(
      "Inter", "AC Milan", "Napoli", "Roma", "Juventus",
      "AC Milan", "Napoli", "Roma", "Juventus", "Inter",
      "Napoli", "Roma", "Juventus", "Inter", "AC Milan",
      "Roma", "Juventus", "Inter", "Napoli"
    ),
    home_goals = sample(0:3, 19, replace = TRUE),
    away_goals = sample(0:3, 19, replace = TRUE)
  )

  test_data <- tibble(
    periods = rep(2, 4),
    home_team = c("Juventus", "Inter", "AC Milan", "Napoli"),
    away_team = c("Inter", "AC Milan", "Napoli", "Juventus"),
    home_goals = sample(0:3, 4, replace = TRUE),
    away_goals = sample(0:3, 4, replace = TRUE)
  )

  data_full <- bind_rows(training_data, test_data)


  fit <- stan_foot(
    data = data_full,
    model = "double_pois",
    predict = 4
  )

  expect_error(foot_rank(fit, data_full, visualize = "individual"), NA)
})

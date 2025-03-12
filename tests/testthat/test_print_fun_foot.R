#   ____________________________________________________________________________
#   Tests for print.stanFoot                                                ####


test_that("print.stanFoot errors for non-positive digits", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####
  data("england")

  england_1999 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "1999")

  colnames(england_1999) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")


  ##  ............................................................................
  ##  Tests                                                                   ####

  model <- stan_foot(england_1999, "biv_pois",
                     method = "VI"
  )
  expect_error(print(model, digits = -1), "'digits' must be a positive numeric value.")
})

test_that("print.stanFoot errors if fit is not a CmdStanFit object", {
  # Dummy CmdStanFit for stanFoot
  dummy_fit_stan <- list(
    metadata = function() {
      list(stan_variables = c("att[1]", "def[1]", "home", "sigma_att", "sigma_def", "rho", "beta"))
    },
    summary = function(variables = NULL) {
      if (is.null(variables)) {
        data.frame(
          variable = c("att[1]", "def[1]", "home", "sigma_att", "sigma_def", "rho", "beta"),
          mean = 1:7,
          sd = 1:7,
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          variable = variables,
          mean = rep(1, length(variables)),
          sd = rep(1, length(variables)),
          stringsAsFactors = FALSE
        )
      }
    },
    code = function() "stan_code_dummy"
  )
  class(dummy_fit_stan) <- "CmdStanFit"

  # Dummy stanFoot object (static model)
  dummy_stanFoot <- list(
    fit = dummy_fit_stan,
    data = data.frame(
      home_team = c("Team A", "Team B"),
      away_team = c("Team B", "Team A"),
      home_goals = c(1, 2),
      away_goals = c(0, 3),
      stringsAsFactors = FALSE
    ),
    stan_data = list(ntimes = 1),
    stan_code_path = "dummy_path",
    stan_args = list(),
    alg_method = "MCMC"
  )
  class(dummy_stanFoot) <- "stanFoot"
  wrong_fit_obj <- dummy_stanFoot
  wrong_fit_obj$fit <- list() # Not a CmdStanFit
  expect_error(print(wrong_fit_obj), "The 'fit' component must be a 'CmdStanFit' object.")
})

test_that("print.stanFoot prints the summary header", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####
  data("england")

  england_99_00 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "1999" | Season == "2000")

  colnames(england_99_00) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")


  ##  ............................................................................
  ##  Tests                                                                   ####

  model <- stan_foot(england_99_00, "biv_pois",
                     method = "VI",
                     seed = 433
  )
  output <- capture.output(print(model))
  expect_true(any(grepl("Summary of Stan football model", output)))
  expect_true(any(grepl("Posterior summaries for model parameters:", output)))


  model_dyn <- stan_foot(england_99_00, "biv_pois",
                         dynamic_type = "seasonal",
                         method = "VI",
                         seed = 433
  )
  expect_error(print(model_dyn), NA)

})



test_that("print.stanFoot filters parameters using 'pars'", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####
  data("england")

  england_1999 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "1999")

  colnames(england_1999) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")


  ##  ............................................................................
  ##  Tests                                                                   ####

  model <- stan_foot(england_1999, "biv_pois",
                     method = "VI",
                     seed = 433
  )
  # When specifying pars = "att", only parameters matching "att" should be printed.
  output <- capture.output(print(model, pars = "att"))
  output <- capture.output(print(model, pars = "att"))
  expect_true(any(grepl("att\\[", output))) # Check that 'att[' appears
  expect_false(any(grepl("def\\[", output))) # Ensure no 'def[' parameters appear
})


test_that("print.stanFoot errors-warnings when invalid teams are provided", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####
  data("england")

  england_1999 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "1999")

  colnames(england_1999) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")


  ##  ............................................................................
  ##  Tests                                                                   ####

  model <- stan_foot(england_1999, "biv_pois",
                     method = "VI",
                     seed = 433
  )
  expect_warning(print(model, teams = c("Milan","Arsenal")))
  expect_error(print(model, teams = "Milan"))
})



#   ____________________________________________________________________________
#   Tests for print.btdFoot                                                 ####


test_that("print.btdFoot errors for non-positive digits", {
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

  model <- btd_foot(
    data = england_2004,
    dynamic_rank = FALSE,
    method = "VI",
    seed = 433
  )
  expect_error(print(model, digits = 0), "'digits' must be a positive numeric value.")
})


test_that("print.btdFoot prints model header and ranking table", {
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
    dplyr::filter(Season == "2004" | Season == "2005") %>%
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

  model <- btd_foot(
    data = england_04_05,
    dynamic_rank = FALSE,
    method = "VI",
    seed = 433
  )
  output <- capture.output(print(model))
  expect_true(any(grepl("Bayesian Bradley-Terry-Davidson model", output)))
  expect_true(any(grepl("Top teams based on relative log-strengths", output)))


  model_dyn <- btd_foot(
    data = england_04_05,
    dynamic_rank = TRUE,
    method = "VI",
    seed = 433
  )
  output <- capture.output(print(model_dyn))
  expect_true(any(grepl("Bayesian Bradley-Terry-Davidson model", output)))
  expect_true(any(grepl("Top teams based on relative log-strengths", output)))
})


test_that("print.btdFoot filters parameters using 'pars'", {
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

  model <- btd_foot(
    data = england_2004,
    dynamic_rank = FALSE,
    method = "VI",
    seed = 433
  )
  output <- capture.output(print(model, pars = "logStrength"))
  expect_true(any(grepl("logStrength", output)))
})


test_that("print.btdFoot filters team-specific parameters", {
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
    dplyr::filter(Season == "2004" | Season == "2005") %>%
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

  model <- btd_foot(
    data = england_04_05,
    dynamic_rank = FALSE,
    method = "VI",
    seed = 433
  )
  expect_error(print(model, teams = "Arsenal"), NA)


  model_dyn <- btd_foot(
    data = england_04_05,
    dynamic_rank = TRUE,
    method = "VI",
    seed = 433
  )
  expect_error(print(model_dyn, teams = "Arsenal"), NA)
})


test_that("print.btdFoot errors-warnings when invalid teams are provided", {
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

  model <- btd_foot(
    data = england_2004,
    dynamic_rank = FALSE,
    method = "VI"
  )
  expect_warning(print(model, teams = c("Milan","Arsenal")))
  expect_error(print(model, teams = "Milan"))
})


#   ____________________________________________________________________________
#   Tests for print.compareFoot                                             ####



test_that("print.compareFoot prints predictive performance metrics", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####
  data("england")

  england_1999 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "1999")

  colnames(england_1999) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")


  ##  ............................................................................
  ##  Tests                                                                   ####

  model_1 <- stan_foot(england_1999,
                       "biv_pois",
                       predict = 30,
                       method = "VI",
                       seed = 433
  )

  model_2 <- stan_foot(england_1999,
                       "double_pois",
                       predict = 30,
                       method = "VI",
                       seed = 433
  )
  comp <- compare_foot(
    list(double_poisson = model_2, bivariate_poisson = model_1),
    england_1999[351:380, ]
  )
  output <- capture.output(print(comp))
  expect_true(any(grepl("Predictive Performance Metrics", output)))
})

test_that("print.compareFoot prints confusion matrices", {
  skip_on_cran()
  skip_if_not(stan_cmdstan_exists())

  ##  ............................................................................
  ##  Data                                                                    ####
  data("england")

  england_1999 <- england %>%
    dplyr::filter(division == 1) %>%
    dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
    dplyr::filter(Season == "1999")

  colnames(england_1999) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")


  ##  ............................................................................
  ##  Tests                                                                   ####

  model_1 <- stan_foot(england_1999,
                       "biv_pois",
                       predict = 30,
                       method = "VI",
                       seed = 433
  )

  model_2 <- stan_foot(england_1999,
                       "double_pois",
                       predict = 30,
                       method = "VI",
                       seed = 433
  )
  comp <- compare_foot(list(double_poisson = model_2, bivariate_poisson = model_1),
                       england_1999[351:380, ],
                       conf_matrix = TRUE
  )
  output <- capture.output(print(comp))
  expect_true(any(grepl("Confusion Matrices", output)))
  expect_true(any(grepl("Model:", output)))
})



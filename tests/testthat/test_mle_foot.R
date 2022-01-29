# packages
library(tidyverse)
library(engsoccerdata)

# data
italy <- as_tibble(italy)
italy_2008<- italy %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter( Season=="2008")

spain <- as_tibble(spain)
spain_2008<- spain %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter( Season=="2008")

germany <- as_tibble(germany)
germany_2008<- germany %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter( Season=="2008")


context("mle plain models")
# These tests regard the performance of the mle_foot functions,
# for methods double_pois, biv_pois, skellam and student_t.

test_that("some mle models get errors/warnings", {


  expect_warning(mle_foot(data = italy_2008,
           model ="skellam"))
  # warnings: "precision lost in result"
  # ---> corrected if using "L-BFGS-B" as method

  expect_warning(mle_foot(data = italy_2008,
           model ="student_t"))
  # warnings: log(sd)
  # ---> corrected if using "Nelder-Mead" as method

  # datasets with less than 4 columns
  expect_error(mle_foot(data = italy_2008[,1:3],
                        model = "biv_pois"))

  # datasets different than matrix and dataframe
  expect_error(mle_foot(data = rnorm(20),
                        model = "biv_pois"))

  # wrong model names
  expect_error(mle_foot(data = italy_2008,
                        model = "neg_binomial"))


})


test_that("prediction causes  warnings/errors", {

  ## predict > N
  expect_error(mle_foot(italy_2008,
                         model ="student_t",
                         predict = dim(italy_2008)[1]+1))

  ## predict not a number
  expect_error(mle_foot(italy_2008,
                         model ="student_t",
                         predict = "a"))

  ## predict decimal number
  # expect_warning(stan_foot(england_2004,
  #           model ="student_t",
  #           predict = 30.5))
})










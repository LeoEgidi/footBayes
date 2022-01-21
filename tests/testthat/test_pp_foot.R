library(tidyverse)
library(engsoccerdata)


italy_2000<- italy %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter(Season=="2000")


test_that("expect error in input",{

  # other than stanfit class
  fit <- mle_foot(italy_2000, "double_pois")
  expect_error(pp_foot(italy_2000, fit))

  # wrong type
  fit1 <- stan_foot(italy_2000, "double_pois", iter = 200)
  expect_error(pp_foot(italy_2000, fit1, "rank"))

  # scale_x_discrete
  expect_warning(italy_2000, fit1, "matches")


})

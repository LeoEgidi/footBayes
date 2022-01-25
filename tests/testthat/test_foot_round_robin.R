library(engsoccerdata)
library(tidyverse)

italy_2000<- italy %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter(Season=="2000")

italy_1999_2000<- italy %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter(Season == "1999"|Season=="2000")


# in-sample
testthat("in-sample models", {
  fit_dyn <- stan_foot(italy_1999_2000, "double_pois", iter = 200)
  expect_error(foot_round_robin(italy_1999_2000, fit_dyn))
  expect_error(foot_round_robin(italy_1999_2000, fit_dyn, "AS Roma"))
  expect_error(foot_round_robin(italy_1999_2000, fit_dyn, c("AS Roma", "US Lecce")))
  expect_error(foot_round_robin(italy_1999_2000, fit_dyn, c("AS Roma", "US Lecce", "Parma AC")))
})

# out-of-sample
testthat("out-of-sample models", {
  fit_dyn_out <- stan_foot(italy_1999_2000, "double_pois", predict = 45, iter = 200)
  expect_error(foot_round_robin(italy_1999_2000, fit_dyn_out), NA)
  expect_error(foot_round_robin(italy_1999_2000, fit_dyn_out, "AS Roma"), NA) # black window
  expect_error(foot_round_robin(italy_1999_2000, fit_dyn_out, c("AS Roma", "US Lecce")), NA)
  expect_error(foot_round_robin(italy_1999_2000, fit_dyn_out, c("AS Roma", "US Lecce", "Parma AC")), NA)
})

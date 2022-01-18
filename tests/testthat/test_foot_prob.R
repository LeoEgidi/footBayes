library(tidyverse)
library(engsoccerdata)


# use student_t model

italy_2000<- italy %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter(Season=="2000")

testthat("foot_prob gets errors", {

  #  with student_t
  fit <- stan_foot(italy_2000, "student_t", predict = 36, iter = 200)
  expect_error(foot_prob(fit, italy_2000))

  # different numbers of home_team and away team
  fit <- stan_foot(italy_2000, "biv_pois", predict = 36, iter = 200)
  expect_error(foot_prob(fit, italy_2000, home_team = c("Hellas Verona", "US Lecce"),
            away_team = c("AC Perugia")))

  # no predict in stan_foot/mle_foot
  fit <- stan_foot(italy_2000, "biv_pois", iter = 200)
  expect_error(foot_prob(fit, italy_2000))

  fit <- mle_foot(italy_2000, "biv_pois", iter = 200)
  expect_error(foot_prob(fit, italy_2000))



})

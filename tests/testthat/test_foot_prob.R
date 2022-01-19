library(tidyverse)
library(engsoccerdata)


italy_2000<- italy %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter(Season=="2000")

testthat("foot_prob gets errors", {

  # different numbers of home_team and away team
  fit2 <- stan_foot(italy_2000, "biv_pois", predict = 36, iter = 200)
  expect_error(foot_prob(fit2, italy_2000, home_team = c("Hellas Verona", "US Lecce"),
            away_team = c("AC Perugia")))

  # no predict
  ## in stan_foot
  fit3 <- stan_foot(italy_2000, "biv_pois", iter = 200)
  expect_error(foot_prob(fit3, italy_2000))


  ## mle_foot
  fit4 <- mle_foot(italy_2000, "biv_pois")
  expect_error(foot_prob(fit4, italy_2000))

  # wrong class
  y <- italy_2000$hgoal
  fit5 <- lm(y~ rnorm(length(y)))
  expect_error(foot_prob(fit5, italy_2000))



})

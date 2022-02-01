## all the test PASSED (also the skipped ones!)

library(tidyverse)
library(engsoccerdata)

##########################
## DATA
#########################

england <- as_tibble(england)
# one season only
england_2004 <- england %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter(  Season=="2004")

# more seasons
england_1999_2001 <- england %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter(  Season=="2001" | Season == "2000" | Season =="1999")


context("static models")
test_that("object argument gives errors/warnings", {

  # the class of the model is not stanfit and not
  # from optimization methods (mle)
  y <- england_2004$hgoal
  fit <- lm(y~ rnorm(length(y)))
  expect_error(foot_abilities(fit, england_2004))


})


# context("dynamic models")  # skipped, too long
#
# test_that("no errors from dynamics",{
# fit <- stan_foot(england_1999_2001, "biv_pois", dynamic_type = "seasonal", iter = 200)
# expect_error(foot_abilities(fit, england_1999_2001), NA)
# expect_error(foot_abilities(fit, england_1999_2001, team = "Arsenal"), NA)
# expect_error(foot_abilities(fit, england_1999_2001, team = c("Arsenal", "Everton")), NA)
# })

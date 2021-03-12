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



context("dynamic models")

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


test_that("some mle models get errors/warnings", {



  expect_warning(mle_foot(data = italy_2008,
           model ="skellam"))
  # warnings: "precision lost in result"
  # ---> corrected if using "L-BFGS-B" as method

  expect_warning(mle_foot(data = italy_2008,
           model ="student_t"))
  # warnings: log(sd)
  # ---> corrected if using "Nelder-Mead" as method

  # mle_foot(data = italy_2008,
  #          model ="biv_pois")
  #
  # mle_foot(data = italy_2008,
  #          model ="double_pois")

  ## with further arguments
  # mle_foot(data = italy_2008,
  #          model ="biv_pois",
  #          interval = "Wald")
  # # problemi in ci di alcune squadre
  # # ---> corrected, error at line 320 in mle_foot
  #
  # mle_foot(data = italy_2008,
  #          model ="double_pois",
  #          interval = "Wald")
  #
  # mle_foot(data = italy_2008,
  #          model ="biv_pois", maxit = 100,
  #          interval = "Wald", hessian = TRUE)
  #
  # mle_foot(data = italy_2008,
  #          model ="biv_pois",
  #          interval = "Wald", hessian = TRUE)
})














library(engsoccerdata)
library(tidyverse)

italy_2000<- italy %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter(Season=="2000")

italy_1999_2000<- italy %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter(Season == "1999"|Season=="2000")



context("one season")

# in-sample
fit <- stan_foot(italy_2000, "double_pois", iter = 200)
fit_t <- stan_foot(italy_2000, "student_t", iter = 200)

test_that("in-sample models", {
expect_error(foot_rank(italy_2000, fit), NA)
expect_error(foot_rank(italy_2000, fit, "AS Roma"), NA)
expect_error(foot_rank(italy_2000, fit, "AS Roma", "individual"), NA)
expect_error(foot_rank(italy_2000, fit, c("AS Roma", "US Lecce")), NA)
expect_error(foot_rank(italy_2000, fit, c("AS Roma", "US Lecce"), "individual"), NA)
expect_error(foot_rank(italy_2000, fit, visualize =  "individual"), NA)

expect_error(foot_rank(italy_2000, fit_t), NA)
expect_error(foot_rank(italy_2000, fit_t, "AS Roma"), NA)
expect_error(foot_rank(italy_2000, fit_t, "AS Roma", "individual"), NA)
expect_error(foot_rank(italy_2000, fit_t, c("AS Roma", "US Lecce")), NA)
expect_error(foot_rank(italy_2000, fit_t, c("AS Roma", "US Lecce"), "individual"), NA)
expect_error(foot_rank(italy_2000, fit_t, visualize =  "individual"), NA)
})

# out-of-sample

test_that("out-of-sample models", {
fit_out <- stan_foot(italy_2000, "double_pois", iter = 200, predict = 45)
expect_error(foot_rank(italy_2000, fit_out), NA)
expect_error(foot_rank(italy_2000, fit_out, visualize = "individual"), NA)
expect_error(foot_rank(italy_2000, fit_out, "AS Roma",  visualize = "individual"), NA)
expect_error(foot_rank(italy_2000, fit_out, c("AS Roma", "US Lecce", "Lazio Roma"), "individual"), NA)

fit_out_few <- stan_foot(italy_2000, "double_pois", iter = 200, predict = 4)
expect_error(foot_rank(italy_2000, fit_out_few))
expect_error(foot_rank(italy_2000, fit_out_few, visualize = "individual"))
expect_error(foot_rank(italy_2000, fit_out_few, "AS Roma",  visualize = "individual"))
expect_error(foot_rank(italy_2000, fit_out_few, c("AS Roma", "US Lecce", "Lazio Roma"), "individual"))

fit_out_last <- stan_foot(italy_2000, "double_pois", iter = 200, predict = 12)
expect_error(foot_rank(italy_2000, fit_out_last))
expect_error(foot_rank(italy_2000, fit_out_last, visualize = "individual"))
expect_error(foot_rank(italy_2000, fit_out_last, "AS Roma",  visualize = "individual"))
expect_error(foot_rank(italy_2000, fit_out_last, c("AS Roma", "US Lecce", "Lazio Roma"), "individual"))

fit_out_but_last <- stan_foot(italy_2000, "double_pois", iter = 200, predict = 18)
expect_error(foot_rank(italy_2000, fit_out_but_last), NA)
expect_error(foot_rank(italy_2000, fit_out_but_last, visualize = "individual"), NA)
expect_error(foot_rank(italy_2000, fit_out_but_last, "AS Roma",  visualize = "individual"),NA)
expect_error(foot_rank(italy_2000, fit_out_but_last, c("AS Roma", "US Lecce", "Lazio Roma"), "individual"), NA)
})



context("more seasons")

# in-sample
testthat("in-sample models", {
fit_dyn <- stan_foot(italy_1999_2000, "double_pois", iter = 200)
expect_error(foot_rank(italy_1999_2000, fit_dyn), NA)
expect_error(foot_rank(italy_1999_2000, fit_dyn, "AS Roma"), NA)
expect_error(foot_rank(italy_1999_2000, fit_dyn, "AS Roma", "individual"), NA)
expect_error(foot_rank(italy_1999_2000, fit_dyn, c("AS Roma", "US Lecce")), NA)
expect_error(foot_rank(italy_1999_2000, fit_dyn, c("AS Roma", "US Lecce"), "individual"), NA)
expect_error(foot_rank(italy_1999_2000, fit_dyn, visualize =  "individual"), NA)
})

# out-of-sample
testthat("out-of-sample models", {
fit_dyn_out <- stan_foot(italy_1999_2000, "double_pois", predict = 45, iter = 200)
expect_error(foot_rank(italy_1999_2000, fit_dyn_out), NA)
expect_error(foot_rank(italy_1999_2000, fit_dyn_out, "AS Roma"), NA)
expect_error(foot_rank(italy_1999_2000, fit_dyn_out, "AS Roma", "individual"), NA)
expect_error(foot_rank(italy_1999_2000, fit_dyn_out, c("AS Roma", "US Lecce")), NA)
expect_error(foot_rank(italy_1999_2000, fit_dyn_out, c("AS Roma", "US Lecce"), "individual"), NA)
expect_error(foot_rank(italy_1999_2000, fit_dyn_out, visualize =  "individual"), NA)

fit_dyn_out_last <- stan_foot(italy_1999_2000, "double_pois", predict = 12, iter = 200)
expect_error(foot_rank(italy_1999_2000, fit_dyn_out_last))
expect_error(foot_rank(italy_1999_2000, fit_dyn_out_last, "AS Roma"))
expect_error(foot_rank(italy_1999_2000, fit_dyn_out_last, "AS Roma", "individual"))
expect_error(foot_rank(italy_1999_2000, fit_dyn_out_last, c("AS Roma", "US Lecce")))
expect_error(foot_rank(italy_1999_2000, fit_dyn_out_last, c("AS Roma", "US Lecce"), "individual"))
expect_error(foot_rank(italy_1999_2000, fit_dyn_out_last, visualize =  "individual"))

fit_dyn_out_too <- stan_foot(italy_1999_2000, "double_pois", predict = 600, iter = 200)
expect_error(foot_rank(italy_1999_2000, fit_dyn_out_too))
expect_error(foot_rank(italy_1999_2000, fit_dyn_out_too, "AS Roma"))
expect_error(foot_rank(italy_1999_2000, fit_dyn_out_too, "AS Roma", "individual"))
expect_error(foot_rank(italy_1999_2000, fit_dyn_out_too, c("AS Roma", "US Lecce")))
expect_error(foot_rank(italy_1999_2000, fit_dyn_out_too, c("AS Roma", "US Lecce"), "individual"))
expect_error(foot_rank(italy_1999_2000, fit_dyn_out_too, visualize =  "individual"))
})



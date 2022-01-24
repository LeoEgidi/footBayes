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
foot_rank(italy_2000, fit)
foot_rank(italy_2000, fit, "AS Roma")
foot_rank(italy_2000, fit, "AS Roma", "individual")
foot_rank(italy_2000, fit, c("AS Roma", "US Lecce"))
foot_rank(italy_2000, fit, c("AS Roma", "US Lecce"), "individual")
foot_rank(italy_2000, fit, visualize =  "individual")

foot_rank(italy_2000, fit_t)
foot_rank(italy_2000, fit_t, "AS Roma")
foot_rank(italy_2000, fit_t, "AS Roma", "individual")
foot_rank(italy_2000, fit_t, c("AS Roma", "US Lecce"))
foot_rank(italy_2000, fit_t, c("AS Roma", "US Lecce"), "individual")
foot_rank(italy_2000, fit_t, visualize =  "individual")




# out-of-sample
fit_out <- stan_foot(italy_2000, "double_pois", iter = 200, predict = 45)
foot_rank(italy_2000, fit_out)
foot_rank(italy_2000, fit_out, visualize = "individual")
foot_rank(italy_2000, fit_out, "AS Roma",  visualize = "individual")
foot_rank(italy_2000, fit_out, c("AS Roma", "US Lecce", "Lazio Roma"), "individual")

context("more seasons")

# in-sample
fit_dyn <- stan_foot(italy_1999_2000, "double_pois", iter = 200)
foot_rank(italy_1999_2000, fit_dyn)
foot_rank(italy_1999_2000, fit_dyn, "AS Roma")
foot_rank(italy_1999_2000, fit_dyn, "AS Roma", "individual")
foot_rank(italy_1999_2000, fit_dyn, c("AS Roma", "US Lecce"))
foot_rank(italy_1999_2000, fit_dyn, c("AS Roma", "US Lecce"), "individual")
foot_rank(italy_1999_2000, fit_dyn, visualize =  "individual")


# out-of-sample
fit_dyn_out <- stan_foot(italy_1999_2000, "double_pois", predict = 45, iter = 200)
foot_rank(italy_1999_2000, fit_dyn_out)
foot_rank(italy_1999_2000, fit_dyn_out, "AS Roma")
foot_rank(italy_1999_2000, fit_dyn_out, "AS Roma", "individual")
foot_rank(italy_1999_2000, fit_dyn_out, c("AS Roma", "US Lecce"))
foot_rank(italy_1999_2000, fit_dyn_out, c("AS Roma", "US Lecce"), "individual")
foot_rank(italy_1999_2000, fit_dyn_out, visualize =  "individual")




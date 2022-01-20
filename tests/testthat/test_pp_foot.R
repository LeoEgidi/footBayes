library(tidyverse)
library(engsoccerdata)


italy_2000<- italy %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter(Season=="2000")
fit1 <- stan_foot(italy_2000, "double_pois", iter = 200)
fit2 <- stan_foot(italy_2000, "biv_pois", iter = 200)
fit3 <- stan_foot(italy_2000, "skellam", iter = 200)
fit4 <- stan_foot(italy_2000, "student_t", iter = 200)

pp_foot(italy_2000, fit1)

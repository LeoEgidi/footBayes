library(tidyverse)
library(engsoccerdata)


italy <- as_tibble(italy)
italy_2008<- italy %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter( Season=="2008")

### STATIC
  ## MLE
  # double_pois, skellam, biv_pois
  obj <- mle_foot(data = italy_2008,
         model ="skellam")
  foot_abilities(obj, italy_2008)

  # student-t
  obj2 <- mle_foot(data = italy_2008,
                  model ="student_t")
  foot_abilities(obj2, italy_2008)


    # + further arguments
    foot_abilities(obj, italy_2008,
                  mar =c(3,7,3,1), pch = 6, cex.main = 1, cex.var = 0.8)
    foot_abilities(obj2, italy_2008,
                   mar =c(3,4,3,1), pch = 6, cex.main = 2, cex.var = 0.8,
                   col ="orange")

  ## Bayesian
  obj3 <- stan_foot(data = italy_2008,
                    model ="double_pois")
  foot_abilities(obj3, italy_2008)

  # + further arguments
  foot_abilities(obj3, italy_2008,
                 mar =c(3,7,3,1), pch = 6, cex.main = 1, cex.var = 0.8)


### DYNAMIC





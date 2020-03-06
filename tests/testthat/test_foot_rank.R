
library(engsoccerdata)
library(tidyverse)

# use one season to predict part of the same season
italy_2007<- italy %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter( Season=="2007")
fit1 <- stan_foot(data = italy_2007,
                  model="double_pois", predict = 100)
foot_rank(data = italy_2007, object= fit1,
          team_sel = c("AS Roma", "Inter", "Atalanta"),
          type="out-of-sample",visualize = 2)
foot_rank(data = italy_2007, object= fit1,
          team_sel = c("AS Roma", "Inter", "Atalanta"),
          type="out-of-sample",visualize = 1)
#

# use two seasons to entirely predict the third season
italy <- as_tibble(italy)
italy_2000_2002<- italy %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter(Season=="2000" |  Season=="2001"| Season=="2002")
fit6 <- stan_foot(data = italy_2000_2002,
                  model="double_pois",
                  dynamic_type ="seasonal",
                  predict = 306) # double poisson

foot_rank(data = italy_2000_2002,
          object= fit6,
          team_sel = c("AS Roma", "Inter"),
          type="out-of-sample",visualize = 2)

foot_rank(data = italy_2000_2002,
          object= fit6,
          team_sel = c("AS Roma", "Inter"),
          type="out-of-sample",visualize = 1)




# use two seasons and a portion of the current season
# to predict the currents season
fit7 <- stan_foot(data = italy_2000_2002,
                  model="double_pois",
                  dynamic_type ="seasonal",
                  predict = 250) # double poisson

foot_rank(data = italy_2000_2002,
          object= fit7,
          team_sel = c("AS Roma", "Inter"),
          type="out-of-sample",visualize = 2)

# quasi ok

foot_rank(data = italy_2000_2002,
          object= fit7,
          team_sel = c("AS Roma", "Inter"),
          type="out-of-sample",visualize = 1)

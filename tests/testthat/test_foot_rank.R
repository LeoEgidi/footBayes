
library(engsoccerdata)
library(tidyverse)

####
# use one season to predict part of the same season
###
italy <- as_tibble(italy)
  italy_2008<- italy %>%
    dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
    filter( Season=="2008")
  italy_1997<- italy %>%
    dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
    filter( Season=="1997")

  fit1 <- stan_foot(data = italy_2008,
                  model="double_pois", predict = 100)
  foot_rank(data = italy_2008, object= fit1,
          team_sel = c("AS Roma", "Inter", "Atalanta"),
          type="out-of-sample",visualize = 2)
    # ok

  foot_rank(data = italy_2008, object= fit1,
          team_sel = c("AS Roma", "Inter", "Atalanta"),
          type="out-of-sample",visualize = 1)
   # ok

  foot_rank(data = italy_2008, object= fit1,
            team_sel = c("AS Roma", "Inter", "Atalanta"),
            type="in-sample",visualize = 1)

  foot_rank(data = italy_2008, object= fit1,
            team_sel = c("AS Roma", "Inter", "Atalanta"),
            type="in-sample",visualize = 2)

  # decidere che fare con in-sample...

  fit2 <- stan_foot(data = italy_1997,
                    model="biv_pois", predict = 20)

  foot_rank(data = italy_1997, object= fit2,
            #type ="out-of-sample",
            visualize = 1)

  # problemi con predict =10, non prende tutte le squadre...

  foot_rank(data = italy_1997, object= fit2,
            #type ="out-of-sample",
            team_sel = c("AS Roma", "Inter"),
            visualize = 2)

  # errori con predict = 10...


###
# use two seasons to entirely predict the third season
###

  italy_2000_2002<- italy %>%
    dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
    filter(Season=="2000" |  Season=="2001"| Season=="2002")

  fit6 <- stan_foot(data = italy_2000_2002,
                  model="double_pois",
                  dynamic_type ="seasonal",
                  predict = 306) # double poisson

  foot_rank(data = italy_2000_2002,
          object= fit6,
          team_sel = c("AS Roma", "AC Milan"),
          type="out-of-sample",visualize = 2)

  # ok

  foot_rank(data = italy_2000_2002,
          object= fit6,
          team_sel = c("AS Roma", "Inter"),
          type="out-of-sample",visualize = 1)

  # ok



###
# use two seasons and a portion of the current season
# to predict the currents season
###
  fit7 <- stan_foot(data = italy_2000_2002,
                  model="double_pois",
                  dynamic_type ="seasonal",
                  predict = 250) # double poisson

  foot_rank(data = italy_2000_2002,
          object= fit7,
          team_sel = c("AS Roma", "Parma AC"),
          type="out-of-sample",visualize = 2)

  # ok

  foot_rank(data = italy_2000_2002,
          object= fit7,
          team_sel = c("AS Roma", "Inter", "AC Perugia",
                       "AC Milan", "Udinese Calcio"),
          type="out-of-sample",visualize = 2)

  # ok

foot_rank(data = italy_2000_2002,
          object= fit7,
          team_sel = c("AS Roma", "Inter"),
          type="out-of-sample",visualize = 1)

 # ok

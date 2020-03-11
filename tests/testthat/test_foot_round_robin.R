###################################################
## SCENARIO 1
# use one season to predict part of the same season
###################################################

italy <- as_tibble(italy)
italy_2008 <- italy %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter( Season=="2008")

  foot_round_robin(data = italy_2008,
                   object = fit1,
                   team_sel = c("Udinese Calcio",
                                "Torino FC",
                                "Juventus",
                                "AC Milan",
                                "Inter"))
  foot_round_robin(data = italy_2008,
                   object = fit2)
  foot_round_robin(data = italy_2008,
                   object = fit3)
  foot_round_robin(data = italy_2008,
                   object = fit4)


#################################
## SCENARIO 2
# no prediction
################################

  foot_round_robin(data = italy_2008,
                   object = fit5,
                   team_sel = c("Udinese Calcio",
                                "Torino FC",
                                "Juventus",
                                "AC Milan",
                                "Inter"))
  foot_round_robin(data = italy_2008,
                   object = fit6)
  foot_round_robin(data = italy_2008,
                   object = fit7)
  foot_round_robin(data = italy_2008,
                   object = fit8)


######################################################
## SCEANARIO 3
# use two seasons to entirely predict the third season
######################################################

italy_2000_2002<- italy %>%
    dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
    filter(Season=="2000" |  Season=="2001"|Season =="2002")

  foot_round_robin(data = italy_2000_2002,
                   object = fit9,
                   team_sel = c("Udinese Calcio",
                                "Torino FC",
                                "Juventus",
                                "AC Milan",
                                "Inter"))
  foot_round_robin(data = italy_2000_2002,
                   object = fit10)
  foot_round_robin(data = italy_2000_2002,
                   object = fit11)
  foot_round_robin(data = italy_2000_2002,
                   object = fit12)


####################################################
## SCENARIO 4
# use two seasons and a portion of the current season
# to predict the currents season
###################################################

  foot_round_robin(data = italy_2000_2002,
                   object = fit13,
                   team_sel = c("Udinese Calcio",
                                "Torino FC",
                                "Juventus",
                                "AC Milan",
                                "Inter"))
  foot_round_robin(data = italy_2000_2002,
                   object = fit14)
  foot_round_robin(data = italy_2000_2002,
                   object = fit15)
  foot_round_robin(data = italy_2000_2002,
                   object = fit16)


library(engsoccerdata)
library(tidyverse)



###################################################
## SCENARIO 1
# use one season to predict part of the same season
###################################################

italy <- as_tibble(italy)
  italy_2008<- italy %>%
    dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
    filter( Season=="2008")
  italy_1997<- italy %>%
    dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
    filter( Season=="1997")

  fit1 <- stan_foot(data = italy_2008,
                  model="double_pois",
                  predict = 100,
                  iter = 300)
  fit2 <- stan_foot(data = italy_2008,
                    model="biv_pois",
                    predict = 100,
                    iter = 300)
  fit3 <- stan_foot(data = italy_2008,
                    model="skellam",
                    predict = 100,
                    iter = 300)
  fit4 <- stan_foot(data = italy_2008,
                    model="student_t",
                    predict = 100,
                    iter = 300)
  fit4.bis <- stan_foot(data = italy_2008,
                    model="student_t",
                    predict = 4,
                    iter = 300)
  fit4.tris <- stan_foot(data = italy_2008,
                    model="student_t",
                    predict = 10,
                    iter = 300)
  fit4.four <- stan_foot(data = italy_2008,
                    model="student_t",
                    predict = 12,
                    iter = 300)
  fit4.five <- stan_foot(data = italy_2008,
                    model="student_t",
                    predict = 21,
                    iter = 300)


  foot_rank(data = italy_2008, object= fit1,
            team_sel = c("AS Roma", "Inter", "Atalanta"),
            visualize = 2)
  foot_rank(data = italy_2008, object= fit1,
            team_sel ="all",
            visualize = 2)
  foot_rank(data = italy_2008, object= fit1,
            visualize = 2)
  foot_rank(data = italy_2008, object= fit2,
            team_sel = c("AS Roma", "Inter","Atalanta"),
            visualize = 2)
  foot_rank(data = italy_2008, object= fit3,
            team_sel = c("AS Roma", "Inter"),
            visualize = 2)
  foot_rank(data = italy_2008, object= fit4,
            team_sel = c("AS Roma", "Inter", "Atalanta"),
            visualize = 2)
  foot_rank(data = italy_2008, object= fit4.bis,
            team_sel = c("AS Roma", "Inter", "Atalanta"),
            visualize = 2)
  #da correggere
  foot_rank(data = italy_2008, object= fit4.tris,
            team_sel = c("AS Roma", "Inter", "Atalanta"),
            visualize = 2)
  # ora va, da ricontrollare però
  foot_rank(data = italy_2008, object= fit4.four,
            team_sel = c("AS Roma", "Inter", "Atalanta"),
            visualize = 2)
  foot_rank(data = italy_2008, object= fit4.five,
            team_sel = c("AS Roma", "Inter", "Atalanta"),
            visualize = 2)


  foot_rank(data = italy_2008, object= fit1,
          team_sel = c("AS Roma", "Inter", "Atalanta"),
          visualize = 1)
  foot_rank(data = italy_2008, object= fit1,
            team_sel = "all",
            visualize = 1)
  foot_rank(data = italy_2008, object= fit1,
            visualize = 1)
  foot_rank(data = italy_2008, object= fit2,
            team_sel = c("AS Roma", "Inter", "Atalanta"),
            visualize = 1)
  foot_rank(data = italy_2008, object= fit3,
            team_sel = c("AS Roma", "Inter", "Atalanta"),
            visualize = 1)
  foot_rank(data = italy_2008, object= fit4,
            team_sel = c("AS Roma", "Inter", "Atalanta"),
            visualize = 1)
  foot_rank(data = italy_2008, object= fit4.bis,
            team_sel = c("AS Roma", "Inter", "Atalanta"),
            visualize = 1)
  # da correggere
  foot_rank(data = italy_2008, object= fit4.tris,
            team_sel = c("AS Roma", "Inter", "AC Milan"),
            visualize = 1)
  foot_rank(data = italy_2008, object= fit4.four,
            team_sel = c("AS Roma", "Inter", "AC Milan"),
            visualize = 1)
  foot_rank(data = italy_2008, object= fit4.five,
            team_sel = c("AS Roma", "Inter", "AC Milan"),
            visualize = 1)




#################################
## SCENARIO 2
# no prediction
################################


  fit5 <- stan_foot(data = italy_2008,
                    model="double_pois",
                    predict = 0,
                    iter = 300)
  fit6 <- stan_foot(data = italy_2008,
                    model="biv_pois",
                    predict = 0,
                    iter = 300)
  fit7 <- stan_foot(data = italy_2008,
                    model="skellam",
                    predict = 0,
                    iter = 300)
  fit8 <- stan_foot(data = italy_2008,
                    model="student_t",
                    predict = 0,
                    iter = 300)

  foot_rank(data = italy_2008, object= fit5,
            team_sel = c("AS Roma", "Inter", "Atalanta"),
            visualize = 2)
  foot_rank(data = italy_2008, object= fit5,
            team_sel = "all",
            visualize = 2)
  foot_rank(data = italy_2008, object= fit5,
            visualize = 2)
  foot_rank(data = italy_2008, object= fit6,
            team_sel = c("AS Roma", "Inter", "Atalanta"),
            visualize = 2)
  foot_rank(data = italy_2008, object= fit7,
            team_sel = c("AS Roma", "Inter", "Atalanta"),
            visualize = 2)
  foot_rank(data = italy_2008, object= fit8,
            team_sel = c("AS Roma", "Inter", "Atalanta"),
            visualize = 2)


  foot_rank(data = italy_2008, object= fit5,
            team_sel = c("AS Roma", "Inter", "Atalanta"),
            visualize = 1)
  foot_rank(data = italy_2008, object= fit5,
            visualize = 1)
  foot_rank(data = italy_2008, object= fit6,
            visualize = 1)
  foot_rank(data = italy_2008, object= fit7,
            visualize = 1)
  foot_rank(data = italy_2008, object= fit8,
            visualize = 1)



######################################################
## SCEANARIO 3
# use two seasons to entirely predict the third season
######################################################


italy_2000_2002<- italy %>%
    dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
    filter(Season=="2000" |  Season=="2001"| Season=="2002")

  fit9 <- stan_foot(data = italy_2000_2002,
                  model="double_pois",
                  predict = 306, iter = 300)
  fit10 <- stan_foot(data = italy_2000_2002,
                    model="biv_pois",
                    predict = 306, iter = 300)
  fit11 <- stan_foot(data = italy_2000_2002,
                    model="skellam",
                    predict = 306, iter = 300)
  fit12 <- stan_foot(data = italy_2000_2002,
                    model="student_t",
                    predict = 306, iter = 300)


  foot_rank(data = italy_2000_2002,
          object= fit9,
          team_sel = c("AS Roma", "AC Milan"),
          visualize = 2)
  foot_rank(data = italy_2000_2002,
            object= fit9,
            visualize = 2)
  foot_rank(data = italy_2000_2002,
            object= fit10,
            team_sel = c("AS Roma", "AC Milan"),
            visualize = 2)
  foot_rank(data = italy_2000_2002,
            object= fit11,
            team_sel = c("AS Roma", "AC Milan"),
            visualize = 2)
  foot_rank(data = italy_2000_2002,
            object= fit12,
            team_sel = c("AS Roma", "AC Milan"),
            visualize = 2)

  foot_rank(data = italy_2000_2002,
            object= fit9,
            team_sel = c("AS Roma", "AC Milan"),
            visualize = 1)
  foot_rank(data = italy_2000_2002,
            object= fit9,
            visualize = 1)
  foot_rank(data = italy_2000_2002,
            object= fit10,
            team_sel = c("AS Roma", "AC Milan"),
            visualize = 1)
  foot_rank(data = italy_2000_2002,
            object= fit11,
            team_sel = c("AS Roma", "AC Milan"),
            visualize = 1)
  foot_rank(data = italy_2000_2002,
            object= fit12,
            team_sel = c("AS Roma", "AC Milan"),
            visualize = 1)






####################################################
## SCENARIO 4
# use two seasons and a portion of the current season
# to predict the current season
###################################################



  fit13 <- stan_foot(data = italy_2000_2002,
                  model="double_pois",
                  dynamic_type ="seasonal",
                  predict = 250,
                  iter = 300)
  fit14 <- stan_foot(data = italy_2000_2002,
                     model="biv_pois",
                     dynamic_type ="seasonal",
                     predict = 250, iter = 300)
  fit15 <- stan_foot(data = italy_2000_2002,
                     model="skellam",
                     dynamic_type ="seasonal",
                     predict = 250, iter = 300)
  fit16 <- stan_foot(data = italy_2000_2002,
                     model="student_t",
                     dynamic_type ="seasonal",
                     predict = 250, iter = 300)
  fit16.bis <- stan_foot(data = italy_2000_2002,
                    model="student_t",
                    dynamic_type ="seasonal",
                    predict = 4, iter = 300)
  fit16.tris <- stan_foot(data = italy_2000_2002,
                    model="student_t",
                    dynamic_type ="seasonal",
                    predict = 10, iter = 300)

  foot_rank(data = italy_2000_2002,
            object= fit13,
            team_sel = c("AS Roma", "AC Milan"),
            visualize = 2)
  foot_rank(data = italy_2000_2002,
            object= fit13,
            visualize = 2)
  foot_rank(data = italy_2000_2002,
            object= fit14,
            team_sel = c("AS Roma", "AC Milan"),
            visualize = 2)
  foot_rank(data = italy_2000_2002,
            object= fit15,
            team_sel = c("AS Roma", "AC Milan"),
            visualize = 2)
  foot_rank(data = italy_2000_2002,
            object= fit16,
            team_sel = c("AS Roma", "AC Milan"),
            visualize = 2)
  foot_rank(data = italy_2000_2002,
            object= fit16.bis,
            team_sel = c("AS Roma", "AC Milan"),
            visualize = 2)
  # da correggere
  foot_rank(data = italy_2000_2002,
            object= fit16.tris,
            team_sel = c("AS Roma", "AC Milan"),
            visualize = 2)




  foot_rank(data = italy_2000_2002,
            object= fit13,
            team_sel = c("AS Roma", "AC Milan"),
            visualize = 1)
  foot_rank(data = italy_2000_2002,
            object= fit13,
            visualize = 1)
  foot_rank(data = italy_2000_2002,
            object= fit14,
            #team_sel = c("AS Roma", "AC Milan"),
            visualize = 1)
  foot_rank(data = italy_2000_2002,
            object= fit15,
            #team_sel = c("AS Roma", "AC Milan"),
            visualize = 1)
  foot_rank(data = italy_2000_2002,
            object= fit16,
            #team_sel = c("AS Roma", "AC Milan"),
            visualize = 1)
  foot_rank(data = italy_2000_2002,
            object= fit16.bis,
            #team_sel = c("AS Roma", "AC Milan"),
            visualize = 1)
  foot_rank(data = italy_2000_2002,
            object= fit16.tris,
            #team_sel = c("AS Roma", "AC Milan"),
            visualize = 1)



############################################
## SCENARIO 5
# use more seasons and a piece of a season
# to predict the remaining matches, also for
# other/another season/seasons
############################################


germany <- as_tibble(germany)
germany_1999_2001 <- germany %>%
    dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
    filter(  Season=="2001" | Season == "2000" | Season =="1999")

  fit17 <- stan_foot(data = germany_1999_2001,
                   model="double_pois",
                   dynamic_type ="seasonal",
                   predict = 320,
                   iter =300)

  fit18 <- stan_foot(data = germany_1999_2001,
                  model="biv_pois",
                  dynamic_type ="seasonal",
                  predict = 310,
                  iter =300)
  fit19 <- stan_foot(data = germany_1999_2001,
                  model="skellam",
                  dynamic_type ="seasonal",
                  predict = 310,
                  iter =300)
  fit20 <- stan_foot(data = germany_1999_2001,
                  model="student_t",
                  dynamic_type ="seasonal",
                  predict = 310,
                  iter =300)

  foot_rank(data = germany_1999_2001,
            object = fit17,
            visualize = 1)

  foot_rank(data = germany_1999_2001,
            object = fit18,
            visualize = 1)

  foot_rank(data = germany_1999_2001,
            object = fit19,
            visualize = 1)

  foot_rank(data = germany_1999_2001,
            object = fit20,
            visualize = 1)

  foot_rank(data = germany_1999_2001,
    object = fit17,
    visualize = 2)

  foot_rank(data = germany_1999_2001,
    object = fit18,
    visualize = 2)

  foot_rank(data = germany_1999_2001,
    object = fit19,
    visualize = 2)

  foot_rank(data = germany_1999_2001,
    object = fit20,
    visualize = 2)





library(tidyverse)
library(engsoccerdata)

##########################
## DATA
#########################

### dplyr, tidyverse

  ## I take six arguments
england <- as_tibble(england)
england_2001 <- england %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal,
                FT) %>%
  filter(  Season=="2001")


stan_foot(data = england_2001,
          model ="biv_pois")

  ## I take six arguments, but within the five
england <- as_tibble(england)
england_2001 <- england %>%
  dplyr::select(Season, home, FT, visitor, hgoal,vgoal) %>%
  filter(  Season=="2001")


stan_foot(data = england_2001,
          model ="biv_pois")

  ## I take twelve arguments, no rename the columns
england <- as_tibble(england)
england_2001 <- england %>%
  #dplyr::select(Season, home, FT, visitor, hgoal,vgoal) %>%
  filter(  Season=="2001")

stan_foot(data = england_2001,
          model ="biv_pois")

  ## I take twelve arguments, I rename the columns
england <- as_tibble(england)
england_2001 <- england %>%
  #dplyr::select(Season, home, FT, visitor, hgoal,vgoal) %>%
  filter(  Season=="2001")
colnames(england_2001) <- c("season", "home",
                            "away", "homegoals",
                            "awaygoals")

stan_foot(data = england_2001,
          model ="biv_pois")


  ## I take four arguments
england <- as_tibble(england)
england_2001 <- england %>%
  dplyr::select(Season, visitor, hgoal,vgoal) %>%
  filter(  Season=="2001")

stan_foot(data = england_2001,
          model ="biv_pois")

### csv file

  ## five arguments
bundes_2008 <- read.csv2(file="BundesLiga07-08.csv",
                         sep =",",dec=".")
is.data.frame(bundes_2008)
bundes_2008_ristr <- bundes_2008[, c('Date', 'HomeTeam',
                               'AwayTeam',
                               'FTHG', 'FTAG')]

stan_foot(data = bundes_2008_ristr,
          model = "biv_pois")

  ## more than five arguments
bundes_2008_ristr <- bundes_2008[, c('Date', 'HomeTeam',
                                     'AwayTeam',
                                     'FTHG', 'FTAG',
                                     'HTHG')]
stan_foot(data = bundes_2008_ristr,
          model = "biv_pois")

  ## four or less arguments
bundes_2008_ristr <- bundes_2008[, c('Date', 'HomeTeam',
                                     'HTHG')]
stan_foot(data = bundes_2008_ristr,
          model = "biv_pois")


######################
## MODELS
######################

england_2001 <- england %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter(  Season=="2001")

  ## wrong name
stan_foot(england_2001,
          model ="normal")

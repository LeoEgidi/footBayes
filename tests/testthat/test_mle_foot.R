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


mle_foot(data = england_2001,
          model ="biv_pois")

mle_foot(data = england_2001,
         model ="double_pois")

mle_foot(data = england_2001,
         model ="skellam")

mle_foot(data = england_2001,
         model ="student_t")


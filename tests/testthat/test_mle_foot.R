library(tidyverse)
library(engsoccerdata)

##########################
## DATA
#########################

### dplyr, tidyverse

italy <- as_tibble(italy)
italy_2008<- italy %>%
  dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
  filter( Season=="2008")


mle_foot(data = italy_2008,
          model ="biv_pois")

mle_foot(data = italy_2008,
         model ="double_pois")

mle_foot(data = italy_2008,
         model ="skellam")

mle_foot(data = italy_2008,
         model ="student_t")


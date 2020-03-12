##########################
## DATA
#########################

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

  ## I take four arguments
england <- as_tibble(england)
england_2001 <- england %>%
  dplyr::select(Season, visitor, hgoal,vgoal) %>%
  filter(  Season=="2001")

stan_foot(data = england_2001,
          model ="biv_pois")



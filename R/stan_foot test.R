library(tidyverse)
# checks
ristr_italy <- as_data_frame(italy)
ristr_italy<- ristr_italy %>%
  select(Season, home, visitor, hgoal,vgoal) %>%
  filter(Season=="2000" |  Season=="2001" | Season =="2003")

stan_foot(data = ristr_italy,
          model="double_pois" )
stan_foot(data = ristr_italy,
          model="double_pois",
          dynamic_type = FALSE)
stan_foot(data = ristr_italy,
          model="double_pois", predict =2 )
stan_foot(data = ristr_italy,
          model="double_pois", predict ="TRUE" )
stan_foot(data = ristr_italy,
          model="double_pois", predict =2,
          dynamic_type = FALSE)


# previsione sulle stagioni dinamica per i 4 modelli
ristr_italy <- as_data_frame(italy)
  ristr_italy<- ristr_italy %>%
    select(Season, home, visitor, hgoal,vgoal) %>%
    filter(Season=="2000" |  Season=="2001" | Season =="2003")

  # double pois
  stan_foot(data = ristr_italy,
          model="double_pois", predict =306,
          dynamic_type = "seasonal")
  # biv pois
  stan_foot(data = ristr_italy,
            model="biv_pois", predict =306,
            dynamic_type = "seasonal")
  # skellam
  stan_foot(data = ristr_italy,
            model="skellam", predict =306,
            dynamic_type = "seasonal")
  # student t
  stan_foot(data = ristr_italy,
            model="student_t", predict =306,
            dynamic_type = "seasonal")


# previsione sulle settimane dinamica per i 4 modelli
ristr_italy <- as_data_frame(italy)
ristr_italy<- ristr_italy %>%
  select(Season, home, visitor, hgoal,vgoal) %>%
  filter(Season=="2000")

  # double pois
  stan_foot(data = ristr_italy,
          model="double_pois", predict =36,
          dynamic_type = "weekly")
  # biv pois
  stan_foot(data = ristr_italy,
            model="biv_pois", predict =36,
            dynamic_type = "weekly")
  # skellam
  stan_foot(data = ristr_italy,
            model="skellam", predict =36,
            dynamic_type = "weekly")
  # student t
  stan_foot(data = ristr_italy,
            model="student_t", predict =36,
            dynamic_type = "weekly")







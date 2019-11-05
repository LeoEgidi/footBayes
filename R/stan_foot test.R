# fit
stan_foot(data = ristr_italy,
          model="double_pois", trend = FALSE )
stan_foot(data = ristr_italy,
          model="biv_pois", trend = FALSE )
stan_foot(data = ristr_italy,
          model="skellam", trend = FALSE )
stan_foot(data = ristr_italy,
          model="student_t", trend = FALSE )
stan_foot(data = ristr_italy,
          model="double_pois", trend = TRUE )
stan_foot(data = ristr_italy,
          model="biv_pois", trend = TRUE )
stan_foot(data = ristr_italy,
          model="skellam", trend = TRUE )
stan_foot(data = ristr_italy,
          model="student_t", trend = TRUE )

# prev
stan_foot(data = ristr_italy, predict =10,
          model="double_pois", trend = FALSE )
stan_foot(data = ristr_italy, predict =10,
          model="biv_pois", trend = FALSE )
stan_foot(data = ristr_italy, predict =10,
          model="skellam", trend = FALSE )
stan_foot(data = ristr_italy, predict =10,
          model="student_t", trend = FALSE )
stan_foot(data = ristr_italy, predict =10,
          model="double_pois", trend = TRUE )
stan_foot(data = ristr_italy, predict =10,
          model="biv_pois", trend = TRUE )
stan_foot(data = ristr_italy, predict =10,
          model="skellam", trend = TRUE )
stan_foot(data = ristr_italy, predict =10,
          model="student_t", trend = TRUE )

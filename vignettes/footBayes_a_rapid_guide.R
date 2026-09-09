params <-
list(EVAL = TRUE)

## ----setup,  include = FALSE--------------------------------------------------
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  purl = NOT_CRAN,
  eval = if (isTRUE(exists("params"))) params$EVAL else FALSE
)
knitr::opts_chunk$set(
  fig.align = "center",
  warning = FALSE,
  message = FALSE,
  fig.asp = 0.600,
  fig.height = 10,
  fig.width = 7,
  out.width = "700px",
  dpi = 96,
  global.par = TRUE,
  dev = "png",
  dev.args = list(pointsize = 10),
  fig.path = ""
)


## ----footBayes_inst_cran, echo = TRUE, eval = FALSE---------------------------
# install.packages("footBayes", type = "source")


## ----footBayes_inst, echo = TRUE, eval = FALSE--------------------------------
# # install.packages("devtools")
# devtools::install_github("LeoEgidi/footBayes")


## ----libraries, echo = TRUE, eval = TRUE--------------------------------------
library(footBayes)
library(dplyr)
library(ggplot2)
library(bayesplot)
library(loo)


## ----settings, echo = TRUE, eval = TRUE---------------------------------------
n_iter <- 1000
n_chains <- 4
seed <- 2026


## ----data_2000, echo = TRUE, eval = TRUE--------------------------------------
data("italy")
italy <- as.data.frame(italy)

italy_2000 <- italy %>%
  filter(Season == "2000") %>%
  arrange(Date) %>%
  select(periods = Season, home_team = home, away_team = visitor,
         home_goals = hgoal, away_goals = vgoal)

head(italy_2000)


## ----data_2018, echo = TRUE, eval = TRUE--------------------------------------
italy_2018_2021 <- italy %>%
  filter(Season %in% c("2018", "2019", "2020", "2021")) %>%
  arrange(Season, Date) %>%
  group_by(Season) %>%
  mutate(half = if_else(row_number() <= n() / 2, 1, 2)) %>%
  ungroup() %>%
  mutate(periods = 2 * (as.numeric(Season) - 2018) + half) %>%
  select(periods, home_team = home, away_team = visitor,
         home_goals = hgoal, away_goals = vgoal)

table(italy_2018_2021$periods)


## ----mle_fit, echo = TRUE, eval = TRUE----------------------------------------
mle_models <- c("double_pois", "biv_pois", "dixon_coles",
                "neg_bin", "skellam", "student_t")

mle_fits <- lapply(mle_models, function(m) {
  mle_foot(data = italy_2000, model = m, interval = "Wald")
})
names(mle_fits) <- mle_models

mle_table <- data.frame(
  model = mle_models,
  logLik = sapply(mle_fits, function(f) round(f$logLik, 2)),
  AIC = sapply(mle_fits, function(f) round(f$aic, 2)),
  BIC = sapply(mle_fits, function(f) round(f$bic, 2))
)
mle_table


## ----mle_pars, echo = TRUE, eval = TRUE---------------------------------------
mle_fits$biv_pois$home_effect
mle_fits$biv_pois$corr
mle_fits$dixon_coles$rho
mle_fits$neg_bin$overdispersion[, "mle"]


## ----static_fit, message = FALSE, results = 'hide', echo = TRUE, eval = TRUE----
fit_bp <- stan_foot(
  data = italy_2000,
  model = "biv_pois",
  chains = n_chains,
  parallel_chains = n_chains,
  iter_sampling = n_iter,
  seed = seed
)


## ----static_fit_print, message = FALSE, echo = TRUE, eval = TRUE--------------
print(fit_bp,
  pars = c("home", "rho", "sigma_att", "sigma_def", "att", "def"),
  teams = c("AS Roma", "Juventus", "AC Milan")
)


## ----static_fit_areas, echo = TRUE, eval = TRUE-------------------------------
posterior_bp <- fit_bp$fit$draws(format = "matrix")
mcmc_areas(posterior_bp, pars = c("home", "rho", "sigma_att", "sigma_def")) +
  theme_bw()


## ----static_fit_dc_nb, message = FALSE, results = 'hide', echo = TRUE, eval = TRUE----
fit_dc <- stan_foot(
  data = italy_2000,
  model = "dixon_coles",
  chains = n_chains,
  parallel_chains = n_chains,
  iter_sampling = n_iter,
  seed = seed
)

fit_nb <- stan_foot(
  data = italy_2000,
  model = "neg_bin",
  chains = n_chains,
  parallel_chains = n_chains,
  iter_sampling = n_iter,
  seed = seed
)


## ----static_fit_dc_nb_print, message = FALSE, echo = TRUE, eval = TRUE--------
print(fit_dc, pars = c("home", "rho"))
print(fit_nb, pars = c("home", "phi1", "phi2"))


## ----static_fit_priors, message = FALSE, results = 'hide', echo = TRUE, eval = TRUE----
fit_bp_t <- stan_foot(
  data = italy_2000,
  model = "biv_pois",
  prior_par = list(
    ability = student_t(4, 0, NULL),
    ability_sd = laplace(0, 1),
    home = normal(0, 10)
  ),
  chains = n_chains,
  parallel_chains = n_chains,
  iter_sampling = n_iter,
  seed = seed
)


## ----comparing_priors, echo = TRUE, eval = TRUE-------------------------------
posterior_bp_t <- fit_bp_t$fit$draws(format = "matrix")
sigma_att_post <- cbind(
  posterior_bp[, "sigma_att"],
  posterior_bp_t[, "sigma_att"]
)
colnames(sigma_att_post) <- c("Default", "Student-t + Laplace")

color_scheme_set("gray")
mcmc_areas(sigma_att_post) +
  ggtitle("Posterior of sigma_att under two prior specifications") +
  theme_bw()


## ----static_fit_pathfinder, message = FALSE, results = 'hide', echo = TRUE, eval = TRUE----
fit_bp_pf <- stan_foot(
  data = italy_2000,
  model = "biv_pois",
  method = "pathfinder",
  seed = seed
)


## ----static_fit_pathfinder_print, message = FALSE, echo = TRUE, eval = TRUE----
print(fit_bp_pf, pars = c("home", "rho", "sigma_att", "sigma_def"))


## ----weekly_fit, message = FALSE, results = 'hide', echo = TRUE, eval = TRUE----
fit_weekly <- stan_foot(
  data = italy_2000,
  model = "biv_pois",
  dynamic_type = "weekly",
  predict = 36,
  chains = n_chains,
  parallel_chains = n_chains,
  iter_sampling = n_iter,
  seed = seed
)


## ----weekly_fit_print, message = FALSE, echo = TRUE, eval = TRUE--------------
print(fit_weekly, pars = c("rho", "sigma_att", "sigma_def"))


## ----weekly_abilities, echo = TRUE, eval = TRUE-------------------------------
foot_abilities(fit_weekly, italy_2000,
  teams = c("AS Roma", "Juventus", "Lazio Roma", "AC Milan", "AS Bari", "SSC Napoli")
)


## ----seasonal_fits, message = FALSE, results = 'hide', echo = TRUE, eval = TRUE----
# separate evolution sds (Egidi et al., 2018)
fit_dyn <- stan_foot(
  data = italy_2018_2021,
  model = "double_pois",
  dynamic_type = "seasonal",
  predict = 190,
  chains = n_chains,
  parallel_chains = n_chains,
  iter_sampling = n_iter,
  seed = seed
)

# common evolution sd (Owen, 2011)
fit_dyn_owen <- stan_foot(
  data = italy_2018_2021,
  model = "double_pois",
  dynamic_type = "seasonal",
  dynamic_par = list(common_sd = TRUE),
  predict = 190,
  chains = n_chains,
  parallel_chains = n_chains,
  iter_sampling = n_iter,
  seed = seed
)

# variance inflation after the summer break (Koopman & Lit, 2015)
fit_dyn_kl <- stan_foot(
  data = italy_2018_2021,
  model = "double_pois",
  dynamic_type = "seasonal",
  dynamic_par = list(kl_variance = TRUE, periods_per_season = 2),
  predict = 190,
  chains = n_chains,
  parallel_chains = n_chains,
  iter_sampling = n_iter,
  seed = seed
)

# weighted dynamic model (Macrì Demartino et al., 2026)
fit_dyn_wdm <- stan_foot(
  data = italy_2018_2021,
  model = "double_pois",
  dynamic_type = "seasonal",
  dynamic_weight = TRUE,
  dynamic_par = list(spike = normal(9, 1.5), slab = normal(0, 3)),
  predict = 190,
  chains = n_chains,
  parallel_chains = n_chains,
  iter_sampling = n_iter,
  seed = seed
)


## ----seasonal_fits_print, message = FALSE, echo = TRUE, eval = TRUE-----------
print(fit_dyn, pars = c("sigma_att", "sigma_def"))
print(fit_dyn_owen, pars = "sigma_common")
fit_dyn_kl$stan_data$is_summer_break
print(fit_dyn_kl, pars = c("sigma_att_kl", "sigma_def_kl", "sigma_break"))


## ----seasonal_wdm_print, message = FALSE, echo = TRUE, eval = TRUE------------
print(fit_dyn_wdm,
  pars = "prob_spike",
  teams = c("Juventus", "AC Milan", "SSC Napoli", "Atalanta", "AS Roma")
)


## ----seasonal_abilities, echo = TRUE, eval = TRUE, fig.show = "hold"----------
foot_abilities(fit_dyn, italy_2018_2021,
  teams = c("Juventus", "Inter", "AC Milan", "SSC Napoli")
)


## ----btd_data, echo = TRUE, eval = TRUE---------------------------------------
italy_2018_2021_btd <- italy_2018_2021 %>%
  filter(periods <= 7) %>%
  mutate(match_outcome = case_when(
    home_goals > away_goals ~ 1,
    home_goals == away_goals ~ 2,
    home_goals < away_goals ~ 3
  )) %>%
  select(periods, home_team, away_team, match_outcome)


## ----btd_fit, message = FALSE, results = 'hide', echo = TRUE, eval = TRUE-----
fit_btd_dyn <- btd_foot(
  data = italy_2018_2021_btd,
  dynamic_rank = TRUE,
  home_effect = TRUE,
  rank_measure = "median",
  chains = n_chains,
  parallel_chains = n_chains,
  iter_sampling = n_iter,
  adapt_delta = 0.9,
  max_treedepth = 12,
  seed = seed
)

fit_btd_stat <- btd_foot(
  data = italy_2018_2021_btd,
  dynamic_rank = FALSE,
  home_effect = TRUE,
  rank_measure = "map",
  chains = n_chains,
  parallel_chains = n_chains,
  iter_sampling = n_iter,
  seed = seed
)


## ----btd_print, message = FALSE, echo = TRUE, eval = TRUE---------------------
print(fit_btd_dyn,
  display = "parameters",
  pars = c("logStrength", "logTie", "home"),
  teams = c("Juventus", "Inter")
)
print(fit_btd_stat, display = "rankings")


## ----plot_btdPosterior_dyn, echo = TRUE, eval = TRUE--------------------------
plot_btdPosterior(fit_btd_dyn,
  teams = c("Juventus", "Inter", "AC Milan", "SSC Napoli"),
  ncol = 2
)


## ----plot_btdPosterior_stat_dens, echo = TRUE, eval = TRUE--------------------
plot_btdPosterior(fit_btd_stat,
  teams = c("Juventus", "Inter", "AC Milan", "SSC Napoli"),
  plot_type = "density",
  scales = "free_y"
)


## ----plot_logStrength, echo = TRUE, eval = TRUE-------------------------------
plot_logStrength(fit_btd_dyn,
  teams = c("Juventus", "Inter", "AC Milan", "SSC Napoli")
)


## ----seasonal_fit_rank, message = FALSE, results = 'hide', echo = TRUE, eval = TRUE----
fit_dyn_rank <- stan_foot(
  data = italy_2018_2021,
  model = "double_pois",
  ranking = fit_btd_dyn,
  dynamic_type = "seasonal",
  predict = 190,
  chains = n_chains,
  parallel_chains = n_chains,
  iter_sampling = n_iter,
  seed = seed
)


## ----seasonal_fit_rank_print, message = FALSE, echo = TRUE, eval = TRUE-------
print(fit_dyn_rank, pars = c("gamma", "sigma_att", "sigma_def"))


## ----pp_foot, echo = TRUE, eval = TRUE----------------------------------------
pp_foot(object = fit_bp, data = italy_2000, type = "aggregated")
pp_foot(object = fit_bp, data = italy_2000, type = "matches")


## ----pp_checks, echo = TRUE, eval = TRUE--------------------------------------
draws_bp <- posterior::as_draws_rvars(fit_bp$fit$draws())
y_rep <- posterior::draws_of(draws_bp[["y_rep"]])
goal_diff <- italy_2000$home_goals - italy_2000$away_goals

ppc_dens_overlay(goal_diff, y_rep[, , 1] - y_rep[, , 2], bw = 0.5) +
  theme_bw()


## ----foot_prob, echo = TRUE, eval = TRUE--------------------------------------
foot_prob(
  object = fit_weekly, data = italy_2000,
  home_team = "Reggina Calcio", away_team = "AC Milan"
)


## ----foot_round_robin, echo = TRUE, eval = TRUE-------------------------------
foot_round_robin(object = fit_weekly, data = italy_2000)


## ----rank_insample, echo = TRUE, eval = TRUE----------------------------------
foot_rank(object = fit_bp, data = italy_2000, visualize = "aggregated")


## ----rank_outsample, echo = TRUE, eval = TRUE---------------------------------
foot_rank(object = fit_weekly, data = italy_2000, visualize = "aggregated")
foot_rank(
  object = fit_weekly, data = italy_2000,
  teams = c("AS Roma", "Juventus", "Lazio Roma", "AC Milan"),
  visualize = "individual"
)


## ----compare_foot, message = FALSE, echo = TRUE, eval = TRUE------------------
italy_2021_test <- italy_2018_2021 %>%
  filter(periods == 8)

compare_results <- compare_foot(
  source = list(
    egidi = fit_dyn,
    owen = fit_dyn_owen
  ),
  test_data = italy_2021_test,
  metric = c("accuracy", "brier", "RPS", "pseudoR2", "ACP"),
  conf_matrix = FALSE
)

print(compare_results, digits = 3)


## ----loo, echo = TRUE, eval = TRUE--------------------------------------------
loo_list <- list(
  biv_pois = fit_bp$fit$loo(),
  biv_pois_t_priors = fit_bp_t$fit$loo(),
  dixon_coles = fit_dc$fit$loo(),
  neg_bin = fit_nb$fit$loo()
)

loo_compare(loo_list)


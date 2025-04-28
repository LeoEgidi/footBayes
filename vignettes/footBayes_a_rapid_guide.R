params <-
list(EVAL = FALSE)

## ----setup,  include = FALSE-------------------------------------------------------
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
  fig.asp = 0.625,
  fig.height = 10,       # Reduced height
  fig.width = 8,        # Reduced width
  out.width = "750px",  # Reduced output width
  dpi = 100,
  global.par = TRUE,
  dev = "png",
  dev.args = list(pointsize = 10),
  fig.path = ""  # Added this line to the standard setup chunk
  )


## ----footBayes_inst_cran, echo =TRUE, eval = FALSE---------------------------------
# install.packages("footBayes", type = "source")


## ----footBayes_inst, echo =TRUE, eval = FALSE--------------------------------------
# library(devtools)
# install_github("LeoEgidi/footBayes")


## ----libraries, echo = TRUE, eval = TRUE-------------------------------------------
library(footBayes)
library(loo)
library(ggplot2)
library(ggridges)
library(dplyr)
library(bayesplot)


## ----BTD_data, echo = TRUE, eval = TRUE--------------------------------------------
library(dplyr)
library(footBayes)

data("italy")

italy_2020_2021 <- italy %>%
  dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
  dplyr::filter(Season == "2020" | Season == "2021") %>%
  dplyr::mutate(match_outcome = dplyr::case_when(
    hgoal > vgoal ~ 1, # Home team wins
    hgoal == vgoal ~ 2, # Draw
    hgoal < vgoal ~ 3 # Away team wins
  )) %>%
  dplyr::mutate(periods = dplyr::case_when(
    dplyr::row_number() <= 190 ~ 1,
    dplyr::row_number() <= 380 ~ 2,
    dplyr::row_number() <= 570 ~ 3,
    TRUE ~ 4
  )) %>% # Assign periods based on match number
  dplyr::select(periods,
    home_team = home,
    away_team = visitor, match_outcome
  )


## ----BTD_model_1_dyn, message = FALSE, results='hide', echo = TRUE, eval = TRUE----
# Dynamic Ranking Example with Median Rank Measure
fit_result_dyn <- btd_foot(
  data = italy_2020_2021,
  dynamic_rank = TRUE,
  rank_measure = "median",
  iter_sampling = 1000,
  # parallel_chains = 2,
  chains = 2,
  adapt_delta = 0.9,
  max_treedepth = 12
)


## ----BTD_model_1_dyn_print, message = FALSE, echo = TRUE, eval = TRUE--------------
print(fit_result_dyn,
  display = "parameters",
  pars = c("logStrength", "logTie"),
  teams = c("AC Milan", "AS Roma")
)


## ----BTD_model_1_stat, message = FALSE, results='hide', echo = TRUE, eval = TRUE----
# Static Ranking Example with MAP Rank Measure
fit_result_stat <- btd_foot(
  data = italy_2020_2021,
  dynamic_rank = FALSE,
  rank_measure = "map",
  iter_sampling = 1000,
  # parallel_chains = 2,
  chains = 2
)


## ----BTD_model_1_stat_print, message = FALSE, echo = TRUE, eval = TRUE-------------
print(fit_result_stat,
  pars = c("logStrength", "logTie"),
  teams = c("AC Milan", "AS Roma")
)


## ----BTD_model_2, message = FALSE, results='hide', echo = TRUE, eval = TRUE--------
# Dynamic Ranking Example with Median Rank Measure
fit_result_dyn_2 <- btd_foot(
  data = italy_2020_2021,
  home_effect = TRUE,
  dynamic_rank = TRUE,
  prior_par = list(
    logStrength = normal(2, 10),
    logTie = normal(-1.5, 5),
    home = normal(0, 5)
  ),
  rank_measure = "median",
  iter_sampling = 1000,
  # parallel_chains = 2,
  chains = 2,
  adapt_delta = 0.9,
  max_treedepth = 12
)


## ----BTD_model_2_dyn_print, message = FALSE, echo = TRUE, eval = TRUE--------------
print(fit_result_dyn_2,
  display = "parameters",
  pars = c("logTie", "home")
)


## ----BTD_model_2_stat, message = FALSE, results='hide', echo = TRUE, eval = TRUE----
# Static Ranking Example with MAP Rank Measure
fit_result_stat_2 <- btd_foot(
  data = italy_2020_2021,
  home_effect = TRUE,
  dynamic_rank = FALSE,
  prior_par = list(
    logStrength = normal(2, 10),
    logTie = normal(0, 2.5),
    home = normal(5, 3)
  ),
  rank_measure = "map",
  iter_sampling = 1000,
  # parallel_chains = 2,
  chains = 2
)


## ----BTD_model_2_stat_print, message = FALSE, echo = TRUE, eval = TRUE-------------
print(fit_result_stat_2,
  pars = c("logTie", "home")
)


## ----BTD_model_vi, message = FALSE, results='hide', echo = TRUE, eval = TRUE-------
# Variational Inference Example
fit_result_vi <- btd_foot(
  data = italy_2020_2021,
  dynamic_rank = TRUE,
  rank_measure = "mean",
  method = "VI"
)



## ----plot_btdPosterior_dyn, echo = TRUE, eval = TRUE, fig.show="hold"--------------
# Dynamic Ranking

plot_btdPosterior(fit_result_dyn)


## ----plot_btdPosterior_stat, echo = TRUE, eval = TRUE, fig.show="hold"-------------
# Static Ranking

plot_btdPosterior(fit_result_stat)


## ----plot_btdPosterior_teams_dyn, echo = TRUE, eval = TRUE, fig.show="hold"--------
# Dynamic Ranking

plot_btdPosterior(fit_result_dyn,
  teams = c("AC Milan", "AS Roma", "Juventus", "Inter"),
  ncol = 2
)


## ----plot_btdPosterior_teams_stat, echo = TRUE, eval = TRUE, fig.show="hold"-------
# Static Ranking

plot_btdPosterior(fit_result_stat,
  teams = c("AC Milan", "AS Roma", "Juventus", "Inter"),
  ncol = 2
)


## ----plot_btdPosterior_teams_dyn_dens, echo = TRUE, eval = TRUE, fig.show="hold"----
# Dynamic Ranking

plot_btdPosterior(fit_result_dyn,
  teams = c("AC Milan", "AS Roma", "Juventus", "Inter"),
  plot_type = "density",
  scales = "free_y"
)


## ----plot_btdPosterior_teams_stat_dens, echo = TRUE, eval = TRUE, fig.show="hold"----
# Static Ranking

plot_btdPosterior(fit_result_stat,
  teams = c("AC Milan", "AS Roma", "Juventus", "Inter"),
  plot_type = "density",
  scales = "free_y"
)


## ----plot_logStrength_teams_dyn, echo = TRUE, eval = TRUE, fig.show="hold"---------
# Dynamic Ranking

plot_logStrength(fit_result_dyn,
  teams = c("AC Milan", "AS Roma", "Juventus", "Inter")
)


## ----static_fit, message = FALSE, results='hide', echo = TRUE, eval = TRUE---------
### Use Italian Serie A 2000/2001

## with 'dplyr' environment
#
# library(dplyr)
# italy <- as_tibble(italy)
# italy_2000<- italy %>%
#  dplyr::select(Season, home, visitor, hgoal,vgoal) #%>%
#  dplyr::filter(Season=="2000")
# italy_2000

## alternatively, you can use the basic 'subsetting' code,
## not using the 'dplyr' environment:
data("italy")
italy <- as.data.frame(italy)
italy_2000 <- subset(
  italy[, c(2, 3, 4, 6, 7)],
  Season == "2000"
)

colnames(italy_2000) <- c("periods", "home_team", "away_team", "home_goals", "away_goals")


### Fit Stan models
## no dynamics, no predictions
## 4 Markov chains, 'n_iter' iterations each

n_iter <- 200 # number of MCMC iterations after the burn-in
fit1_stan <- stan_foot(
  data = italy_2000,
  model = "biv_pois",
  chains = 4,
  # parallel_chains = 4,
  iter_sampling = n_iter
) # biv poisson


## ----static_fit_print, message = FALSE, echo = TRUE, eval = TRUE-------------------
## Print of model summary for parameters:

print(fit1_stan,
  pars = c(
    "home", "rho", "sigma_att",
    "sigma_def", "att", "def"
  ),
  teams = c("AC Milan", "AS Roma")
)


## ----static_fit_corr, echo =TRUE, eval = TRUE--------------------------------------
## Marginal posterior with bayesplot

posterior1 <- fit1_stan$fit$draws(format = "matrix")
mcmc_areas(posterior1, pars = c(
  "home", "rho",
  "sigma_att", "sigma_def"
)) +
  theme_bw()


## ----stan_extract, echo = TRUE, eval = TRUE----------------------------------------
### Model's code extraction

fit1_stan$stan_code


## ----stan_foot_model_pth, message = FALSE, results='hide', echo = TRUE, eval = TRUE----
# Pathfinder algorithm example
fit1_stan_path <- stan_foot(
  data = italy_2000,
  model = "biv_pois",
  method = "pathfinder"
) # biv poisson



## ----static_fit2, echo = TRUE, eval = TRUE-----------------------------------------
### Fit MLE models
## no dynamics, no predictions
## Wald intervals

fit1_mle <- mle_foot(
  data = italy_2000,
  model = "biv_pois",
  interval = "Wald"
) # mle biv poisson
fit1_mle$home_effect


## ----static_fit_priors, message = FALSE, results='hide', echo =TRUE, eval = TRUE----
### Fit Stan models
## changing priors
## student-t for team-specific abilities, laplace for sds

fit1_stan_t <- stan_foot(
  data = italy_2000,
  model = "biv_pois",
  chains = 4,
  prior_par = list(
    ability = student_t(4, 0, NULL),
    ability_sd = laplace(0, 1),
    home = normal(0, 10)
  ),
  # parallel_chains = 4,
  iter_sampling = n_iter
) # biv poisson


## ----comparing_priors, eval = TRUE-------------------------------------------------
## comparing posteriors

posterior1_t <- fit1_stan_t$fit$draws(format = "matrix")
model_names <- c("Default", "Stud+Laplace")
color_scheme_set(scheme = "gray")
gl_posterior <- cbind(
  posterior1[, "sigma_att"],
  posterior1_t[, "sigma_att"]
)
colnames(gl_posterior) <- c("sigma_att", "sigma_att_t")
mcmc_areas(gl_posterior, pars = c("sigma_att", "sigma_att_t")) +
  xaxis_text(on = TRUE, size = ggplot2::rel(2.9)) +
  yaxis_text(on = TRUE, size = ggplot2::rel(2.9)) +
  scale_y_discrete(labels = ((parse(text = model_names)))) +
  ggtitle("Att/def sds") +
  theme(plot.title = element_text(hjust = 0.5, size = rel(2.6))) +
  theme_bw()


## ----dynamic_fit,message = FALSE, results='hide', echo =TRUE, eval = TRUE----------
### Fit Stan models
## seasonal dynamics, no predictions
## 2 Markov chains, 'n_iter' iterations each

fit2_stan <- stan_foot(
  data = italy_2000,
  model = "biv_pois",
  dynamic_type = "weekly",
  # parallel_chains = 2,
  chains = 2,
  iter_sampling = n_iter
) # biv poisson


## ----dynamic_fit_print, message = FALSE, echo = TRUE, eval = TRUE------------------
print(fit2_stan, pars = c(
  "home", "rho", "sigma_att",
  "sigma_def"
))


## ----weekly_fit, message = FALSE, results='hide', echo = TRUE, eval = TRUE---------
### Fit Stan models
## weekly dynamics, no predictions
## 2 chains, 'n_iter' iterations each

fit3_stan <- stan_foot(
  data = italy_2000,
  model = "double_pois",
  dynamic_type = "weekly",
  # parallel_chains = 2,
  chains = 2,
  iter_sampling = n_iter
) # double poisson


## ----weekly_fit_print, message = FALSE, echo = TRUE, eval = TRUE-------------------
print(fit3_stan, pars = c(
  "home", "sigma_att",
  "sigma_def"
))


## ----weekly_fit_t, message = FALSE, results='hide', echo = TRUE, eval = TRUE-------
### Fit Stan models
## weekly dynamics, no predictions
## 2 chains, 'n_iter' iterations each

fit3_stan_t <- stan_foot(
  data = italy_2000,
  model = "double_pois",
  prior_par = list(
    ability = student_t(4, 0, NULL),
    ability_sd = cauchy(0, 25),
    home = normal(0, 5)
  ),
  dynamic_type = "weekly",
  # parallel_chains = 2,
  chains = 2,
  iter_sampling = n_iter
) # double poisson


## ----weekly_fit_t_print, message = FALSE, echo = TRUE, eval = TRUE-----------------
print(fit3_stan_t, pars = c(
  "home", "sigma_att",
  "sigma_def"
))


## ----btd_foot_and_stan_foot, message = FALSE, results='hide', echo = TRUE, eval = TRUE----
# Dynamic Bradley-Terry-Davidson model

data("italy")

italy_2020_2021_rank <- italy %>%
  dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
  dplyr::filter(Season == "2020" | Season == "2021") %>%
  dplyr::mutate(match_outcome = dplyr::case_when(
    hgoal > vgoal ~ 1, # Home team wins
    hgoal == vgoal ~ 2, # Draw
    hgoal < vgoal ~ 3 # Away team wins
  )) %>%
  dplyr::filter(dplyr::row_number() <= 570) %>%
  dplyr::mutate(periods = dplyr::case_when(
    dplyr::row_number() <= 190 ~ 1,
    dplyr::row_number() <= 380 ~ 2,
    dplyr::row_number() <= 570 ~ 3
  )) %>%
  dplyr::select(periods,
    home_team = home,
    away_team = visitor, match_outcome
  )



fit_btd_dyn <- btd_foot(
  data = italy_2020_2021_rank,
  dynamic_rank = TRUE,
  rank_measure = "median",
  iter_sampling = 1000,
  # parallel_chains = 2,
  chains = 2,
  adapt_delta = 0.9,
  max_treedepth = 12
)

# Dynamic Bivariate Poisson Model

italy_2020_2021_fit <- italy %>%
  dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
  dplyr::filter(Season == "2020" | Season == "2021") %>%
  dplyr::filter(dplyr::row_number() <= 570) %>%
  dplyr::mutate(periods = dplyr::case_when(
    dplyr::row_number() <= 190 ~ 1,
    dplyr::row_number() <= 380 ~ 2,
    dplyr::row_number() <= 570 ~ 3
  )) %>% # Assign periods based on match number
  dplyr::select(periods,
    home_team = home,
    away_team = visitor, home_goals = hgoal, away_goals = vgoal
  )



fit_stan_rank <- stan_foot(
  data = italy_2020_2021_fit,
  model = "biv_pois",
  ranking = fit_btd_dyn,
  predict = 180,
  prior_par = list(
    ability = student_t(4, 0, NULL),
    ability_sd = cauchy(0, 25),
    home = normal(0, 5)
  ),
  dynamic_type = "season",
  chains = 2,
  # parallel_chains = 2,
  iter_sampling = 1000
)


## ----btd_foot_and_stan_foot_print, message = FALSE, echo = TRUE, eval = TRUE-------
print(fit_stan_rank,
  pars = c("home", "rho", "sigma_att", "sigma_def")
)


## ----abilities, echo = TRUE, eval = TRUE, fig.show="hold"--------------------------
## Plotting abilities: credible and confidence 95% intervals

foot_abilities(object = fit1_stan, data = italy_2000)
foot_abilities(object = fit1_mle, data = italy_2000)


## ----abilities_dyn, echo = TRUE, eval = TRUE, fig.show="hold"----------------------
## Plotting abilities: credible and confidence 95% intervals

foot_abilities(fit2_stan, italy_2000)


## ----pp_foot, echo = TRUE, eval = TRUE---------------------------------------------
## PP checks: aggregated goal's differences and ordered goal differences

pp_foot(
  object = fit1_stan, data = italy_2000,
  type = "aggregated"
)

pp_foot(
  object = fit1_stan, data = italy_2000,
  type = "matches"
)


## ----pp_checks, echo = TRUE, eval = TRUE-------------------------------------------
## PPC densities overlay with the bayesplot package

# extracting the replications
draws_raw <- fit1_stan$fit$draws()
draws <- posterior::as_draws_rvars(draws_raw)
sims <- list()
sims$y_rep <- posterior::draws_of(draws[["y_rep"]])

goal_diff <- italy_2000$home_goals - italy_2000$away_goals

# plotting data density vs replications densities

ppc_dens_overlay(goal_diff, sims$y_rep[, , 1] - sims$y_rep[, , 2], bw = 0.5) +
  theme_bw()


## ----weekly_predict,message = FALSE, results='hide', echo=TRUE, eval = TRUE--------
### Fit Stan models
## weekly dynamics, predictions of last four weeks
## 2 chains 'n_iter' iterations each

fit4_stan <- stan_foot(
  data = italy_2000,
  model = "biv_pois",
  predict = 36,
  dynamic_type = "weekly",
  # parallel_chains = 2,
  chains = 2,
  iter_sampling = n_iter
) # biv poisson


## ----foot_prob_weekly_predict, echo = TRUE, eval = TRUE, fig.show="hold"-----------
foot_prob(
  object = fit4_stan, data = italy_2000,
  home_team = "Reggina Calcio",
  away_team = "AC Milan"
)


## ----foot_roundrobin, echo=TRUE, eval = TRUE---------------------------------------
## Home win out-of-sample probabilities

foot_round_robin(object = fit4_stan, data = italy_2000)


## ----rank_pred1, echo=TRUE, eval = TRUE--------------------------------------------
## Rank league reconstruction

# aggregated plot

foot_rank(object = fit1_stan, data = italy_2000)

# team-specific plot

foot_rank(
  object = fit1_stan, data = italy_2000,
  visualize = "individual"
)


## ----rank_pred2, echo=TRUE, eval = TRUE--------------------------------------------
## Rank predictions for individual teams

# aggregated plot

foot_rank(object = fit4_stan, data = italy_2000)

# team-specific plot

foot_rank(
  object = fit4_stan, data = italy_2000,
  teams = c("AC Milan", "AS Roma"),
  visualize = "individual"
)

foot_rank(
  object = fit4_stan, data = italy_2000,
  visualize = "individual"
)


## ----compare_foot, message = FALSE, results='hide', echo = TRUE, eval = TRUE-------
italy_2020_2021_fit <- italy %>%
  dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
  dplyr::filter(Season == "2020" | Season == "2021") %>%
  dplyr::mutate(periods = dplyr::case_when(
    dplyr::row_number() <= 190 ~ 1,
    dplyr::row_number() <= 380 ~ 2,
    dplyr::row_number() <= 570 ~ 3,
    TRUE ~ 4
  )) %>% # Assign periods based on match number
  dplyr::select(periods,
    home_team = home,
    away_team = visitor, home_goals = hgoal, away_goals = vgoal
  )


fit_comp_1 <- stan_foot(
  data = italy_2020_2021_fit,
  model = "biv_pois",
  home_effect = TRUE,
  predict = 190,
  dynamic_type = "season",
  # parallel_chains = 4,
  iter_sampling = n_iter
)

fit_comp_2 <- stan_foot(
  data = italy_2020_2021_fit,
  model = "double_pois",
  home_effect = TRUE,
  predict = 190,
  dynamic_type = "season",
  # parallel_chains = 4,
  iter_sampling = n_iter
)


italy_2020_2021_test <- italy %>%
  dplyr::select(Season, home, visitor, hgoal, vgoal) %>%
  dplyr::filter(Season == "2014" | Season == "2015") %>%
  dplyr::mutate(periods = dplyr::case_when(
    dplyr::row_number() <= 190 ~ 1,
    dplyr::row_number() <= 380 ~ 2,
    dplyr::row_number() <= 570 ~ 3,
    TRUE ~ 4
  )) %>%
  dplyr::filter(dplyr::row_number() > 570) %>%
  dplyr::select(periods,
    home_team = home,
    away_team = visitor,
    home_goals = hgoal,
    away_goals = vgoal
  )


## ----compare_foot_print, message = FALSE, echo = TRUE, eval = TRUE-----------------
compare_results_models <- compare_foot(
  source = list(
    biv_pois = fit_comp_1,
    double_pois = fit_comp_2
  ),
  test_data = italy_2020_2021_test,
  metric = c("accuracy", "brier", "ACP", "pseudoR2", "RPS"),
  conf_matrix = TRUE
)


print(compare_results_models, digits = 3)


## ----loo, echo = TRUE, eval = TRUE-------------------------------------------------
### Model comparisons
## LOOIC, loo function

# compute loo

loo1 <- fit1_stan$fit$loo()
loo1_t <- fit1_stan_t$fit$loo()
loo2 <- fit2_stan$fit$loo()
loo3 <- fit3_stan$fit$loo()
loo3_t <- fit3_stan_t$fit$loo()


# compare three looic

loo_compare(loo1, loo1_t, loo2, loo3, loo3_t)


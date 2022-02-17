## ----setup, cache = FALSE, include = FALSE------------------------------------
knitr::opts_chunk$set(
  eval = FALSE,
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_chunk$set(fig.align = 'center', 
                      warning=FALSE, message=FALSE, fig.asp=0.625, fig.height =10, fig.width = 8, 
                      out.width='750px', dpi=200, 
                      global.par = TRUE,
                      dev='png',  
                      dev.args=list(pointsize=10)
                      ,fig.path = 'figs/'
                      )

## ----footBayes_inst, echo =TRUE, eval = FALSE---------------------------------
#  library(devtools)
#  install_github("LeoEgidi/footBayes")

## ----libraries, echo = TRUE, eval = TRUE--------------------------------------
library(footBayes)
library(engsoccerdata)
library(bayesplot)
library(loo)
library(ggplot2)
library(dplyr)
library(tidyverse)

## ----static_fit, echo =TRUE, eval = TRUE--------------------------------------

### Use Italian Serie A 2000/2001

## with 'tidyverse' environment
#
#library(tidyverse)
#italy <- as_tibble(italy)
#italy_2000<- italy %>%
#  dplyr::select(Season, home, visitor, hgoal,vgoal) #%>%
#  dplyr::filter(Season=="2000")
#italy_2000

## alternatively, you can use the basic 'subsetting' code, 
## not using the 'tidyverse' environment: 
data(italy)
italy <- as.data.frame(italy)
italy_2000 <- subset(italy[, c(2,3,4,6,7)], 
                     Season =="2000")
head(italy_2000)

### Fit Stan models
## no dynamics, no predictions
## 4 Markov chains, 'n_iter' iterations each

n_iter <- 200    # number of MCMC iterations
fit1_stan <- stan_foot(data = italy_2000,
                       model="biv_pois",
                       chains = 4,
                       #cores = 4,
                       iter = n_iter) # biv poisson

## print of model summary for parameters:
## home, sigma_att, sigma_def

print(fit1_stan, pars =c("home", "rho", "sigma_att",
                         "sigma_def"))

## ----static_fit_corr, echo =TRUE, eval = TRUE---------------------------------
## Marginal posterior with bayesplot

posterior1 <- as.matrix(fit1_stan)
mcmc_areas(posterior1, regex_pars=c("home", "rho",
  "sigma_att", "sigma_def"))

## ----stan_extract, echo =TRUE, eval = TRUE------------------------------------
### Model's code extraction

fit1_stan@stanmodel

## ----static_fit2, echo = TRUE, eval = TRUE------------------------------------
### Fit MLE models
## no dynamics, no predictions
## Wald intervals

fit1_mle <- mle_foot(data = italy_2000,
                     model="biv_pois",
                     interval = "Wald") # mle biv poisson
fit1_mle$home

## ----static_fit_priors, echo =TRUE, eval = TRUE-------------------------------
### Fit Stan models
## changing priors
## student-t for team-specific abilities, laplace for sds

fit1_stan_t <- stan_foot(data = italy_2000,
                         model="biv_pois",
                         chains = 4, 
                         prior = student_t(4,0,NULL),
                         prior_sd = laplace(0,1),
                         #cores = 4,
                         iter = n_iter) # biv poisson


## ----comparing_priors---------------------------------------------------------
#  ## comparing posteriors
#  
#  posterior1_t <- as.matrix(fit1_stan_t)
#  model_names <- c("Default", "Stud+Laplace")
#  color_scheme_set(scheme = "gray")
#  gl_posterior <- cbind(posterior1[,"sigma_att"],
#                        posterior1_t[,"sigma_att"])
#  colnames(gl_posterior)<-c("sigma_att", "sigma_att_t")
#  mcmc_areas(gl_posterior, pars=c("sigma_att", "sigma_att_t"))+
#    xaxis_text(on =TRUE, size=ggplot2::rel(2.9))+
#    yaxis_text(on =TRUE, size=ggplot2::rel(2.9))+
#    scale_y_discrete(labels = ((parse(text= model_names))))+
#    ggtitle("Att/def sds")+
#    theme(plot.title = element_text(hjust = 0.5, size =rel(2.6)))

## ----dynamic_fit, echo =TRUE, eval = TRUE-------------------------------------
### Fit Stan models
## seasonal dynamics, no predictions
## 4 Markov chains, 'n_iter' iterations each

fit2_stan <- stan_foot(data = italy_2000,
                       model="biv_pois",
                       dynamic_type ="weekly", 
                       #cores = 4,
                       iter = n_iter) # biv poisson
print(fit2_stan, pars =c("home", "rho", "sigma_att",
                        "sigma_def"))

## ----weekly_fit, echo = TRUE, eval = TRUE-------------------------------------
### Fit Stan models
## weekly dynamics, no predictions
## 4 chains, 'n_iter' iterations each

fit3_stan <- stan_foot(data = italy_2000,
                       model="double_pois",
                       dynamic_type = "weekly",
                       #cores = 4,
                       iter = n_iter)  # double poisson
print(fit3_stan, pars =c("home", "sigma_att",
                        "sigma_def"))

## ----weekly_fit_t, echo = TRUE, eval = TRUE-----------------------------------
### Fit Stan models
## weekly dynamics, no predictions
## 4 chains, 'n_iter' iterations each

fit3_stan_t <- stan_foot(data = italy_2000,
                model="double_pois",
                prior = student_t(4,0, NULL), # 4 df
                prior_sd = cauchy(0,25),
                dynamic_type = "weekly",
                #cores = 4,
                iter = n_iter)  # double poisson
print(fit3_stan_t, pars =c("home", "sigma_att",
                           "sigma_def"))

## ----abilities, echo = TRUE, eval = TRUE, fig.show="hold"---------------------
## Plotting abilities: credible and confidence 95% intervals

foot_abilities(object = fit1_stan, data = italy_2000, cex.var = 1)
foot_abilities(object = fit1_mle, data = italy_2000, cex.var = 1)

## ----abilities_dyn, echo = TRUE, eval = TRUE, fig.show="hold"-----------------
## Plotting abilities: credible and confidence 95% intervals

foot_abilities(fit2_stan, italy_2000)

## ----pp_foot, echo = TRUE, eval = TRUE----------------------------------------
## PP checks: aggregated goal's differences and ordered goal differences

pp_foot(data = italy_2000, object = fit1_stan, 
        type = "aggregated")

pp_foot(data = italy_2000, object = fit1_stan, 
        type = "matches")


## ----pp_checks, echo = TRUE, eval = TRUE--------------------------------------
## PPC densities overlay with the bayesplot package

# extracting the replications

sims <-rstan::extract(fit1_stan)
goal_diff <- italy_2000$hgoal-italy_2000$vgoal

# plotting data density vs replications densities

ppc_dens_overlay(goal_diff, sims$y_rep[,,1]-sims$y_rep[,,2], bw = 0.5)

## ----weekly_predict, echo=TRUE, eval= TRUE------------------------------------
### Fit Stan models
## weekly dynamics, predictions of last four weeks
## 4 chains 'n_iter' iterations each

fit4_stan <- stan_foot(data = italy_2000,
                       model="biv_pois", 
                       predict = 36,
                       dynamic_type = "weekly",
                       #cores = 4,
                       iter = n_iter)  # biv poisson
foot_prob(object = fit4_stan, data = italy_2000,
          home_team = "Reggina Calcio",
          away_team= "AC Milan")

## ----foot_roundrobin, echo=TRUE, eval= TRUE-----------------------------------
## Home win out-of-sample probabilities

foot_round_robin(data = italy_2000, object = fit4_stan)

## ----rank_pred1, echo=TRUE, eval= TRUE----------------------------------------
## Rank league reconstruction

# aggregated plot

foot_rank(data = italy_2000, object = fit1_stan)

# team-specific plot

foot_rank(data = italy_2000, object = fit1_stan, visualize = "individual")


## ----rank_pred2, echo=TRUE, eval= TRUE----------------------------------------
## Rank predictions for individual teams

# aggregated plot

foot_rank(data = italy_2000, object = fit4_stan)

# team-specific plot

foot_rank(italy_2000, fit4_stan, 
          team_sel = c("AC Milan", "AS Roma"), 
          visualize = "individual")

foot_rank(italy_2000, fit4_stan, 
          visualize = "individual")


## ----loo, echo = TRUE, eval = TRUE--------------------------------------------
### Model comparisons
## LOOIC, loo function

# extract pointwise log-likelihood

log_lik_1 <- extract_log_lik(fit1_stan)
log_lik_1_t <- extract_log_lik(fit1_stan_t)
log_lik_2 <- extract_log_lik(fit2_stan)
log_lik_3 <- extract_log_lik(fit3_stan)
log_lik_3_t <- extract_log_lik(fit3_stan_t)

# compute loo

loo1 <- loo(log_lik_1)
loo1_t <- loo(log_lik_1_t)
loo2 <- loo(log_lik_2)
loo3 <- loo(log_lik_3)
loo3_t <- loo(log_lik_3_t)


# compare three looic

compare(loo1, loo1_t, loo2, loo3, loo3_t)


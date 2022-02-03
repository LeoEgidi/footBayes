## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_chunk$set(fig.align = 'center', 
                      warning=FALSE, message=FALSE, fig.asp=0.625, fig.height =10, fig.width = 8, 
                      out.width='750px', dpi=200, global.par = TRUE
                      ,dev='png',  dev.args=list(pointsize=10), fig.path = 'figs/'
                      )

## ----footBayes_inst, echo =TRUE, eval = FALSE---------------------------------
#  library(devtools)
#  install_github("LeoEgidi/footBayes")

## ----libraries, echo = TRUE, eval = TRUE--------------------------------------
library(footBayes)
library(tidyverse)
library(engsoccerdata)
library(bayesplot)
library(loo)
library(dplyr)
library(ggplot2)

## ----static_fit, echo =TRUE, eval = TRUE--------------------------------------

### Use Italian Serie A 2000/2001

italy <- as_tibble(italy)
italy_2000<- italy %>%
 dplyr::select(Season, home, visitor, hgoal,vgoal) %>%
 dplyr::filter(Season=="2000")

### Fit Stan models
## no dynamics, no predictions
## 4 Markov chains, 'n_iter' iterations each

n_iter <- 200    # number of MCMC iterations
fit1_stan <- stan_foot(data = italy_2000,
                       model="biv_pois",
                       chains = 4,
                       cores = 4,
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
                         cores = 4,
                         iter = n_iter) # biv poisson


## ----comparing_priors---------------------------------------------------------
## comparing posteriors

posterior1_t <- as.matrix(fit1_stan_t)
model_names <- c("Default", "Stud+Laplace")
color_scheme_set(scheme = "gray")
gl_posterior <- cbind(posterior1[,"sigma_att"], 
                      posterior1_t[,"sigma_att"])
colnames(gl_posterior)<-c("sigma_att", "sigma_att_t")
mcmc_areas(gl_posterior, pars=c("sigma_att", "sigma_att_t"))+
  xaxis_text(on =TRUE, size=ggplot2::rel(2.9))+
  yaxis_text(on =TRUE, size=ggplot2::rel(2.9))+
  scale_y_discrete(labels = ((parse(text= model_names))))+
  ggtitle("Att/def sds")+
  theme(plot.title = element_text(hjust = 0.5, size =rel(2.6)))


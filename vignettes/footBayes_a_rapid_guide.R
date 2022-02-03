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


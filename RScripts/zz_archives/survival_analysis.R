## Survival Analysis
## By: Bridget Bittmann
## Date created: 11/03/2022

## Import Packages ####
remove.packages('rstanarm')
# install.packages("rstanarm", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
#library(rstanarm)
library(devtools)
#install_github('stan-dev/rstanarm',ref = 'feature/survival', build_vignettes = FALSE)
library(brms)
#library(rstanarm)
library(tidyverse)
library(bayesplot)
library(tidybayes)
library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(tibble)
library(ggrepel)
library(flexmix)
library(modelr)
library(loo)
library(survival)
library(tidyr)

## Load the data ####

diversions <- read.csv('~/Desktop/diversion_models/Data.Inputs/mixed_model_input.csv')
diversions <- diversions[!duplicated(diversions[c('Name', 'Year')]),] #remove duplicates
# Format the data appropriately
diversions <- mutate(diversions, Survive = case_when(StartDayofYear >= 60 & EndDayofYear <= 319 ~ 'none',
                                                     EndDayofYear > 319 ~ 'right',
                                                     StartDayofYear < 60 ~ 'left'))

diversions <- diversions %>%
  drop_na(Survive)

## Create a survival model through time ####
# This has no predictor variables

weibull_mod <- brm(Time | cens(Survive) ~ (1 + scale_class1_urban| Name) + scale_class1_urban,
               data = diversions,
               family = weibull,
               iter = 2000,
               cores=getOption('mc.cores', parallel::detectCores()))
summary(weibull_mod)
pp_check(weibull_mod)
weibull_mod <- add_criterion(weibull_mod, criterion = 'loo')

weibull_mod_yr <- brm(Time | cens(Survive) ~ (1 + scale_class1_urban| Name) + (1|Year) + scale_class1_urban,
                   data = diversions,
                   family = weibull,
                   iter = 2000,
                   cores=getOption('mc.cores', parallel::detectCores()))
summary(weibull_mod)
pp_check(weibull_mod)
weibull_mod <- add_criterion(weibull_mod, criterion = 'loo')

exp_mod <- brm(Time | cens(Survive) ~ (1 + scale_class1_urban| Name) + scale_class1_urban,
               data = diversions,
               family = exponential,
               iter = 2000,
               cores=getOption('mc.cores', parallel::detectCores()))

summary(exp_mod)
pp_check(exp_mod)
exp_mod <- add_criterion(exp_mod, criterion = 'loo')

logn_mod <- brm(Time | cens(Survive) ~ (1 + scale_class1_urban| Name) + scale_class1_urban,
               data = diversions,
               family = lognormal,
               iter = 2000,
               cores=getOption('mc.cores', parallel::detectCores()))

summary(logn_mod)
pp_check(logn_mod)
logn_mod <- add_criterion(logn_mod, criterion = 'loo')

cox_mod <- brm(Time | cens(Survive) ~ (1 + scale_class1_urban| Name) + scale_class1_urban,
               data = diversions,
               family = cox,
               iter = 2000,
               cores=getOption('mc.cores', parallel::detectCores()))
summary(cox_mod)
pp_check(cox_mod)

## Compare models with loo ##
loo(exp_mod, logn_mod, weibull_mod)

mae(weibull_mod, diversions$Time)


# K fold cross validation of the different models #

library(brms)
library(bayesplot)
library(tidyverse)
library(tidybayes)
library(plyr)
library(dplyr)
library(readr)
library(tibble)
library(ggrepel)
library(flexmix)
library(modelr)
library(loo)
library(here)

# Import the data
# Model without autoregressive
diversions <- read.csv('~/scratch/kfold-div/data/mixed_model_input.csv')
diversions <- diversions[!duplicated(diversions[c('Name', 'Year')]),] #remove duplicates
diversions$lt <- log(diversions$Acre_feet)
diversions$nat_flow <- diversions$Acre_feet - diversions$AF_used
diversions$rate <- diversions$Acre_feet / diversions$Size_acres

# Priors 
priors <- c(
  set_prior('normal(0,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_class1_urban'),
  set_prior('gamma(1,1)', class = 'sd'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_irrig_prcp'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_irrig_temp'),
  set_prior('normal(0,1)', class = 'b', coef = 'et'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_AF_used')
)

#Model with div flows, climate, and urban

lt.mix.noauto <- brm(lt ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + et + scale_AF_used,
                   data = diversions,
                   family = 'normal',
                   prior = priors,
                   iter = 4000,
                   control = list(max_treedepth = 20,
                                  adapt_delta = 0.999),
                   cores = getOption('mc.cores', parallel::detectCores()))
saveRDS(lt.mix.noauto, file = '~/scratch/kfold-div/model_output/lt-mix-noauto.RDS')

#Kfold validation
print('Kfold validation for non-autoregressive')
kfold1<- kfold(lt.mix.noauto)
saveRDS(kfold1, file = '~/scratch/kfold-div/model_output/k-noauto.RDS')

# Climate model 
print('Climate model')

priors <- c(
  set_prior('normal(0,1)', class = 'Intercept'),
  set_prior('gamma(1,1)', class = 'sd'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_irrig_prcp'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_irrig_temp'),
  set_prior('normal(0,1)', class = 'b', coef = 'et')
)

lt.climate <- brm(lt ~ (1 | Name) + (1|Year) + scale_irrig_prcp + scale_irrig_temp + et,
                  data = diversions,
                  family = 'normal',
                  prior = priors,
                  iter = 4000,
                  control = list(max_treedepth = 20,
                                 adapt_delta = 0.999),
                  cores = getOption('mc.cores', parallel::detectCores()))
summary(lt.climate)
saveRDS(lt.climate, file = '~/scratch/kfold-div/model_output/lt-climate-noauto.RDS')

#Kfold validation
print('Kfold validation for climate non-autoregressive')
kfold2<- kfold(lt.climate)
saveRDS(kfold2, file = '~/scratch/kfold-div/model_output/k-clim.RDS')

# Urban effect model 
print('Urban effect model')
priors <- c(
  set_prior('normal(0,1)', class = 'Intercept'),
  set_prior('gamma(2,1)', class = 'sd'),
  set_prior('normal(0,0.5)', class = 'b', coef = 'scale_class1_urban')
)

lt.urb <- brm(lt ~ (1 + scale_class1_urban | Name) + scale_class1_urban,
              data = diversions,
              prior = priors,
              iter = 4000,
              family = 'normal',
              control=list(max_treedepth=20,
                           adapt_delta=0.99),
              cores=getOption('mc.cores', parallel::detectCores()))
print('Urban effect model results')
summary(lt.urb)
saveRDS(lt.urb, file = '~/scratch/kfold-div/model_output/lt-urb-noauto.RDS')

#Kfold validation
print('Kfold validation for urban non-autoregressive')
kfold3<- kfold(lt.urb)
saveRDS(kfold3, file = '~/scratch/kfold-div/model_output/k-urb.RDS')







## MODEL EXPLORATION ##

## This script will do 3 things: 
# 1) I will explore the tail effects in my data.
# 2) I will use an offset to help the varying intercept fit better
# 3) I will look into adding an autoregressive term. 


library(brms)
library(bayesplot)
library(tidyverse)
library(tidybayes)
library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(tibble)
library(ggrepel)
library(flexmix)
library(modelr)

#Read in the data
diversions <- data.frame(read.csv('~/Desktop/diversion_models/Data.Inputs/mixed_model_input.csv'))

## FIRST ATTEMPT: TAIL EFFECTS

# Calculate log transform of AF
diversions$log_trans <- log(diversions$Acre_feet)

# Calculate the mean and variance of the log transformed data
mean(diversions$log_trans)
var(diversions$log_trans)

# Import a model with a lognormal distribution
load('~/Desktop/diversion_models/Model_outputs/AF_logn_mix.Rdata')
posterior <- add_predicted_draws(diversions, AF.logn.mix, ndraws=10)
# Calculate the mean and variance of the log normal distribution
log(mean(posterior$.prediction))
log(var(posterior$.prediction))

# Conclusion: The mean and variance log transforming the data is VERY different than applying 
#             a log normal distribution to the data. The mean and variance for the log transformed
#             data is 8.10 and 5.24 respectively. The mean and variance for predicted posterior draws 
#             is 10.34 and 23.50 respectively.

# Create a model throwing away the top top and bottom 20% of the data. 

data_sub <- diversions %>%
  filter(between(Acre_feet, quantile(Acre_feet, 0.1), quantile(Acre_feet, 0.9)))

hist(data_sub$Acre_feet)
min(data_sub$Acre_feet)
max(data_sub$Acre_feet)

prior_test <- test_prior <- c(
  set_prior("normal(7.5,0.2)", class="Intercept"),
  set_prior("normal(1,0.2)", class = "sd"), 
  set_prior("normal(0,0.5)", class = "b", coef = "scale_.class1_urban")
)

prior_mod <- prior_samples <- brm(Acre_feet ~ (1 + scale_.class1_urban | Name) + scale_.class1_urban,
                                  family = "lognormal",
                                  data = data_sub,  # This data won't actually be incorporated into the posterior! Just there to let the model run.
                                  prior = prior_test, 
                                  sample_prior = "only")
preds_from_prior <-  add_predicted_draws(data_sub, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_.irrig_prcp, y = .prediction)) + 
  geom_point(alpha = 0.4)
max(preds_from_prior$.prediction)
mean(preds_from_prior$.prediction)
median(preds_from_prior$.prediction)
min(preds_from_prior$.prediction)

mod.sub <- brm(Acre_feet ~ (1 + scale_.class1_urban | Name) + scale_.class1_urban,
               data = data_sub,
               family = 'lognormal',
               iter = 2000,
               prio = prior_test,
               control = list(max_treedepth = 20,
                              adapt_delta = 0.99),
               cores = getOption('mc.cores', parallel::detectCores())
)
summary(mod.sub)
pp_check(mod.sub)
conditional_effects(mod.sub)

## CONCLUSION: Tails ARE very much influencing the outputs of the models. 

## SECOND: OFFSETS IN THE DATA

# Create priors for urban offset model
priors <- c(
  set_prior('normal(1,0.2)', class = 'Intercept'),
  set_prior('normal(0,0.3)', class = 'sd'),
  set_prior('normal(0,0.1)', class = 'b', coef = 'scale_.class1_urban')
  )

prior_mod <- brm(Acre_feet ~ (1 + scale_.class1_urban | Name) + scale_.class1_urban + offset(log(Size_acres)),
                 data = diversions,
                 family = 'lognormal',
                 prior = priors,
                 sample_prior = 'only')
preds_from_prior <-  add_predicted_draws(diversions, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_.irrig_prcp, y = .prediction)) + 
  geom_point(alpha = 0.4)
min(preds_from_prior$.prediction)
max(preds_from_prior$.prediction)
mean(preds_from_prior$.prediction)
median(preds_from_prior$.prediction)

mod.urb.off <- brm(Acre_feet ~ (1 + scale_.class1_urban | Name) + scale_.class1_urban + offset(log(Size_acres)),
                   data = diversions,
                   family = 'lognormal',
                   iter = 2000,
                   prior = priors,
                   control = list(max_treedepth = 20,
                                  adapt_delta = 0.99),
                   cores = getOption('mc.cores', parallel::detectCores())
                   )
summary(mod.urb.off)
plot(mod.urb.off)
pp_check(mod.urb.off)
conditional_effects(mod.urb.off)
save(mod.urb.off, file = '~/Desktop/diversion_models/Model_outputs/mod_urb_offset.Rdata')

# Create priors for a mix of climate and urban offset model
priors <- c(
  set_prior('normal(0,0.1)', class = 'Intercept'),
  set_prior('normal(0,0.1)', class = 'sd'),
  set_prior('normal(0,0.3)', class = 'b', coef = 'scale_.class1_urban'),
  set_prior('normal(0,0.3)', class = 'b', coef = 'et'),
  set_prior('normal(0,0.3)', class = 'b', coef = 'scale_.irrig_prcp'),
  set_prior('normal(0,0.3)', class = 'b', coef = 'scale_.irrig_temp')
)
prior_mod <- brm(Acre_feet ~ (1 + scale_.class1_urban | Name) + scale_.class1_urban + et + scale_.irrig_prcp + scale_.irrig_temp + offset(log(Size_acres)),
                 data = diversions,
                 family = 'lognormal',
                 prior = priors,
                 sample_prior = 'only')
preds_from_prior <-  add_predicted_draws(diversions, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_.irrig_prcp, y = .prediction)) + 
  geom_point(alpha = 0.4) +
  ylim(0, 850000)
min(preds_from_prior$.prediction)
max(preds_from_prior$.prediction)
mean(preds_from_prior$.prediction)
median(preds_from_prior$.prediction)

mod.mix.off <- brm(Acre_feet ~ (1 + scale_.class1_urban | Name) + scale_.class1_urban + et + scale_.irrig_prcp + scale_.irrig_temp + offset(log(Size_acres)),
                 data = diversions,
                 family = 'lognormal',
                 prior = priors,
                 iter = 2000,
                 control = list(max_treedepth = 20,
                                adapt_delta = 0.99),
                 cores = getOption('mc.cores', parallel::detectCores()))
plot(mod.mix.off)
summary(mod.mix.off)
conditional_effects(mod.mix.off)
ranef(mod.mix.off)
save(mod.mix.off, file = '~/Desktop/diversion_models/Model_outputs/mod_mix_offset.Rdata')


## CONCLUSION: Using an offset did seem to help with intercept sampling and the 
#             speed of the model. However, it only helped in tandem with setting more
#             informative priors. The offset DOES change the outputs of the model quite a bit.
#             The baseline intercept is at about 38,000 rather than 3,000. The negative
#             urban effect disappears. 

## THIRD: AUTOREGRESSIVE TERM

mod.sub <- brm(Acre_feet ~ (1|Name) + ar(time = Year, gr = Name),
               data = diversions,
               family = 'lognormal',
               iter = 2000,
               control = list(max_treedepth = 20,
                              adapt_delta = 0.99),
               cores = getOption('mc.cores', parallel::detectCores())
)

## FOURTH: URBAN EFFECT VARYING -- TRY WITH FIXED EFFECT

priors <- c(
  set_prior('normal(1,0.2)', class = 'Intercept'),
  set_prior('normal(0,0.1)', class = 'sd'),
  set_prior('normal(0,0.4)', class = 'b', coef = 'scale_.class1_urban'),
  set_prior('normal(0,0.4)', class = 'b', coef = 'et'),
  set_prior('normal(0,0.4)', class = 'b', coef = 'scale_.irrig_prcp'),
  set_prior('normal(0,0.4)', class = 'b', coef = 'scale_.irrig_temp')
)

mod.mix.off.ng <- brm(Acre_feet ~ (1|Name) + scale_.class1_urban + et + scale_.irrig_prcp + scale_.irrig_temp + offset(log(Size_acres)),
                      data = diversions,
                      family = 'lognormal',
                      prior = priors,
                      iter = 2000,
                      control = list(max_treedepth = 20,
                                     adapt_delta = 0.99),
                      cores = getOption('mc.cores', parallel::detectCores()))
summary(mod.mix.off.ng)

priors <- c(
  set_prior('normal(7,0.5)', class = 'Intercept'),
  set_prior('normal(2,0.2)', class = 'sd'),
  set_prior('normal(0,0.3)', class = 'b', coef = 'scale_.class1_urban'),
  set_prior('normal(0,0.3)', class = 'b', coef = 'et'),
  set_prior('normal(0,0.3)', class = 'b', coef = 'scale_.irrig_prcp'),
  set_prior('normal(0,0.3)', class = 'b', coef = 'scale_.irrig_temp')
)

prior_mod <- brm(Acre_feet ~ (1|Name) + scale_.class1_urban + et + scale_.irrig_prcp + scale_.irrig_temp,
                           data = diversions,
                           family = 'lognormal',
                           prior = priors,
                           sample_prior = 'only')

preds_from_prior <-  add_predicted_draws(diversions, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_.irrig_prcp, y = .prediction)) + 
  geom_point(alpha = 0.4) +
  ylim(0, 850000)
mean(preds_from_prior$.prediction)

AF.logn.mix.ng <- brm(Acre_feet ~ (1|Name) + scale_.class1_urban + scale_.irrig_prcp + scale_.irrig_temp + et,
                   data=diversions,
                   family = 'lognormal',
                   iter=2000,
                   prior = priors,
                   control=list(max_treedepth=20,
                                adapt_delta=0.99),
                   cores=getOption('mc.cores', parallel::detectCores()))
summary(AF.logn.mix.ng)
pp_check(AF.logn.mix.ng)

priors <- c(
  set_prior('normal(7,0.2)', class = 'Intercept'),
  set_prior('normal(2,0.1)', class = 'sd'),
  set_prior('normal(0,0.25)', class = 'b', coef = 'scale_.class1_urban'),
  set_prior('normal(0,0.25)', class = 'b', coef = 'et'),
  set_prior('normal(0,0.25)', class = 'b', coef = 'scale_.irrig_prcp'),
  set_prior('normal(0,0.25)', class = 'b', coef = 'scale_.irrig_temp')
)

prior_mod <- brm(log_trans ~ (1|Name) + scale_.class1_urban + et + scale_.irrig_prcp + scale_.irrig_temp,
                 data = diversions,
                 family = 'normal',
                 prior = priors,
                 sample_prior = 'only')

preds_from_prior <-  add_predicted_draws(diversions, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_.irrig_prcp, y = .prediction)) + 
  geom_point(alpha = 0.4)
mean(preds_from_prior$.prediction)
max(diversions$log_trans)
min(diversions$log_trans)

lt.ng <- brm(log_trans ~ (1|Name) + scale_.class1_urban + et + scale_.irrig_prcp + scale_.irrig_temp,
             data=diversions,
             family = 'normal',
             iter=2000,
             prior = priors,
             control=list(max_treedepth=20,
                          adapt_delta=0.99),
             cores=getOption('mc.cores', parallel::detectCores()))
plot(lt.ng)

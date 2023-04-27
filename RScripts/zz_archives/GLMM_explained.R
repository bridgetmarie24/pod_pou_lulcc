## Models with hypotheses ##

## By: Bridget Bittmann
## Date created: 10/05/2022

#### Import Packages ####

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
library(loo)
library(tidyr)

###### Import the data ######

diversions <- read.csv('~/Desktop/diversion_models/Data.Inputs/mixed_model_input.csv')
diversions <- diversions[!duplicated(diversions[c('Name', 'Year')]),] #remove duplicates
diversions$lt <- log(diversions$Acre_feet)
diversions$nat_flow <- diversions$Acre_feet - diversions$AF_used
diversions$rate <- diversions$Acre_feet / diversions$Size_acres

## Assessing Model Fit ####

# Median Absolute Error
mae <- function(model, data_compare){
  yhat <- posterior_predict(model)
  resid <- sweep(yhat, 
                 2,
                 data_compare,
                 FUN="-")
  return(median(abs(resid)))
}

mae_lt <- function(model, data_compare){
  yhat <- exp(posterior_predict(model))
  resid <- sweep(yhat, 
                 2,
                 data_compare,
                 FUN="-")
  return(median(abs(resid)))
}

# Compare volume models against one another using LOO 

loo(AF.mix, AF.mix.stor, AF.urb)

# Compare timing models against one another using LOO 

loo(len.climate, len.mix, len.mix.ng, len.stor, len.stor.ng, len.urb)

##### CREATE VOLUME MODELS ####
# Fitting the diversions with a lognormal distribution is quite difficult because of
# the spread in the data

#### Urban only effect ####
# Run lognormal dist. on raw data and normal dist. on log-transformed data using same priors

priors <- c(
  set_prior('normal(0,1)', class = 'Intercept'),
  set_prior('gamma(2,1)', class = 'sd'),
  set_prior('normal(0,0.5)', class = 'b', coef = 'scale_class1_urban')
)

prior_mod <- brm(Acre_feet ~ (1 + scale_class1_urban | Name) + scale_class1_urban,
                 family = "lognormal",
                 data = diversions,  # This data won't actually be incorporated into the posterior! Just there to let the model run.
                 prior = priors, 
                 sample_prior = "only")
preds_from_prior <-  add_predicted_draws(diversions, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_class1_urban, y = .prediction)) + 
  geom_point(alpha = 0.4) +
  ylim(0,1000000)

AF.urb <- brm(Acre_feet ~ (1 + scale_class1_urban | Name) + scale_class1_urban,
              data = diversions,
              prior = priors,
              iter = 4000,
              family = 'lognormal',
              control=list(max_treedepth=20,
                          adapt_delta=0.99),
              cores=getOption('mc.cores', parallel::detectCores()))

plot(AF.urb)
summary(AF.urb)
pp_check(AF.urb) +
  xlim(0,1000000)
mae(AF.urb, diversions$Acre_feet)
AF.urb <- add_criterion(AF.urb, criterion='loo')
save(AF.urb, file = '~/Desktop/diversion_models/Model_outputs/Af_urb_1026.Rdata')

lt.urb <-brm(lt ~ (1 + scale_class1_urban | Name) + scale_class1_urban,
             data = diversions,
             prior = priors,
             iter = 4000,
             family = 'normal',
             control=list(max_treedepth=20,
                          adapt_delta=0.99),
             cores=getOption('mc.cores', parallel::detectCores()))

summary(lt.urb)
plot(lt.urb)
pp_check(lt.urb)
lt.urb <- add_criterion(lt.urb, criterion = 'loo')
save(lt.urb, file = '~/Desktop/diversion_models/Model_outputs/lt_urb_1017.Rdata')

#### Urban Autoregressive and time ####

lt.urb.auto <-brm(lt ~ (1 + scale_class1_urban | Name) + scale_class1_urban + arma(time = Year, gr = Name),
             data = diversions,
             prior = priors,
             iter = 2000,
             family = 'normal',
             control=list(max_treedepth=20,
                          adapt_delta=0.99),
             cores=getOption('mc.cores', parallel::detectCores()))
summary(lt.urb.auto)
save(lt.urb.auto, file = '~/Desktop/diversion_models/Model_outputs/lt_urb_auto.Rdata')


plt <- conditional_effects(lt.urb.auto, method = 'posterior_predict', transform = 'exp')
plot(plt, plot = FALSE)[[1]] +
  xlab('Urban Proportion') +
  ylab('Discharge (AF/yr)')

plt <- conditional_effects(lt.urb, method = 'posterior_predict', transform = 'exp')
plot(plt, plot = FALSE)[[1]] +
  xlab('Urban Proportion') +
  ylab('Discharge (AF/yr)')

lt.urb.time <-brm(lt ~ (1 + scale_class1_urban | Name) + (1|Year) + scale_class1_urban,
                  data = diversions,
                  prior = priors,
                  iter = 2000,
                  family = 'normal',
                  control=list(max_treedepth=20,
                               adapt_delta=0.99),
                  cores=getOption('mc.cores', parallel::detectCores()))
summary(lt.urb.time)
conditional_effects(lt.urb.time)

#### Climate only effects ####
# Run lognormal dist on raw data and normal dist. on log-transformed data to compare results

priors <- c(
  set_prior('normal(0,1)', class = 'Intercept'),
  set_prior('gamma(1,1)', class = 'sd'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_irrig_prcp'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_irrig_temp'),
  set_prior('normal(0,1)', class = 'b', coef = 'et')
)

prior_mod <- brm(Acre_feet ~ (1 | Name) + (1|Year) + scale_irrig_prcp + scale_irrig_temp + et,
                 data = diversions,
                 prior = priors,
                 family = 'lognormal',
                 sample_prior = 'only')
preds_from_prior <-  add_predicted_draws(diversions, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_irrig_temp, y = .prediction)) + 
  geom_point(alpha = 0.4) +
  ylim(0,1000000)

lt.climate <- brm(lt ~ (1 | Name) + (1|Year) + scale_irrig_prcp + scale_irrig_temp + et,
                  data = diversions,
                  family = 'normal',
                  prior = priors,
                  iter = 4000,
                  control = list(max_treedepth = 20,
                                 adapt_delta = 0.999),
                  cores = getOption('mc.cores', parallel::detectCores()))
summary(AF.climate)
plot(AF.climate)
AF.climate <- add_criterion(AF.climate, criterion = 'loo')

lt.climate.auto <- brm(lt ~ (1 | Name) + scale_irrig_prcp + scale_irrig_temp + et + arma(time = Year, gr = Name),
                  data = diversions,
                  family = 'normal',
                  prior = priors,
                  iter = 4000,
                  control = list(max_treedepth = 20,
                                 adapt_delta = 0.999),
                  cores = getOption('mc.cores', parallel::detectCores()))
summary(lt.climate.auto)

plot(lt.climate)
lt.climate <- add_criterion(lt.climate, criterion = 'loo')
save(lt.climate.auto, file = '~/Desktop/diversion_models/Model_outputs/lt_climate_auto.Rdata')

plt <- conditional_effects(lt.climate.auto, method = 'posterior_predict', transform = 'exp')
plot(plt, plot = FALSE)[[3]] +
  xlab('Evapotranspiration (m)') +
  ylab('Discharge (AF/yr')

#### Autoregressive climate ####

lt.climate <- brm(lt ~ (1 | Name) + scale_irrig_prcp + scale_irrig_temp + et,
                  data = diversions,
                  family = 'normal',
                  prior = priors,
                  iter = 4000,
                  control = list(max_treedepth = 20,
                                 adapt_delta = 0.999),
                  cores = getOption('mc.cores', parallel::detectCores()))

plot(lt.climate)
summary(lt.climate)
plt <- conditional_effects(AF.climate, method = 'posterior_predict')
plot(plt, plot = FALSE)[[3]]+
  xlab('Evapotranspiration (m)') +
  ylab('Discharge (AF/yr)')

#### Urban and climate #####

priors <- c(
  set_prior('normal(0,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_class1_urban'),
  set_prior('gamma(1,1)', class = 'sd'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_irrig_prcp'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_irrig_temp'),
  set_prior('normal(0,1)', class = 'b', coef = 'et')
)

prior_mod <- brm(Acre_feet ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + et,
                 data = diversions,
                 prior = priors,
                 family = 'lognormal',
                 sample_prior = 'only')
preds_from_prior <-  add_predicted_draws(diversions, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_irrig_temp, y = .prediction)) + 
  geom_point(alpha = 0.4) +
  ylim(0,1000000)

AF.mix <- brm(Acre_feet ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + et,
                  data = diversions,
                  family = 'lognormal',
                  prior = priors,
                  iter = 4000,
                  control = list(max_treedepth = 20,
                                 adapt_delta = 0.999),
                  cores = getOption('mc.cores', parallel::detectCores()))
summary(AF.mix)
plot(AF.mix)
pp_check(AF.mix) + 
  xlim(0,900000) +
  xlab('Annual Discharge (AF/yr)') +
  ylab('Density')
AF.mix <- add_criterion(AF.mix, criterion = 'loo')
mae(AF.mix, diversions$Acre_feet)
save(AF.mix, file = '~/Desktop/diversion_models/Model_outputs/AF_mix_1017.Rdata')

lt.mix <- brm(Acre_feet ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + et + arma(gr=Name, time=Year),
              data = diversions,
              family = 'normal',
              prior = priors,
              iter = 4000,
              control = list(max_treedepth = 20,
                             adapt_delta = 0.999),
              cores = getOption('mc.cores', parallel::detectCores()))

#### Urban and climate autoregressive #####

lt.mix <- brm(Acre_feet ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + et,
              data = diversions,
              family = 'normal',
              prior = priors,
              iter = 4000,
              control = list(max_treedepth = 20,
                             adapt_delta = 0.999),
              cores = getOption('mc.cores', parallel::detectCores()))


#### Urban, climate, and reservoir control ####

priors <- c(
  set_prior('normal(0,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_class1_urban'),
  set_prior('gamma(1,1)', class = 'sd'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_irrig_prcp'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_irrig_temp'),
  set_prior('normal(0,1)', class = 'b', coef = 'et'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_AF_used')
)

AF.mix.stor <- brm(Acre_feet ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + et + scale_AF_used,
              data = diversions,
              family = 'lognormal',
              prior = priors,
              iter = 4000,
              control = list(max_treedepth = 20,
                             adapt_delta = 0.999),
              cores = getOption('mc.cores', parallel::detectCores()))

summary(AF.mix.stor)
mae(AF.mix.stor, diversions$Acre_feet)
AF.mix.stor <- add_criterion(AF.mix.stor, criterion = 'loo')
save(AF.mix.stor, file = '~/Desktop/diversion_models/Model_outputs/AF_mix_stor_1017.Rdata')

## Urban, climate, reservoir control, and discharge at t-1 ####

## Need to restructure data a little bit

div_prev <- diversions %>%
  group_by(Name) %>%
  mutate(vol_prev = dplyr::lag(Acre_feet, n=1))
div_prev <- div_prev %>%
  drop_na(vol_prev)

priors <- c(
  set_prior('normal(0,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_class1_urban'),
  set_prior('gamma(1,1)', class = 'sd'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_irrig_prcp'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_irrig_temp'),
  set_prior('normal(0,1)', class = 'b', coef = 'et'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_AF_used')
)

# AR for real values
AF.prev <- brm(lt ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + et + scale_AF_used +log(vol_prev),
                      data = div_prev,
                      family = 'normal',
                      prior = priors,
                      iter = 4000,
                      control = list(max_treedepth = 20,
                                     adapt_delta = 0.999),
                      cores = getOption('mc.cores', parallel::detectCores()))
summary(AF.prev)
plot(AF.prev)
conditional_effects(AF.prev)
pp_check(AF.prev)
mae_lt(AF.prev, div_prev$Acre_feet)
AF.prev <- add_criterion(AF.prev, criterion = 'loo')
save(AF.prev, file = '~/Desktop/diversion_models/Model_outputs/AF_prev_AR.Rdata')

# ARMA for residuals
AF.arma.four <- brm(lt ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + et + scale_AF_used + arma(gr=Name),
               data = div_prev,
               family = 'normal',
               prior = priors,
               iter = 4000,
               control = list(max_treedepth = 20,
                              adapt_delta = 0.999),
               cores = getOption('mc.cores', parallel::detectCores()))
mae_lt(AF.arma, div_prev$Acre_feet)
summary(AF.arma)
conditional_effects(AF.arma, method = 'posterior_predict', transform = 'exp')
AF.arma <- add_criterion(AF.arma.four, criterion='loo')
save(AF.arma, file = '~/Desktop/diversion_models/Model_outputs/AF_arma_full.Rdata')

# ARMA full
Arma.full <- brm(lt ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + et + scale_AF_used + arma(gr=Name),
                    data = diversions,
                    family = 'normal',
                    prior = priors,
                    iter = 4000,
                    control = list(max_treedepth = 20,
                                   adapt_delta = 0.999),
                    cores = getOption('mc.cores', parallel::detectCores()))

# Mix AR and ARMA
AF.arma.ar <- brm(lt ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + et + scale_AF_used + log(vol_prev) + arma(gr=Name),
               data = div_prev,
               family = 'normal',
               prior = priors,
               iter = 4000,
               control = list(max_treedepth = 20,
                              adapt_delta = 0.999),
               cores = getOption('mc.cores', parallel::detectCores()))

AF.mix.stor.data <- brm(lt ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + et + scale_AF_used,
                   data = div_prev,
                   family = 'normal',
                   prior = priors,
                   iter = 4000,
                   control = list(max_treedepth = 20,
                                  adapt_delta = 0.999),
                   cores = getOption('mc.cores', parallel::detectCores()))
summary(AF.mix.stor.data)
mae_lt(AF.mix.stor.data, div_prev$Acre_feet)
AF.mix.stor.data <- add_criterion(AF.mix.stor.data, criterion = 'loo')
save(AF.mix.stor.data, file = '~/Desktop/diversion_models/Model_outputs/AF_mix_data.Rdata')


AF_split <- split(diversions, diversions$Name)
out <- Map(
  f = function(x,y)
    forecast::ggPacf(x$Acre_feet) + labs(title = y),
  x = AF_split,
  y = names(AF_split)
)

#### Urban, climate, and reservoir control (no ny) ####

div_ny <- diversions[!(diversions$Name == 'New York Canal'), ]

NoNY.AF <- brm(Acre_feet ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + et + scale_AF_used,
                data = div_ny,
                family = 'lognormal',
                prior = priors,
                iter = 2000,
                control = list(max_treedepth = 20,
                               adapt_delta = 0.999),
                cores = getOption('mc.cores', parallel::detectCores()))
summary(NoNY.AF)

NoNY.mix <- brm(lt ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + et + scale_AF_used,
                   data = div_ny,
                   family = 'normal',
                   prior = priors,
                   iter = 2000,
                   control = list(max_treedepth = 20,
                                  adapt_delta = 0.999),
                   cores = getOption('mc.cores', parallel::detectCores()))
summary(NoNY.mix)
mae_lt(NoNy.mix, div_ny$Acre_feet)
NoNy.mix <- add_criterion(NoNY.mix, criterion = 'loo')
save(NoNy.mix, file = '~/Desktop/diversion_models/Model_outputs/NoNY_110722.Rdata')


NoNY.mix.auto <- brm(lt ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + et + scale_AF_used + arma(time = Year, gr = Name),
                data = div_ny,
                family = 'normal',
                prior = priors,
                iter = 2000,
                control = list(max_treedepth = 20,
                               adapt_delta = 0.999),
                cores = getOption('mc.cores', parallel::detectCores()))
summary(NoNY.mix.auto)
mae_lt(NoNy.mix.auto, div_ny$Acre_feet)
pp_check(NoNY.mix.auto)
conditional_effects(NoNY.mix.auto)
NoNy.mix.auto <- add_criterion(NoNY.mix.auto, criterion = 'loo')
save(NoNy.mix.auto, file = '~/Desktop/diversion_models/Model_outputs/NoNY_auto_110722.Rdata')

NoNY.time <- brm(lt ~ (1 + scale_class1_urban | Name) + (1|Year) + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + et + scale_AF_used,
                     data = div_ny,
                     family = 'normal',
                     prior = priors,
                     iter = 2000,
                     control = list(max_treedepth = 20,
                                    adapt_delta = 0.999),
                     cores = getOption('mc.cores', parallel::detectCores()))

summary(NoNY.time)
NoNY.time <- add_criterion(NoNY.time, criterion = 'loo')
save(NoNY.time, file = '~/Desktop/diversion_models/Model_outputs/NoNY_time_110722.Rdata')

## RUN INDIVIDUAL BRMS MODELS ####

names <- unique(diversions$Name)
stored <- data.frame(matrix(nrow=63, ncol=7))
cols <- c('Name', 'Int', 'Urban', 'Precip', 'Temp', 'ET', 'Storage')
colnames(stored) <- cols

for (i in 1:length(names)){
  fit <- brm(Acre_feet ~ scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + et + scale_AF_used,
             data = diversions[diversions$Name == names[i],],
             family = "Gamma"(link = log),
             cores = getOption('mc.cores', parallel::detectCores()))
  stored$Name[i] <- fixef(fit)[1]
  stored$Int[i] <- fixef(fit)[2]
  stored$Urban[i] <- fixef(fit)[3]
  stored$Precip[i] <- fixef(fit)[4]
  stored$Temp[i] <- fixef(fit)[5]
  stored$ET[i] <- fixef(fit)[6]
  stored$Storage[i] <- fixef(fit)[7]
}

summary(fit)

stored$Name[i] <- names[i]


loo1 <- loo(AF.arma, save_psis = TRUE)
loo2 <- loo(AF.mix, save_psis = TRUE)

kfold(AF.arma, k=34)




mcmc_plot(arma,
          type = 'areas',
          variable = c('b_et',
                       'b_scale_irrig_prcp',
                       'b_scale_irrig_temp',
                       'b_scale_class1_urban',
                       'b_scale_AF_used'),
          prob = 0.95)

mcmc_plot(noauto,
          type = 'areas',
          variable = c('b_et',
                       'b_scale_irrig_prcp',
                       'b_scale_irrig_temp',
                       'b_scale_class1_urban',
                       'b_scale_AF_used'),
          prob = 0.95)


gglot(diversions, aes())

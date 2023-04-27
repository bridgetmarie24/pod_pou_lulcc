## ---------------------------------- ##
## Mixed Effects Model for Diversions ## 
## ---------------------------------- ## 

## By: Bridget Bittmann
## Date created: 06/09/2022
## Date modified: 

## --------------------------------------- ## 
## Section 1: Import packages and the data ##
## --------------------------------------- ##

#install.packages('htmltools')
#remove.packages('brms')
#install.packages('brms')
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

diversions <- read_csv('~/Desktop/diversion_models/Data.Inputs/mixed_model_input.csv')
diversions <- data.frame(diversions)
diversions <- na.omit()
small <- diversions %>% 
  group_by(Name) %>%
  filter(Acre_feet<20000)
hist(small$Acre_feet)

high_urb_change <- data.frame(read_csv('~/Desktop/diversion_models/Data.Inputs/high_change.csv'))
names <- as.list(high_urb_change$Name)
change <- diversions[diversions$Name %in% names,]
hist(change$Acre_feet, breaks = 15)

mod.climate <- brm(Acre_feet ~ (1|Name)+scale_.irrig_prcp+scale_.irrig_temp+et+scale_.Max_Fill,
                   data=diversions,
                   family='lognormal',
                   iter = 4000,
                   control=list(max_treedepth=20, 
                                adapt_delta=0.999),
                   cores=getOption('mc.cores', parallel::detectCores()))
summary(mod.climate)
pp_check(mod.climate)
save(mod.climate, file='~/Desktop/diversion_models/Model_outputs/AF_logn_clim_ir.Rdata')
plot(mod.climate)
mcmc_areas(mod.climate, pars = c('b_scale_.irrig_prcp',
                                 'b_scale_.irrig_temp',
                                 'b_et',
                                 'b_scale_.Max_Fill'))
conditional_effects(mod.climate)

AF.logn.JAtmp <- brm(Acre_feet ~ (1|Name) + scale_.JuneAug_temp,
                     data = diversions,
                     family = 'lognormal',
                     iter = 20000, 
                     control = list(max_treedepth = 20,
                                    adapt_delta = 0.999),
                     save_pars = save_pars(all=TRUE),
                     cores = getOption('mc.cores', parallel::detectCores()))
summary(AF.logn.JAtmp)

AF.logn.irtmp <- brm(Acre_feet ~ (1|Name) + scale_.irrig_temp,
                     data = diversions,
                     family = 'lognormal',
                     iter = 12000, 
                     control = list(max_treedepth = 20,
                                    adapt_delta = 0.999),
                     save_pars = save_pars(all=TRUE),
                     cores = getOption('mc.cores', parallel::detectCores()))
summary(AF.logn.irtmp)


AF_logn_clim_non <- brm(Acre_feet ~ (1|Name) + scale_.ant_prcp + scale_.Max_Fill + scale_.LP_inflows,
                        data=diversions,
                        family='lognormal',
                        iter = 8000,
                        control=list(max_treedepth=20, 
                                     adapt_delta=0.999),
                        cores=getOption('mc.cores', parallel::detectCores()))
save(AF_logn_clim_non, file='~/Desktop/diversion_models/Model_outputs/AF_logn_clim_non.Rdata')
summary(AF_logn_clim_non)

AF_logn_clim2 <- brm(Acre_feet ~ (1|Name) + scale_.ant_prcp + scale_.irrig_prcp + et + scale_.JuneAug_temp + scale_.LP_inflows,
                    data=diversions,
                    family='lognormal',
                    iter = 8000,
                    control=list(max_treedepth=20, 
                                 adapt_delta=0.999),
                    cores=getOption('mc.cores', parallel::detectCores()))
save(AF_logn_clim2, file='~/Desktop/diversion_models/Model_outputs/AF_logn_clim2.Rdata')
summary(AF_logn_clim2)

AF_lon_clim3 <- brm(Acre_feet ~ (1|Name)+scale_.irrig_prcp+scale_.JuneAug_temp+et+scale_.Max_Fill,
                   data=diversions,
                   family='lognormal',
                   iter = 8000,
                   control=list(max_treedepth=20, 
                                adapt_delta=0.999),
                   cores=getOption('mc.cores', parallel::detectCores()))
summary(AF_lon_clim3)

mod.urban <- brm(Acre_feet ~ (1+scale_.class1_urban|Name) + scale_.class1_urban,
                 data=diversions,
                 family = 'lognormal',
                 iter=4000,
                 control=list(max_treedepth=20,
                              adapt_delta=0.99),
                 cores=getOption('mc.cores', parallel::detectCores()))
save(mod.urban, file='~/Desktop/diversion_models/Model_outputs/AF_logn_urb.Rdata')
pp_check(mod.urban)
summary(mod.urban)
mcmc_areas(mod.urban, pars=c('b_scale_.class1_urban'), prob=0.9)
mcmc_areas(mod.urban, prob=0.9)
ranef(mod.urban)
conditional_effects(mod.urban)

AF.logn.urb.ng <- brm(Acre_feet ~ (1|Name) + scale_.class1_urban,
                 data=subset(diversions),
                 family = 'lognormal',
                 iter=4000,
                 control=list(max_treedepth=20,
                              adapt_delta=0.99),
                 save_pars=save_pars(all=TRUE),
                 cores=getOption('mc.cores', parallel::detectCores()))
summary(AF.logn.urb.ng)
plot(AF.logn.urb.ng)
AF.logn.urb.ng <- add_criterion(AF.logn.urb.ng, criterion='loo', moment_match=TRUE)
save(AF.logn.urb.ng, file='~/Desktop/diversion_models/Model_outputs/AF_logn_urb_ng.Rdata')
pp_check(AF.logn.urb.ng)
summary(AF.logn.urb.ng)
mcmc_areas(AF.logn.urb.ng, pars=c('b_scale_.class1_urban'), prob=0.9)
conditional_effects(AF.logn.urb.ng)

priors <- c(
  set_prior("normal(7,1)", class="Intercept"),
  set_prior("normal(0,1)", class = "sd"), 
  set_prior("normal(0,0.6)", class = "b", coef = "scale_class1_urban"),
  set_prior("normal(0,0.6)", class = "b", coef = "scale_irrig_prcp"),
  set_prior("normal(0,0.6)", class = "b", coef = "et"),
  set_prior("normal(0,0.6)", class = "b", coef = "scale_irrig_temp"),
  set_prior("normal(0,0.6)", class = "b", coef = "scale_Max_Fill")
)

prior_mod <- brm(Range ~ (1+scale_class1_urban|Name)+scale_irrig_prcp +scale_irrig_temp+scale_Max_Fill + scale_class1_urban + et,
                              family = "lognormal",
                              data = diversions,  # This data won't actually be incorporated into the posterior! Just there to let the model run.
                              prior = priors, 
                              sample_prior = "only")
preds_from_prior <-  add_predicted_draws(diversions, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_irrig_prcp, y = .prediction)) + 
  geom_point(alpha = 0.4)
  
AF.logn.mix <- brm(Acre_feet ~ (1+scale_class1_urban|Name) + scale_class1_urban +scale_irrig_prcp+scale_irrig_temp+et+scale_Max_Fill,
                 data=diversions,
                 family = 'lognormal',
                 iter=2000,
                 prior = priors,
                 control=list(max_treedepth=20,
                              adapt_delta=0.99),
                 cores=getOption('mc.cores', parallel::detectCores()))
AF.logn.mix <- add_criterion(AF.logn.mix, criterion = 'loo', moment_match=TRUE)
save(AF.logn.mix, file='~/Desktop/diversion_models/Model_outputs/AF_logn_mix_new.Rdata')
pp_check(AF.logn.mix)
summary(AF.logn.mix)
mcmc_areas(AF.logn.mix, pars=c('b_scale_.class1_urban',
                             'b_scale_.irrig_prcp',
                             'b_scale_.irrig_temp',
                             'b_et',
                             'b_scale_.Max_Fill'), prob=0.9)
mcmc_areas(AF.logn.mix, pars=c('b_scale_.class1_urban'), prob=0.9)
conditional_effects(AF.logn.mix)

test_prior <- c(
  set_prior("normal(0,4)", class="Intercept"),
  set_prior("normal(0,1)", class="b", coef = "scale_.class1_urban"),
  set_prior("normal(0,1)", class = "b", coef = "scale_.irrig_prcp"),
  set_prior("normal(0,1)", class = "b", coef = "scale_.JuneAug_temp"),
  set_prior("normal(0,1)", class = "b", coef = "et"),
  set_prior("normal(0,1)", class = "b", coef = "scale_.Carryover")
)

prior_samples <- brm(Acre_feet ~ (1+scale_.class1_urban|Name) + scale_.class1_urban +scale_.irrig_prcp+scale_.JuneAug_temp+et+scale_.Max_Fill,
                     family = lognormal(),
                     data = diversions,  # This data won't actually be incorporated into the posterior! Just there to let the model run.
                     prior = test_prior, 
                     sample_prior = "only")

summary(prior_samples)
preds_from_prior <-  add_predicted_draws(data, prior_samples, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_.irrig_prcp, y = .prediction)) + 
  geom_point(alpha = 0.4) +
  ylim(0,1000000)
max(preds_from_prior$.prediction)
mean(preds_from_prior$.prediction)
median(preds_from_prior$.prediction)
min(preds_from_prior$.prediction)

AF.logn.mixJAtmp.testcar <- brm(Acre_feet ~ (1+scale_.class1_urban|Name) + scale_.class1_urban +scale_.irrig_prcp+scale_.JuneAug_temp+et+scale_.Carryover,
                   data=diversions,
                   family = 'lognormal',
                   iter=4000,
                   prior = test_prior,
                   control=list(max_treedepth=20,
                                adapt_delta=0.99),
                   cores=getOption('mc.cores', parallel::detectCores()))
summary(AF.logn.mixJAtmp.testcar)
AF.logn.mixJAtmp.testcar <- add_criterion(AF.logn.mixJAtmp.testcar, criterion = 'loo', moment_match=TRUE)
save(AF.logn.mixJAtmp.testcar, file='~/Desktop/diversion_models/Model_outputs/AF_logn_mixJAtmp_testcar.Rdata')
summary(AF.logn.mixJAtmp)
load('~/Desktop/diversion_models/Model_outputs/AF_logn_mixJAtmp.Rdata')
plot(AF.logn.mixJAtmp)
conditional_effects(AF.logn.mixJAtmp)

AF.logn.mixJAtmp.ng <- brm(Acre_feet ~ (1|Name) + scale_.class1_urban +scale_.irrig_prcp+scale_.JuneAug_temp+et+scale_.Max_Fill,
                        data=diversions,
                        family = 'lognormal',
                        iter=4000,
                        control=list(max_treedepth=20,
                                     adapt_delta=0.99),
                        save_pars = save_pars(all=TRUE),
                        cores=getOption('mc.cores', parallel::detectCores()))
summary(AF.logn.mixJAtmp.ng)
AF.logn.mixJAtmp.ng <- add_criterion(AF.logn.mixJAtmp.ng, criterion='loo', moment_match=TRUE)
save(AF.logn.mixJAtmp.ng, file='~/Desktop/diversion_models/Model_outputs/AF_logn_mixJAtmp_ng.Rdata')
conditional_effects(AF.logn.mixJAtmp.ng)
pdf(file='~/Desktop/diversion_models/figures/AF_ppcheck.pdf',
    width=6,
    height=6)
pp_check(AF.logn.mixJAtmp)+
  ylab('Density')+
  xlab('Annual Discharge (AF/yr)')+
  theme_bw()
dev.off()


## ---------------------------------- ##
## LENGTH OF IRRIGATION SEASON MODELS ##
## ---------------------------------- ##
hist(diversions$Range)
len_sn_urb <- brm(Range ~ (1+scale_.class1_urban|Name) + scale_.class1_urban,
                 data=diversions,
                 family = 'skew_normal',
                 iter=6000,
                 control=list(max_treedepth=20,
                              adapt_delta=0.99),
                 cores=getOption('mc.cores', parallel::detectCores()))
save(len_sn_urb, file='~/Desktop/diversion_models/Model_outputs/len_sn_urb.Rdata')
summary(len_sn_urb)
pp_check(len_sn_urb)
conditional_effects(len_sn_urb)
mcmc_areas(len_sn_urb, prob=0.9)

len.ST.urb.ng <- brm(Range ~ (1|Name) + scale_.class1_urban,
                  data=diversions,
                  family = 'student',
                  iter=8000,
                  control=list(max_treedepth=20,
                               adapt_delta=0.99),
                  save_pars = save_pars(all=TRUE),
                  cores=getOption('mc.cores', parallel::detectCores()))
summary(len.ST.urb.ng)
len.ST.urb.ng <- add_criterion(len.ST.urb.ng, criterion='loo', moment_match=TRUE)
save(len.ST.urb.ng, file='~/Desktop/diversion_models/Model_outputs/len_st_urb_ng.Rdata')



len_ST_urb <- brm(Range ~ (1+scale_.class1_urban|Name) + scale_.class1_urban,
                  data=diversions,
                  family = 'student',
                  iter=6000,
                  control=list(max_treedepth=20,
                               adapt_delta=0.99),
                  cores=getOption('mc.cores', parallel::detectCores()))
save(len_ST_urb, file='~/Desktop/diversion_models/Model_outputs/len_ST_urb.Rdata')
summary(len_ST_urb)
pp_check(len_ST_urb)
conditional_effects(len_ST_urb)
mcmc_areas(len_ST_urb, pars=c('b_scale_.class1_urban'), prob=0.9)

len.ST.clim.ir <- brm(Range ~ (1|Name)+scale_.irrig_prcp+scale_.irrig_temp+et+scale_.Max_Fill,
                   data=diversions,
                   family='student',
                   iter = 8000,
                   control=list(max_treedepth=20, 
                                adapt_delta=0.99),
                   cores=getOption('mc.cores', parallel::detectCores()))
save(len.ST.clim.ir, file='~/Desktop/diversion_models/Model_outputs/len_ST_clim_ir.Rdata')
pp_check(len.ST.clim.ir)
summary(len.ST.clim.ir)
conditional_effects(len.ST.clim.ir)

len.ST.clim.non2 <- brm(Range ~ (1|Name)+scale_.ant_prcp+scale_.LP_inflows+scale_.Max_Fill,
                      data=diversions,
                      family='student',
                      iter = 6000,
                      control=list(max_treedepth=20, 
                                   adapt_delta=0.99),
                      cores=getOption('mc.cores', parallel::detectCores()))
save(len.ST.clim.non2, file='~/Desktop/diversion_models/Model_outputs/len_ST_clim_non2.Rdata')
pp_check(len.ST.clim.non2)
summary(len.ST.clim.non2)
conditional_effects(len.ST.clim.non2)

len.ST.mix.ir <- brm(Range ~ (1+scale_.class1_urban|Name)+scale_.class1_urban+scale_.irrig_prcp+scale_.irrig_temp+et+scale_.Max_Fill,
                      data=diversions,
                      family='student',
                      iter = 8000,
                      control=list(max_treedepth=20, 
                                   adapt_delta=0.99),
                      cores=getOption('mc.cores', parallel::detectCores()))
save(len.ST.mix.ir, file='~/Desktop/diversion_models/Model_outputs/len_ST_mix_ir.Rdata')
pdf(file='~/Desktop/diversion_models/figures/len_ppcheck.pdf',
    width=6,
    height=6)
pp_check(len.ST.mix.ir)+
  ylab('Density')+
  xlab('Length of Irrigation Season (days)')+
  theme_bw()
dev.off()
summary(len.ST.mix.ir)
conditional_effects(len.ST.mix.ir)

len.ST.mix.ir.ng <- brm(Range ~ (1|Name)+scale_.class1_urban+scale_.irrig_prcp+scale_.irrig_temp+et+scale_.Max_Fill,
                     data=diversions,
                     family='student',
                     iter = 8000,
                     control=list(max_treedepth=20, 
                                  adapt_delta=0.99),
                     cores=getOption('mc.cores', parallel::detectCores()))
len.ST.mix.ir.ng <- add_criterion(len.ST.mix.ir.ng, criterion='loo')
save(len.ST.mix.ir.ng, file='~/Desktop/diversion_models/Model_outputs/len_ST_mix_ir_ng.Rdata')
summary(len.ST.mix.ir.ng)

len.ST.mix.mix <- brm(Range ~ (1+scale_.class1_urban|Name)+scale_.class1_urban+scale_.ant_prcp+scale_.irrig_temp+et+scale_.Max_Fill,
                      data=diversions,
                      family='student',
                      iter = 8000,
                      control=list(max_treedepth=20, 
                                   adapt_delta=0.99),
                      save_pars = save_pars(all=TRUE),
                      cores=getOption('mc.cores', parallel::detectCores()))
len.ST.mix.mix <- add_criterion(len.ST.mix.mix, criterion='loo')
save(len.ST.mix.mix, file='~/Desktop/diversion_models/Model_outputs/len_ST_mix_mix.Rdata')
summary(len.ST.mix.mix)

len.ST.mix.noet <- brm(Range ~ (1+scale_.class1_urban|Name)+scale_.class1_urban+scale_.irrig_prcp+scale_.irrig_temp+scale_.Max_Fill,
                      data=diversions,
                      family='student',
                      iter = 8000,
                      control=list(max_treedepth=20, 
                                   adapt_delta=0.99),
                      save_pars = save_pars(all=TRUE),
                      cores=getOption('mc.cores', parallel::detectCores()))
len.ST.mix.noet <- add_criterion(len.ST.mix.noet, criterion='loo', moment_match=TRUE)
save(len.ST.mix.noet, file='~/Desktop/diversion_models/Model_outputs/len_ST_mix_noet.Rdata')
summary(len.ST.mix.noet)

priors <- c(
  set_prior('student_t(2,180,4)', class = 'Intercept'),
  set_prior('normal(0,10)', class = 'sd'),
  set_prior('normal(0,10)', class = 'b', coef = 'scale_class1_urban'),
  set_prior('normal(0,10)', class = 'b', coef = 'scale_Mar_prcp'),
  set_prior('normal(0,10)', class = 'b', coef = 'scale_Max_Fill'),
  set_prior('normal(0,10)', class = 'b', coef = 'scale_irrig_temp')
  
)
prior_mod <- brm(Range ~ (1|Name)+scale_Mar_prcp +scale_irrig_temp+scale_Max_Fill + scale_class1_urban,
                                  family = "student",
                                  data = diversions,  # This data won't actually be incorporated into the posterior! Just there to let the model run.
                                  prior = priors, 
                                  sample_prior = "only")
preds_from_prior <-  add_predicted_draws(diversions, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_Mar_prcp, y = .prediction)) + 
  geom_point(alpha = 0.4)

len.mix <- brm(Range ~ (1|Name)+scale_Mar_prcp +scale_irrig_temp+scale_Max_Fill + scale_class1_urban,
               family = "student",
               data = diversions, 
               prior = priors, 
               iter = 2000,
               control=list(max_treedepth=20, 
                            adapt_delta=0.99),
               cores=getOption('mc.cores', parallel::detectCores()))
summary(len.mix)
mae(len.mix, diversions$Range)

## REMOVE GROUPS WITH LESS THAN 10 OBS ## 

filtered <- diversions %>%
  group_by(Name) %>%
  filter(n() > 10)

filtered <- data.frame(filtered)

hist(filtered$Range)
hist(filtered$Acre_feet, breaks=15)

len.ST.mix.ir.filt <- brm(Range ~ (1+scale_.class1_urban|Name)+scale_.class1_urban+scale_.irrig_prcp+scale_.irrig_temp+et+scale_.Max_Fill,
                     data=filtered,
                     family='student',
                     iter = 8000,
                     control=list(max_treedepth=20, 
                                  adapt_delta=0.99),
                     cores=getOption('mc.cores', parallel::detectCores()))
save(len.ST.mix.ir.filt, file='~/Desktop/diversion_models/Model_outputs/len_ST_mix_ir_filt.Rdata')
summary(len.ST.mix.ir.filt)

AF.logn.mix.filt <- brm(Acre_feet ~ (1+scale_.class1_urban|Name) + scale_.class1_urban +scale_.irrig_prcp+scale_.irrig_temp+et+scale_.Max_Fill,
                     data=filtered,
                     family = 'lognormal',
                     iter=8000,
                     control=list(max_treedepth=20,
                                  adapt_delta=0.99),
                     cores=getOption('mc.cores', parallel::detectCores()))
save(AF.logn.mix.filt, file='~/Desktop/diversion_models/Model_outputs/AF_logn_mix_filt.Rdata')
summary(AF.logn.mix.filt)
pp_check(AF.logn.mix.filt)
conditional_effects(AF.logn.mix.filt)

len.ST.mix.trial <- brm(Range ~ (1+scale_.class1_urban|Name)+scale_.class1_urban+scale_.irrig_prcp+scale_.irrig_temp+et+scale_.Max_Fill,
                          data=filtered,
                          family='student',
                          iter = 4000,
                          control=list(max_treedepth=20, 
                                       adapt_delta=0.99),
                          cores=getOption('mc.cores', parallel::detectCores()))
save(len.ST.mix.trial, file ='~/Desktop/diversion_models/Model_outputs/len_ST_mix_trial.Rdata')
summary(len.ST.mix.trial)
conditional_effects(len.ST.mix.trial)
pp_check(len.ST.mix.trial)

## log transform data ## 

diversions$log_trans <- log(diversions$Acre_feet)
hist(diversions$log_trans)

prior_test <- test_prior <- c(
  set_prior("normal(8,0.3)", class="Intercept"),
  set_prior("normal(0,0.75)", class = "b", coef = "scale_.irrig_prcp"),
  set_prior("normal(0,0.75)", class = "b", coef = "scale_.JuneAug_temp"),
  set_prior("normal(0,0.75)", class = "b", coef = "et"),
  set_prior("normal(0,0.75)", class = "b", coef = "scale_.Carryover"),
  set_prior("normal(0,0.75)", class = "b", coef = "scale_.irrig_temp"),
  set_prior("normal(0,0.75)", class = "b", coef = "scale_.Max_Fill"),
  set_prior("normal(2,1)", class = "sd")
)

prior_mod <- prior_samples <- brm(log_trans ~ (1|Name)+scale_.irrig_prcp+scale_.JuneAug_temp+scale_.irrig_temp+et+scale_.Max_Fill+scale_.Carryover,
                                 family = "normal",
                                 data = diversions,  # This data won't actually be incorporated into the posterior! Just there to let the model run.
                                 prior = prior_test, 
                                 sample_prior = "only")
preds_from_prior <-  add_predicted_draws(diversions, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_.irrig_prcp, y = .prediction)) + 
  geom_point(alpha = 0.4)
max(preds_from_prior$.prediction)
mean(preds_from_prior$.prediction)
median(preds_from_prior$.prediction)
min(preds_from_prior$.prediction)
min(diversions$log_trans)
max(diversions$log_trans)
mean(diversions$log_trans)
median(diversions$log_trans)

  
mod.climate.lt <- brm(log_trans ~ (1|Name)+scale_.irrig_prcp+scale_.JuneAug_temp+scale_.irrig_temp+et+scale_.Max_Fill+scale_.Carryover,
                   data=diversions,
                   family='normal',
                   iter = 4000,
                   prior = prior_test,
                   control=list(max_treedepth=20, 
                                adapt_delta=0.999),
                   cores=getOption('mc.cores', parallel::detectCores()))
plot(mod.climate.lt)
summary(mod.climate.lt)
pp_check(mod.climate.lt, ndraws=100)
prior_summary(mod.climate.lt)

prior_test <- test_prior <- c(
  set_prior("normal(8,0.3)", class="Intercept"),
  set_prior("normal(0,0.75)", class = "b", coef = "scale_.irrig_prcp"),
  set_prior("normal(0,0.75)", class = "b", coef = "et"),
  set_prior("normal(0,0.75)", class = "b", coef = "scale_.irrig_temp"),
  set_prior("normal(2,1)", class = "sd"), 
  set_prior("normal(0,0.75)", class = "b", coef = "scale_.class1_urban")
)
prior_mod <-  brm(log_trans ~ (1 + scale_.class1_urban | Name) + scale_.class1_urban + scale_.irrig_prcp+scale_.JuneAug_temp+scale_.irrig_temp+et,
                                  family = "normal",
                                  data = diversions,  # This data won't actually be incorporated into the posterior! Just there to let the model run.
                                  prior = prior_test, 
                                  sample_prior = "only")
preds_from_prior <-  add_predicted_draws(diversions, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_.irrig_prcp, y = .prediction)) + 
  geom_point(alpha = 0.4)
max(preds_from_prior$.prediction)
mean(preds_from_prior$.prediction)
median(preds_from_prior$.prediction)
min(preds_from_prior$.prediction)
min(diversions$log_trans)
max(diversions$log_trans)
mean(diversions$log_trans)
median(diversions$log_trans)

mod.mix.lt <- brm(log_trans ~ (1 + scale_.class1_urban | Name) + scale_.class1_urban + scale_.irrig_prcp+scale_.irrig_temp+et,
                  data=diversions,
                  family='normal',
                  iter = 4000,
                  control=list(max_treedepth=20, 
                               adapt_delta=0.999),
                  cores=getOption('mc.cores', parallel::detectCores()))
plot(mod.mix.lt)
summary(mod.mix.lt)
save(mod.mix.lt, file = '~/Desktop/diversion_models/Model_outputs/mod_mix_lt.Rdata')

prior_test <- test_prior <- c(
  set_prior("normal(8,0.3)", class="Intercept"),
  set_prior("normal(2,1)", class = "sd"), 
  set_prior("normal(0,0.75)", class = "b", coef = "scale_.class1_urban")
)

prior_mod <- prior_samples <- brm(log_trans ~ (1+scale_.class1_urban| Name) + scale_.class1_urban,
                                  family = "normal",
                                  data = diversions,  # This data won't actually be incorporated into the posterior! Just there to let the model run.
                                  prior = prior_test, 
                                  sample_prior = "only")
preds_from_prior <-  add_predicted_draws(diversions, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_.irrig_prcp, y = .prediction)) + 
  geom_point(alpha = 0.4)
max(preds_from_prior$.prediction)
mean(preds_from_prior$.prediction)
median(preds_from_prior$.prediction)
min(preds_from_prior$.prediction)
min(diversions$log_trans)
max(diversions$log_trans)
mean(diversions$log_trans)
median(diversions$log_trans)

mod.urb.lt <- brm(log_trans ~ (1| scale_.class1_urban + Name) + scale_.class1_urban,
                  data=diversions,
                  family='normal',
                  iter = 4000,
                  prior = prior_test,
                  control=list(max_treedepth=20, 
                               adapt_delta=0.999),
                  cores=getOption('mc.cores', parallel::detectCores()))
summary(mod.urb.lt)
pp_check(mod.urb.lt, ndraws=50)

prior_test <- test_prior <- c(
  set_prior("normal(8,0.3)", class="Intercept"),
  set_prior("normal(2,1)", class = "sd"), 
  set_prior("normal(0,0.6)", class = "b", coef = "scale_.class1_urban"),
  set_prior("normal(0,0.6)", class = "b", coef = "scale_.irrig_prcp"),
  set_prior("normal(0,0.6)", class = "b", coef = "et"),
  set_prior("normal(0,0.6)", class = "b", coef = "scale_.irrig_temp"),
  set_prior("normal(0,0.6)", class = "b", coef = "scale_.Max_Fill")
)

prior_mod <- prior_samples <- brm(log_trans ~ (1| scale_.class1_urban + Name) + scale_.class1_urban+scale_.irrig_prcp+scale_.JuneAug_temp+et+scale_.Max_Fill,
                                  family = "normal",
                                  data = diversions,  # This data won't actually be incorporated into the posterior! Just there to let the model run.
                                  prior = prior_test, 
                                  sample_prior = "only")
preds_from_prior <-  add_predicted_draws(diversions, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_.irrig_prcp, y = .prediction)) + 
  geom_point(alpha = 0.4)
max(preds_from_prior$.prediction)
min(preds_from_prior$.prediction)
mean(preds_from_prior$.prediction)
median(preds_from_prior$.prediction)

min(diversions$log_trans)
max(diversions$log_trans)
mean(diversions$log_trans)
median(diversions$log_trans)

mod.mixJA.lt <- brm(log_trans ~ (1 + scale_.class1_urban|Name) + scale_.class1_urban+scale_.irrig_prcp+scale_.JuneAug_temp+et+scale_.Max_Fill,
                   data=diversions,
                   family='normal',
                   iter = 4000,
                   prior = prior_test,
                   control=list(max_treedepth=20, 
                                adapt_delta=0.999),
                   cores=getOption('mc.cores', parallel::detectCores()))
summary(mod.mixJA.lt)

mod.mixJA.lt <- brm(log_trans ~ (1| scale_.class1_urban + Name) + scale_.class1_urban+scale_.irrig_prcp+scale_.irrig_temp+et+scale_.Max_Fill,
                    data=diversions,
                    family='normal',
                    iter = 4000,
                    prior = prior_test,
                    control=list(max_treedepth=20, 
                                 adapt_delta=0.999),
                    cores=getOption('mc.cores', parallel::detectCores()))
summary(mod.mixJA.lt)

## ----------------------------------- 
## TIMING COMPONENT: START DAY OF YEAR 

# Look at distribution for family
ggplot(data=data)+
  aes(x=StartDayofYear)+
  geom_histogram(color='black')+
  ylab('Count')+
  xlab('Start Day of Year')+
  theme_bw() +
  theme(text = element_text(size = 20))
ggsave('~/Desktop/diversion_models/Figures/start_hist.jpg', 
       width = 4,
       height = 4,
       units = 'in')

# Model 1: Urban as only effect, varying

priors <- c(
  set_prior('student_t(2,100,5)', class = 'Intercept'),
  set_prior('normal(0,5)', class = 'sd'),
  set_prior('normal(0,10)', class = 'b', coef = 'scale_class1_urban')
)

prior_mod <- prior_samples <- brm(StartDayofYear ~ (1 | Name) + scale_class1_urban,
                                  family = "student",
                                  data = diversions,  # This data won't actually be incorporated into the posterior! Just there to let the model run.
                                  prior = priors, 
                                  sample_prior = "only")
preds_from_prior <-  add_predicted_draws(diversions, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_class1_urban, y = .prediction)) + 
  geom_point(alpha = 0.4)

start.urb.ng <- brm(StartDayofYear ~ (1 | Name) + scale_class1_urban,
                 family = 'student',
                 data = diversions,
                 prior = priors,
                 iter = 2000,
                 control=list(max_treedepth=20, 
                              adapt_delta=0.999),
                 cores=getOption('mc.cores', parallel::detectCores()))
save(start.urb.ng, file='~/Desktop/diversion_models/Model_outputs/start_urb_ng.Rdata')
summary(start.urb.ng)
pp_check(start.urb.ng)
conditional_effects(start.urb.ng)
ranef(start.urb)
mae(start.urb.ng, diversions$StartDayofYear)

priors <- c(
  set_prior('student_t(2,100,4)', class = 'Intercept'),
  set_prior('normal(9,3)', class = 'sd'),
  set_prior('normal(0,10)', class = 'b', coef = 'scale_class1_urban'),
  set_prior('normal(0,10)', class = 'b', coef = 'Mar_et'),
  set_prior('normal(0,10)', class = 'b', coef = 'scale_Mar_tmp'),
  set_prior('normal(0,10)', class = 'b', coef = 'scale_Mar_prcp')
)

prior_mod <- prior_samples <- brm(StartDayofYear ~ (1 + scale_class1_urban | Name) + scale_class1_urban + Mar_et + scale_Mar_tmp + scale_Mar_prcp,
                                  family = "student",
                                  data = diversions,  # This data won't actually be incorporated into the posterior! Just there to let the model run.
                                  prior = priors, 
                                  sample_prior = "only")
preds_from_prior <-  add_predicted_draws(diversions, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_class1_urban, y = .prediction)) + 
  geom_point(alpha = 0.4)

start.mix <- brm(StartDayofYear ~ (1 | Name) + scale_class1_urban + Mar_et + scale_Mar_tmp + scale_Mar_prcp,
                 family = 'student',
                 data = diversions,
                 prior = priors,
                 iter = 2000, 
                 control=list(max_treedepth=20, 
                              adapt_delta=0.999),
                 cores=getOption('mc.cores', parallel::detectCores()))
plot(start.mix)
summary(start.mix)
pp_check(start.mix)
mae(start.mix, diversions$StartDayofYear)
conditional_effects(start.mix)

priors <- c(
  set_prior('student_t(2,100,4)', class = 'Intercept'),
  set_prior('normal(9,3)', class = 'sd'),
  set_prior('normal(0,10)', class = 'b', coef = 'Mar_et'),
  set_prior('normal(0,10)', class = 'b', coef = 'scale_Mar_tmp'),
  set_prior('normal(0,10)', class = 'b', coef = 'scale_Mar_prcp')
)
start.clim <- brm(StartDayofYear ~ (1 | Name) + Mar_et + scale_Mar_tmp + scale_Mar_prcp,
                 family = 'student',
                 data = diversions,
                 prior = priors,
                 iter = 2000, 
                 control=list(max_treedepth=20, 
                              adapt_delta=0.999),
                 cores=getOption('mc.cores', parallel::detectCores()))

summary(start.clim)
plot(start.clim)
conditional_effects(start.clim)

priors <- c(
  set_prior('student_t(2,100,4)', class = 'Intercept'),
  set_prior('normal(9,3)', class = 'sd'),
  set_prior('normal(0,10)', class = 'b', coef = 'scale_class2_crops'),
  set_prior('normal(0,10)', class = 'b', coef = 'Mar_et'),
  set_prior('normal(0,10)', class = 'b', coef = 'scale_Mar_tmp'),
  set_prior('normal(0,10)', class = 'b', coef = 'scale_Mar_prcp')
)
start.mix.crops <- brm(StartDayofYear ~ (1 | Name) + scale_class2_crops + Mar_et + scale_Mar_tmp + scale_Mar_prcp,
                 family = 'student',
                 data = diversions,
                 prior = priors,
                 iter = 2000, 
                 control=list(max_treedepth=20, 
                              adapt_delta=0.999),
                 cores=getOption('mc.cores', parallel::detectCores()))
summary(start.mix.crops)
pp_check(start.mix.crops)+
  xlim(50,200)
conditional_effects(start.mix.crops)


mod.stor <- brm(Acre_feet ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_AF_used + scale_irrig_prcp + scale_irrig_temp + et,
                iter = 2000,
                family = 'lognormal',
                data = diversions,
                control=list(max_treedepth=20, 
                             adapt_delta=0.999),
                cores=getOption('mc.cores', parallel::detectCores()))
summary(mod.stor)
mae(mod.stor, diversions$Acre_feet)




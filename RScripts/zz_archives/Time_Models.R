## Time Models that have been explored
## Bridget Bittmann
## Nov. 19, 2022

#### CREATING MODELS FOR TIMING ####
# Looking at length of irrigation season

#### Urban Model ####

priors <- c(
  set_prior('normal (170,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class = 'sd'),
  set_prior('normal(0,8)', class = 'b', coef = 'scale_class1_urban'))

prior_mod <- brm(Range ~ (1 + scale_class1_urban | Name) + scale_class1_urban,
                 family = "student",
                 data = diversions,  # This data won't actually be incorporated into the posterior! Just there to let the model run.
                 prior = priors, 
                 sample_prior = "only")
preds_from_prior <-  add_predicted_draws(diversions, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_class1_urban, y = .prediction)) + 
  geom_point(alpha = 0.4) 

len.urb <- brm(Range ~ (1 + scale_class1_urban | Name) + scale_class1_urban,
               data = diversions,
               iter = 4000,
               family = student(),
               control=list(max_treedepth=20,
                            adapt_delta=0.99),
               cores=getOption('mc.cores', parallel::detectCores()))

mae(len.urb, diversions$Range)
summary(len.urb)
plot(len.urb)
len.urb <- add_criterion(len.urb, criterion = 'loo')
save(len.urb, file = '~/Desktop/diversion_models/Model_outputs/len_urb_1018.Rdata')

### Climate model ####

priors <- c(
  set_prior('normal (170,10)', class = 'Intercept'),
  set_prior('normal(10,5)', class = 'sd'),
  set_prior('normal(0,5)', class = 'b', coef = 'Mar_et'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_irrig_temp'),
  set_prior('normal(0,5)', class = 'b', coef = 'scale_Mar_prcp')
)

len.climate <- brm(Range ~ (1 | Name) + scale_Mar_prcp + Mar_et + scale_irrig_temp,
                   data = diversions,
                   iter = 4000,
                   family = student(),
                   control=list(max_treedepth=20,
                                adapt_delta=0.99),
                   cores=getOption('mc.cores', parallel::detectCores()))
len.climate <- add_criterion(len.climate, criterion = 'loo')
summary(len.climate)
pp_check(len.climate) +
  xlim(50,250)
plot(len.climate)
mae(len.climate, diversions$Range)
save(len.climate, file = '~/Desktop/diversion_models/Model_outputs/len_climate_1018.Rdata')

## Climate + urban w/ group model ####
priors <- c(
  set_prior('normal (170,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class = 'sd'),
  set_prior('normal(0,8)', class = 'b', coef = 'scale_class1_urban'),
  set_prior('normal(0,8)', class = 'b', coef = 'Mar_et'),
  set_prior('normal(0,8)', class = 'b', coef = 'scale_irrig_temp'),
  set_prior('normal(0,8)', class = 'b', coef = 'scale_Mar_prcp')
)

len.mix <- brm(Range ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_Mar_prcp + Mar_et + scale_irrig_temp,
               data = diversions,
               iter = 4000,
               family = student(),
               control=list(max_treedepth=20,
                            adapt_delta=0.99),
               cores=getOption('mc.cores', parallel::detectCores()))

summary(len.mix)
plot(len.mix)
mae(len.mix, diversions$Range)
len.mix <- add_criterion(len.mix, criterion = 'loo')
save(len.mix, file = '~/Desktop/diversion_models/Model_outputs/len_mix_1018.Rdata')

#### CLimate + urban, no groups, model ####

priors <- c(
  set_prior('normal (170,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class = 'sd'),
  set_prior('normal(0,8)', class = 'b', coef = 'scale_class1_urban'),
  set_prior('normal(0,8)', class = 'b', coef = 'Mar_et'),
  set_prior('normal(0,8)', class = 'b', coef = 'scale_irrig_temp'),
  set_prior('normal(0,8)', class = 'b', coef = 'scale_Mar_prcp')
)

len.mix.ng <- brm(Range ~ (1 | Name) + scale_class1_urban + scale_Mar_prcp + Mar_et + scale_irrig_temp,
                  data = diversions,
                  iter = 4000,
                  family = student(),
                  control=list(max_treedepth=20,
                               adapt_delta=0.99),
                  cores=getOption('mc.cores', parallel::detectCores()))

summary(len.mix.ng)
plot(len.mix.ng)
mae(len.mix.ng, diversions$Range)
len.mix.ng <- add_criterion(len.mix.ng, criterion = 'loo')
save(len.mix.ng, file = '~/Desktop/diversion_models/Model_outputs/len_mix_1018.Rdata')

#### Urban, climate, and reservoir model w/ group ####
priors <- c(
  set_prior('normal (170,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class = 'sd'),
  set_prior('normal(0,8)', class = 'b', coef = 'scale_class1_urban'),
  set_prior('normal(0,8)', class = 'b', coef = 'Mar_et'),
  set_prior('normal(0,8)', class = 'b', coef = 'scale_irrig_temp'),
  set_prior('normal(0,8)', class = 'b', coef = 'scale_Mar_prcp'),
  set_prior('normal(0,8)', class = 'b', coef = 'scale_AF_used')
)

prior_mod <- brm(Range ~ (1 + scale_class1_urban | Name) + scale_class1_urban + Mar_et + scale_irrig_temp + scale_Mar_prcp + scale_AF_used,
                 family = "student",
                 data = diversions,  # This data won't actually be incorporated into the posterior! Just there to let the model run.
                 prior = priors, 
                 sample_prior = "only")
preds_from_prior <-  add_predicted_draws(diversions, prior_mod, ndraws = 10)
ggplot(preds_from_prior,
       aes(x = scale_class1_urban, y = .prediction)) + 
  geom_point(alpha = 0.4) 

len.stor <- brm(Range ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_Mar_prcp + Mar_et + scale_irrig_temp + scale_AF_used,
                data = diversions,
                iter = 4000,
                family = student(),
                control=list(max_treedepth=20,
                             adapt_delta=0.99),
                cores=getOption('mc.cores', parallel::detectCores()))

plot(len.stor)
summary(len.stor)
pp_check(len.stor) +
  xlim(50,300) +
  ylab('Density') +
  xlab('Length of Irrigation Season')
conditional_effects(len.stor)
mae(len.stor, diversions$Range)
len.stor <- add_criterion(len.stor, criterion = 'loo')
save(len.stor, file  = '~/Desktop/diversion_models/Model_outputs/len_stor_1018.Rdata')

#### Urban, climate, and reservoir model no group ####
priors <- c(
  set_prior('normal (170,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class = 'sd'),
  set_prior('normal(0,8)', class = 'b', coef = 'scale_class1_urban'),
  set_prior('normal(0,8)', class = 'b', coef = 'Mar_et'),
  set_prior('normal(0,8)', class = 'b', coef = 'scale_irrig_temp'),
  set_prior('normal(0,8)', class = 'b', coef = 'scale_Mar_prcp'),
  set_prior('normal(0,8)', class = 'b', coef = 'scale_AF_used')
)


len.stor.ng <- brm(Range ~ (1 | Name) + scale_class1_urban + scale_Mar_prcp + Mar_et + scale_irrig_temp + scale_AF_used,
                   data = diversions,
                   iter = 4000,
                   family = student(),
                   control=list(max_treedepth=20,
                                adapt_delta=0.99),
                   cores=getOption('mc.cores', parallel::detectCores()))

summary(len.stor.ng)
mae(len.stor.ng, diversions$Range)
len.stor.ng <- add_criterion(len.stor.ng, criterion='loo')
save(len.stor.ng, file = '~/Desktop/diversion_models/Model_outputs/len_stor_urb_1018.Rdata')


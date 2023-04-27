## Evaluates Model Fit ##
## Author: Bridget Bittmann
## Created: 07/15/2022
## Editted: 07/15/2022

install.packages('loo')
library(loo)
library(brms)
library(tidyverse)

mae <- function(model, data_compare){
  yhat <- posterior_predict(model)
  resid <- sweep(yhat, 
                 2,
                 data_compare,
                 FUN="-")
  return(median(abs(resid)))
}


## First calculate the mean, med, and std of whole dataset

diversions <- read.csv('~/Desktop/diversion_models/Data.Inputs/mixed_model_input.csv')

mean(diversions$Acre_feet)
median(diversions$Acre_feet)
sd(diversions$Acre_feet)

mean(diversions$Range)
median(diversions$Range)
sd(diversions$Range)

## Import models ##

load('~/Desktop/diversion_models/Model_outputs/AF_logn_clim_ir.Rdata')
#AF_logn_clim_ir <- add_criterion(AF_logn_clim_ir, criterion='loo', moment_match=TRUE)
#save(AF_logn_clim_ir, file='~/Desktop/diversion_models/Model_outputs/AF_logn_clim_ir.Rdata')
load('~/Desktop/diversion_models/Model_outputs/AF_logn_mix.Rdata')
#AF.logn.mix <- add_criterion(AF.logn.mix, criterion='loo', moment_match=TRUE)
#save(AF.logn.mix, file='~/Desktop/diversion_models/Model_outputs/AF_logn_mix.Rdata')
load('~/Desktop/diversion_models/Model_outputs/AF_logn_urb.Rdata')
#AF_logn_urb <- add_criterion(AF_logn_urb, criterion='loo', moment_match=TRUE)
#save(AF_logn_urb, file='~/Desktop/diversion_models/Model_outputs/AF_logn_urb.Rdata')
load('~/Desktop/diversion_models/Model_outputs/AF_logn_mix_filt.Rdata')
load('~/Desktop/diversion_models/Model_outputs/AF_logn_mixJAtmp.Rdata')
#AF.logn.mixJAtmp <- add_criterion(AF.logn.mixJAtmp, criterion='loo', moment_match=TRUE)
#save(AF.logn.mixJAtmp, file='~/Desktop/diversion_models/Model_outputs/AF_logn_mixJAtmp.Rdata')
load(file='~/Desktop/diversion_models/Model_outputs/mod_mix_offset.Rdata')
mod.mix.off <- add_criterion(mod.mix.off, criterion='loo')

## TIMING MODELS
load('~/Desktop/diversion_models/Model_outputs/len_sn_urb.Rdata')

load('~/Desktop/diversion_models/Model_outputs/len_ST_urb.Rdata')
#len_ST_urb <- add_criterion(len_ST_urb, criterion='loo')
#save(len_ST_urb, file='~/Desktop/diversion_models/Model_outputs/len_ST_urb.Rdata')
load('~/Desktop/diversion_models/Model_outputs/len_ST_clim_ir.Rdata')
#len.ST.clim.ir <- add_criterion(len.ST.clim.ir, criterion='loo')
#save(len.ST.clim.ir, file='~/Desktop/diversion_models/Model_outputs/len_ST_clim_ir.Rdata')
load('~/Desktop/diversion_models/Model_outputs/len_ST_mix_ir.Rdata')
#len.ST.mix.ir<- add_criterion(len.ST.mix.ir, criterion='loo')
#save(len.ST.mix.ir, file='~/Desktop/diversion_models/Model_outputs/len_ST_mix_ir.Rdata')

## ----------------------- ##
## Calculate LOOIC and MAE ##
## ----------------------- ##

## VOLUME MODELS ##
## ------------ ## 


loo_compare(AF.logn.mix, AF.logn.mixJAtmp, mod.mix.off)

mae(AF_logn_clim_ir, diversions$Acre_feet)
mae(AF_logn_urb, diversions$Acre_feet)
mae(AF.logn.mix, diversions$Acre_feet)
mae(AF.logn.mix.filt, filtered$Acre_feet)
mae(AF.logn.mixJAtmp, diversions$Acre_feet)
mae(AF.logn.mixJAtmp.ng, diversions$Acre_feet)
mae(mod.mix.off, diversions$Acre_feet)

## TIMING MODELS ##
## ------------- ##

loo(len_ST_urb, len.ST.clim.ir, len.ST.mix.ir, len.ST.mix.mix, len.ST.urb.ng, len.ST.mix.ir.ng)

mae(len_sn_urb, diversions$Range)
mae(len_ST_urb, diversions$Range)
mae(len.ST.clim.ir, diversions$Range)
mae(len.ST.mix.ir, diversions$Range)
mae(len.ST.urb.ng, diversions$Range)
mae(len.ST.mix.ir.ng, diversions$Range)
mae(len.ST.mix.mix, diversions$Range)


modelsummary(AF.logn.mixJAtmp)

post <- posterior_samples(AF.logn.mix)

ggplot(aes(x=b_scale_.irrig_temp, y = b_scale_.irrig_prcp), data=post) +
  geom_point(alpha=1/2, color='firebrick4') +
  theme_bw() +
  xlab('Irrigation season temp') +
  ylab('Irrigation season prcp')

ggplot(aes(x=b_scale_.irrig_temp, y = b_scale_.class1_urban), data=post) +
  geom_point(alpha=1/2, color='firebrick4') +
  theme_bw() +
  xlab('Irrigation season temp') +
  ylab('Urban proportion')

ggplot(aes(x=b_et, y = b_scale_.class1_urban), data=post) +
  geom_point(alpha=1/2, color='firebrick4') +
  theme_bw() +
  xlab('ET') +
  ylab('Urban proportion')





## Looking at the diversion data through time and space ##

# The purpose of this script it examine correlations through space and examine the residuals in models
# that have already been run.

# Author: Bridget Bittmann
# Date created: 01/26/2023

# Import packages 
library(brms) # model read in
library(tidybayes) # pulling posterior information from model 
library(bayesplot) # Plotting model information
library(dplyr)
library(tidyverse)
library(readr)
library(tibble)
library(flexmix)
library(modelr)
library(Kendall) # Mann Kendall test
library(ggplot2) # Plotting
library(ggpubr) # Plotting

# Import the diversion data
div <- read.csv('~/Desktop/diversion_models/Data.Inputs/mixed_model_input.csv')
div <- read.csv('~/Desktop/diversion_models/Data.Inputs/input_full_013023.csv')
div <- div[!duplicated(div[c('Name', 'Year')]),] #remove duplicates
div$lt <- log(div$Acre_feet)

# Now look at the variables through time as a whole

averages <- div %>%
  group_by(Year) %>%
  summarise(avg.et = mean(et),
            avg.urb = mean(class1_urban),
            avg.prcp = mean(irrig_prcp),
            avg.tmp = mean(irrig_temp),
            avg.stor = mean(AF_used),
            avg.div = mean(Acre_feet))

names <- colnames(averages)

# ET through time
MannKendall(averages$avg.et) # not significant

ggplot(data = averages, aes(y= avg.et, x = Year)) +
  geom_point() +
  stat_smooth(method = 'lm', color = 'black') + 
  ggtitle('ET through time') +
  ylab('Avg. ET (m)') +
  theme_bw() +
  theme(text = element_text(size = 15))

ggplot(data = div, aes(y= et, x = Year)) +
  geom_point() +
  stat_smooth(method = 'lm', color = 'black') + 
  ggtitle('ET through time') +
  ylab('Avg. ET (m)') +
  theme_bw() +
  theme(text = element_text(size = 15))

# Urban through time
MannKendall(averages$avg.urb) # significant

ggplot(data = averages, aes(y= avg.urb, x = Year)) +
  geom_point() +
  stat_smooth(method = 'lm', color = 'black') + 
  ggtitle('Urban % through time') +
  ylab('Urban Percent') +
  theme_bw() +
  theme(text = element_text(size = 15))

ggplot(data = div, aes(y= class1_urban, x = Year)) +
  geom_point() +
  stat_smooth(method = 'lm', color = 'black') + 
  ggtitle('Urban % through time') +
  ylab('Urban Percent') +
  theme_bw() +
  theme(text = element_text(size = 15))

# Precip through time
MannKendall(averages$avg.prcp) # significant

ggplot(data = averages, aes(y= avg.prcp, x = Year)) +
  geom_point() +
  stat_smooth(method = 'lm', color = 'black') + 
  ggtitle('Precip. through time') +
  ylab('Precip (mm)') +
  theme_bw() +
  theme(text = element_text(size = 15))

ggplot(data = div, aes(y= irrig_prcp, x = Year)) +
  geom_point() +
  stat_smooth(method = 'lm', color = 'black') + 
  ggtitle('Precip. through time') +
  ylab('Precip (mm)') +
  theme_bw() +
  theme(text = element_text(size = 15))

# Temp through time
MannKendall(averages$avg.tmp) # not significant

ggplot(data = averages, aes(y= avg.tmp, x = Year)) +
  geom_point() +
  stat_smooth(method = 'lm', color = 'black') + 
  ggtitle('Temp. through time') +
  ylab('Temp (C)') +
  theme_bw() +
  theme(text = element_text(size = 15))

ggplot(data = div, aes(y= irrig_temp, x = Year)) +
  geom_point() +
  stat_smooth(method = 'lm', color = 'black') + 
  ggtitle('Temp. through time') +
  ylab('Temp (C)') +
  theme_bw() +
  theme(text = element_text(size = 15))

# Diversions through time
MannKendall(averages$avg.div)

ggplot(data = averages, aes(y= avg.div, x = Year)) +
  geom_point() +
  stat_smooth(method = 'lm', color = 'black') + 
  ggtitle('Diversions through time') +
  ylab('Volume (Acre-feet') +
  theme_bw() +
  theme(text = element_text(size = 15))

ggplot(data = div, aes(y= Acre_feet, x = Year)) +
  geom_point() +
  stat_smooth(method = 'lm', color = 'black') + 
  ggtitle('Diversions through time') +
  ylab('Volume (Acre-feet') +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  coord_cartesian(ylim = c(0,50000))

# Storage water use through time
MannKendall(averages$avg.stor) # not significant

ggplot(data = averages, aes(y= avg.stor, x = Year)) +
  geom_point() +
  stat_smooth(method = 'lm', color = 'black') + 
  ggtitle('Storage use through time') +
  ylab('Storage Use (AF)') +
  theme_bw() +
  theme(text = element_text(size = 15))

ggplot(data = div, aes(y= AF_used, x = Year)) +
  geom_point() +
  stat_smooth(method = 'lm', color = 'black') + 
  ggtitle('Storage use through time') +
  ylab('Storage Use (AF)') +
  theme_bw() +
  theme(text = element_text(size = 15))


## Examining pairs plots for models

full.arma <- readRDS('~/Desktop/diversion_models/borah_out/AF-full-arma.RDS')
pair_full <- pairs(full.arma,
                   pars = c('b_scale_class1_urban',
                            'b_scale_irrig_prcp',
                            'b_scale_irrig_temp',
                            'b_sacle_AF_used',
                            'b_et'))
ggsave('~/Desktop/diversion_models/Figures/pairs_noauto.png', plot = pair_full,
       height = 12,
       width = 12)

prior_summary(full.arma)
res <- residuals(full.arma)
res <- data.frame(res)
res$Name <- div$Name
res$Year <- div$Year


noauto <- readRDS('~/Desktop/diversion_models/borah_out/lt-mix-noauto.RDS')
pair <- pairs(noauto, 
      pars = c('b_scale_class1_urban',
               'b_scale_irrig_prcp',
               'b_scale_irrig_temp',
               'b_sacle_AF_used',
               'b_et'))
ggsave('~/Desktop/diversion_models/Figures/pairs_noauto.png', plot = pair,
       height = 12,
       width = 12)

res <- residuals(noauto, summary = FALSE)
res <- data.frame(res)
res$Name <- div$Name
res$Year <- div$Year

ggplot(data = res, aes(x = 1:1906, y = Estimate)) +
  geom_point()


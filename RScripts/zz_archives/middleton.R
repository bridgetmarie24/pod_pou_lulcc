# Models for above and below Middleton #

## By: Bridget Bittmann
## Date created: 01/20/2023

# Import packages 
library(brms)
library(tidyverse)
library(tidybayes)
library(plyr)
library(dplyr)
library(readr)
library(tibble)
library(flexmix)
library(modelr)
library(loo)
library(tidyr)

##### Import the data ######

diversions <- read.csv('~/scratch/middleton/data/mixed_model_input.csv')
diversions <- diversions[!duplicated(diversions[c('Name', 'Year')]),] #remove duplicates
diversions$lt <- log(diversions$Acre_feet)
diversions$nat_flow <- diversions$Acre_feet - diversions$AF_used
diversions$rate <- diversions$Acre_feet / diversions$Size_acres

# Split data by points upstream and downstream of Middleton

north <- c("Barber pumps",
           "Eagle Island State Park",
           "Boise City Parks",
           "Ballentyne Canal",
           "Boise City Canal",
           "Boise Valley Canal",
           "Bubb Canal",
           "Caldwell Highline Canal",
           "Canyon County Canal",
           "Capitol View Canal",
           "Conway-Hamming Canal",
           "Ester Simplot",
           "Eureka No1 Canal",
           "Fairview Acres",
           "Farmers Union Canal",
           "Graham-Gilbert Canal",
           "Hart-Davis Canal",
           "Lemp Canal",
           "Mace-Mace Canal",
           "Mace-Catlin Canal",
           "Middleton Canal",
           "New Dry Creek Canal",
           "New Union Canal",
           "New York Canal",
           "Penitentiary Canal",
           "Phyllis Canal",
           "Quinns Pond",
           "Ridenbaugh Canal",
           "River Run",
           "Riverside Village",
           "Rossi Mill and Meeves Canals",
           "Settlers Canal",
           "Seven Suckers Canal",
           "Shakespeare",
           "Suez",
           "Surprise Valley and Micron",
           "Thomas Aiken Canal",
           "Thurman Mill Canal",
           "Warm Springs Canal"
           )
print(length(north))

north_data <- subset(diversions, Name %in% north)
south_data <- subset(diversions, !(Name %in% north))
names_south <- unique(south_data$Name)
print(names_south)

priors <- c(
  set_prior('normal(0,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_class1_urban'),
  set_prior('gamma(1,1)', class = 'sd'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_irrig_prcp'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_irrig_temp'),
  set_prior('normal(0,1)', class = 'b', coef = 'et'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_AF_used')
)

north_mod <- brm(Acre_feet ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + et + scale_AF_used,
                 data = north_data,
                 family = 'lognormal',
                 prior = priors,
                 iter = 4000,
                 control = list(max_treedepth = 20,
                                adapt_delta = 0.999),
                 cores = getOption('mc.cores', parallel::detectCores()))
print('North Model')
print(summary(north_mod))
saveRDS(north_mod, file = '~/scratch/middleton/model-out/north-mod.RDS')

south_mod <- brm(Acre_feet ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + et + scale_AF_used,
                 data = south_data,
                 family = 'lognormal',
                 prior = priors,
                 iter = 4000,
                 control = list(max_treedepth = 20,
                                adapt_delta = 0.999),
                 cores = getOption('mc.cores', parallel::detectCores()))
print('South Model')
print(summary(south_mod))
saveRDS(south_mod, file = '~/scratch/middleton/model-out/south-mod.RDS')



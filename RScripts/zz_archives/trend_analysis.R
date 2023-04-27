# Trend Analysis ##
# The purpose of this script is to perform Mann Kenall trend analysis on 
# all of the diversions.

# Author: Bridget Bittmann
# Date created: 01/20/2023

# Import packages 
library(brms)
library(plyr)
library(dplyr)
library(tidyverse)
library(readr)
library(tibble)
library(flexmix)
library(modelr)
library(Kendall)

# Import data 

# diversions <- read.csv('~/scratch/trend-models/data/mixed_model_input.csv')
diversions <- read.csv('~/scratch/trend-models/data/input_full_013023.csv')
diversions <- diversions[!duplicated(diversions[c('Name', 'Year')]),] #remove duplicates
diversions$lt <- log(diversions$Acre_feet)
diversions$nat_flow <- diversions$Acre_feet - diversions$AF_used
diversions$rate <- diversions$Acre_feet / diversions$Size_acres

# Perform trend analysis

names <- unique(diversions$Name)

trend <- list()
change_names <- list()
for (i in names) {
  sub_data <- subset(diversions, Name == i)
  if (length(sub_data$Acre_feet) > 3){
    analysis <- MannKendall(sub_data$Acre_feet)
    print(i)
    print(analysis)
    if (analysis$sl < 0.05) {
      trend[[i]] <- c(i, analysis$tau, analysis$sl)
      change_names[i] <- i
    }
    else {}
  }
  else{}
}

print(change_names)
change_names <- data.frame(change_names)
div_trend <- subset(diversions, Name %in% change_names)
# write.csv(change_names, file = '~/Desktop/diversion_models/Data.Inputs/trend_names.csv')
# write.csv(div_trend, file = '~/Desktop/diversion_models/Data.Inputs/subset_trends.csv')

# Look at the data distribution of the trend data
# ggplot(div_trend, aes(Acre_feet)) +
#   geom_density()
priors <- c(
  set_prior('normal(0,1)', class = 'Intercept'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_class1_urban'),
  set_prior('gamma(1,1)', class = 'sd'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_irrig_prcp'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_irrig_temp'),
  set_prior('normal(0,1)', class = 'b', coef = 'et'),
  set_prior('normal(0,1)', class = 'b', coef = 'scale_AF_used')
)

trend_mod <- brm(lt ~ (1 + scale_class1_urban | Name) + scale_class1_urban + scale_irrig_prcp + scale_irrig_temp + et + scale_AF_used + arma(gr=Name),
                 data = div_trend,
                 family = 'normal',
                 prior = priors,
                 iter = 4000,
                 control = list(max_treedepth = 20,
                                adapt_delta = 0.999),
                 cores = getOption('mc.cores', parallel::detectCores()))
print(summary(trend_mod))
saveRDS(trend_mod, file = '~/scratch/trend-models/model-out/trend-mod.RDS')





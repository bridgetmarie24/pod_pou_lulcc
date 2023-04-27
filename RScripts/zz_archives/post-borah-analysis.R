# Post- Borah model analysis #
# -------------------------- #

# The purpose of this script is to look at the outputs of anything run in Borah for 
# diversion models. Sections are split by the name of the model output from div_mixed_model_borah.R script. 

# Date created: 01/23/2023
# Author: Bridget Bittmann

# Import packages 
library(brms)
library(plyr)
library(dplyr)
library(Matrix)
library(tidyverse)
library(flexmix)
library(modelr)
library(bayesplot)
library(tidybayes)
library(ggplot2)

# Set up directory:
cd_data <- '~/Desktop/diversion_models/Data.Inputs/'
cd_models <- '~/Desktop/diversion_models/final_models/'

## Output from: mod-mix-041123.RDS ####

# --------------------------------------------------------------------------------------- #
# Purpose of the analysis: This model asks if total diversion volumes are influenced by
#   by urban area, climate, or reservoir availability. This model standardized all 
#   predictor variables but does no differencing and does not take into account year to 
#   year changes. This includes 63 diversions. The dataset is unbalanced. 
# --------------------------------------------------------------------------------------- #

# Import the model and dataframe used for model
mod.mix <- readRDS(paste(cd_models, 'mod-mix-041123.RDS', sep=''))
df.mix <- read.csv(paste(cd_data, 'glmm_input_041123.csv', sep = ''))

# Plot model to check chain convergence and check summary for R-hat and ESS
plot(mod.mix)
summary(mod.mix)

# Plot posterior predictions vs observations
pp_check(mod.mix) +
  coord_cartesian(xlim = c(0,900000))

# Visualize quick marginal effects plots
conditional_effects(mod.mix)

# Calculate metrics of fit: Bayes R2 and Median Absolute Error
bayes_R2(mod.mix)

mae <- function(model, data_compare){
  yhat <- posterior_predict(model)
  resid <- sweep(yhat, 
                 2,
                 data_compare,
                 FUN="-")
  return(median(abs(resid)))
}

mae(mod.mix, df.mix$Acre_feet)

## Output from: mod-arma-stud-041123.RDS ####

# --------------------------------------------------------------------------------------- #
# Purpose of the analysis: This model asks how year to year changes in diversion volumes 
#   are related to changes in urban area, climate, and reservoir storage. This model differenced 
#   (delta_x = x(t)-x(t-1)) and standardized all predictor variables and uses an ARMA 
#   in the model. This includes 47 diversions. All diversions have 34 years of consecutive data.
# --------------------------------------------------------------------------------------- #

# Import the model and dataframe used for model
mod.arma <- readRDS(paste(cd_models, 'mod-arma-stud-041123.RDS', sep = ''))
df.arma <- read.csv(paste(cd_data, 'arma_input_041123.csv', sep=''))

# Plot model to check chain convergence and check summary for R-hat and ESS
plot(mod.arma)
summary(mod.arma)

# Plot posterior predictions vs observations
pp_check(mod.arma, ndraws = 20) +
   theme_bw()

# Visualize quick marginal effects plots
conditional_effects(mod.arma)

# Calculate metrics of fit: Bayes R2 and Median Absolute Error
bayes_R2(mod.arma)

mae_lt <- function(model, data_compare){
  yhat <- exp(posterior_predict(model))
  resid <- sweep(yhat, 
                 2,
                 data_compare,
                 FUN="-")
  return(median(abs(resid)))
}

mae_lt(mod.arma, df.arma$Acre_feet)


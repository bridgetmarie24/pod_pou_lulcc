# Trend analysis in Bayes #

# Author: Bridget Bittmann
# Date created: 05-01-2023

# -- PURPOSE -------------------------------------------------------------------------------- #
# This script tests each of the diversions for a change in flow through time using a Bayesian #
# generalized linear model with a Gamma distribution.                                         #
# ------------------------------------------------------------------------------------------- #

# ------------------ #
# Import packages ####
# ------------------ #
library(Matrix)
library(tidyverse) # data frame manipulation
library(brms) # to run generalized linear models in bayesian
library(tidybayes) # taking posterior draws from the model
library(ggplot2) # plotting 
library(modelr) # for data grid when drawing expected predictions

# ---------------- #
# Set Directory ####
# ---------------- #

cd <- '~/Desktop/diversion_models/'

# ------------------ #
# Import the data ####
# ------------------ #

div_data <- read.csv(paste(cd, 'Data.Inputs/glmm_input_041123.csv', sep = ''))

# ------------------------------------------------------ #
# Filter data to meet trend through time requirements ####
# ------------------------------------------------------ #

# Requirement 1: Continuous with no data gaps (e.g., didn't use a diversion one year)
# Remove diversions with gaps in the data
remove <- c("Barber pumps", 
            "Mace-Mace Canal",
            "River Run",
            "Surprise Valley and Micron",
            "Thomas Aiken Canal",
            "Warm Springs Canal")

div_data <- subset(div_data, !(Name %in% remove))

# Requirement 2: Longer than 3 time points, going according to Mann Kendall test. This will be filtered
#                out in for loop when running the models.

# --------------------------------- #
# Create function for brms model ####
# --------------------------------- # 

time_brm <- function(data, name){
  
  # Subset the data based on the name
  sub_data <- subset(data, Name == name)
  
  # Create model in brms
  mod <- brm(Acre_feet ~ Year,
             data = sub_data,
             family = 'Gamma', # set to gamma distribution because positively constrained data
             iter = 2000) # run model for 2000 iterations
  
  # Start creating a list to export the necessary values
  all_export <- list(model = mod)
  
  # Create expected predicted draws for marginal effects figures
  
  # 1) Simulate the data
  new_data <- sub_data %>%
    data_grid(Year = seq_range(Year, n = 200))
  
  # 2) Draw predictions using the simulated data
  edraws <- add_epred_draws(mod,
                            newdata = new_data,
                            ndraws = 500,
                            re_formula = NA)
  # 3) Add expected draws to list of what to return
  all_export$edraws <- edraws
  
  return(all_export)
}

# --------------------------------------------------------- #
# Run all diversions through the trend analysis function ####
# --------------------------------------------------------- #

names <- unique(div_data$Name)

all_mods <- list()
for (i in names) {
  length_test <- subset(div_data, Name == i)
  if (length(length_test$Name) > 3 ) {
    all_mods[[i]] <- time_brm(div_data, i)
  }
  else{
    print(paste('no:', i ))
  }
}



# Compare drain and diversion trends #

# The purpose of this script is to compare drain and diversion trends through time

# Author: Bridget Bittmann
# Date created: 01/24/2023

# Import packages
library(plyr)
library(dplyr)
library(tidyverse)
library(readr)
library(tibble)
library(flexmix)
library(modelr)
library(Kendall)
library(ggplot2)
library(ggpubr)

# Import relevant data
trend.div <- read.csv('~/Desktop/diversion_models/Data.Inputs/subset_trends.csv')
rf <- read.csv('~/Desktop/CASCWork/Rdata/mixed_model_input.csv')
diversions <- read.csv('~/Desktop/diversion_models/Data.Inputs/mixed_model_input.csv')
join_dict <- read.csv('~/Desktop/CASCWork/Rdata/SpatialJoin_Drain.csv')

# 

names<- c('Mason Drain', 'Mason Creek')
md <- subset(rf, Name == 'Mason Drain')
mc <- subset(rf, Name == 'Mason Creek')
rf <- subset(rf, !(Name %in% names))

mc_new <- md[, c('Year', 'Sum_AF', 'SD_AF')]
md_new <- mc[, c('Year', 'Sum_AF', 'SD_AF')]

md$Sum_AF <- md_new$Sum_AF
md$SD_AF <- md_new$SD_AF

mc$Sum_AF <- mc_new$Sum_AF
mc$SD_AF <- mc_new$SD_AF

rf <- rbind(rf, mc, md)
write.csv(rf, file = '~/Desktop/CASCWork/Rdata/model_input_correct.csv')

new_rf_mix <- readRDS('~/Desktop/CASCWork/borah-out/rf_mix_012423.RDS')
summary(new_rf_mix)


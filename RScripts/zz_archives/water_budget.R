# Water balance data 

library(corrplot)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(GGally)
library(ggfortify)
library(Kendall)

# Import data 
diversions <- read.csv('~/Desktop/diversion_models/Data.Inputs/mixed_model_input.csv')
drains <- read.csv('~/Desktop/CASCWork/Rdata/mixed_model_input.csv')

annual <- diversions %>%
  select(Year, Acre_feet) %>%
  group_by(Year) %>%
  summarize(Total = sum(Acre_feet))
annual$name <- 'Canal'

annual_drain <- drains %>%
  select(Year, Sum_AF) %>%
  group_by(Year) %>%
  summarize(Total = sum(Sum_AF))
annual_drain$name <- 'Drain'

annuals <- rbind(annual, annual_drain)

names <- unique(annuals$name)

for (i in names){
  data <- subset(annuals, name == i)
  print(i)
  print(MannKendall(data$Total))
}

ggplot(data = annuals) +
  geom_point(aes(x = Year, y = Total, color = name))



## Create figures for CDLs ##
## ----------------------- ##

## Date created: Bridget Bittmann
## Date modified: 


library(corrplot)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(GGally)
library(ggfortify)

years <- c(2007:2020)
relate <- read.csv('~/Desktop/diversion_models/Data.Inputs/CropList_2.csv')
relate_new <- data.frame(unique(relate$New_txt), unique(relate$New_val))

data <- data.frame()
for (i in years){
    name <- paste('data_', as.character(i), sep='')
    file <- paste('~/Desktop/diversion_models/Data.Inputs/TableSummaries/Summary_Reclass_CDL_', as.character(i), '.csv', sep='')
    file <- read.csv(file)
    file$Year <- i
    file$Crop <- relate_new$unique.relate.New_txt.[match(file$Value, relate_new$unique.relate.New_val.)]
    file <- file[-c(1)]
    if (i == 2005){
      data <- file
    } else {
      data <- rbind(data, file)
    }
}

data_plot <- data %>%
  select(Year, Count, Crop) %>%
  group_by(Year) %>%
  summarize(Percent = Count/sum(Count)*100)

data$Percent <- data_plot$Percent

ggplot(data, aes(fill=Crop, y=Percent, x=Year))+
  geom_bar(position='stack', stat='identity')





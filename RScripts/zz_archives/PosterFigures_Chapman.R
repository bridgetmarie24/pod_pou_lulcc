## POSTER FIGURES ##
## -------------- ##

library(corrplot)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(GGally)
library(ggfortify)

## Import data
data <- read.csv('~/Desktop/diversion_models/Data.Inputs/mixed_model_input.csv')

## Make figures of urban v discharge for 4 diversions

# Look at the urban change and choose a good spread
change <- data %>%
  select(Name, class1_urban, Acre_feet) %>%
  group_by(Name) %>%
  summarize(change = max(class1_urban)-min(class1_urban),
            avg_dis = mean(Acre_feet)) %>%
  arrange(avg_dis)

## Low - 1.55% : Lower Center Point
## 10.42% : Eureka #1
## 30.13% : New Dry Creek Canal
## High - 60.37% : Warm Springs Canal

# Extract Canal Names: 
names <- data.frame(c('Lower Center Point',
                      'Eureka No1 Canal',
                      'New Dry Creek Canal',
                      'Warm Springs Canal'))
names <- names %>%
  rename(Names = c..Lower.Center.Point....Eureka.No1.Canal....New.Dry.Creek.Canal...)
display_dat <- filter(data, Name %in% names$Names)

# Color scheme for plots
cpal <- c('purple4', 'red3', '#D55E00', 'goldenrod1')

#E69F00 yellow color

#Urban Vs. Discharge Plot
pdf(file='~/Desktop/Thesis Writing/Conferences/Presentations/Figures/urb_v_dis_all.pdf',
    width=7,
    height=4)
ggplot(data = display_dat) +
  aes(x = class1_urban, y = Acre_feet, fill = Name, color = Name) +
  geom_point() + 
  geom_smooth(method = 'lm', aes(fill = Name, color=Name)) +
  scale_color_manual(name = 'Total Urban Change',
                     labels = c('1.55%',
                                '10.42%',
                                '30.13%',
                                '60.37%'),
                     values = cpal) +
  scale_fill_manual(name = 'Total Urban Change',
                    labels = c('1.55%',
                               '10.42%',
                               '30.13%',
                               '60.37%'),
                    values = cpal) +
  theme_bw() +
  theme(legend.position = 'right') +
  xlab('Urban Proportion') + 
  ylab('Discharge (AF)') +
  theme(text = element_text(size = 18)) +
  xlim(0, 100)
ggsave('~/Desktop/Thesis Writing/Conferences/Presentations/Figures/legend.jpg', 
       width = 7,
       height = 4,
       units = 'in')
dev.off()








# ET V Discharge Plot
pdf(file='~/Desktop/Thesis Writing/Conferences/Presentations/Figures/et_v_dis_all.pdf',
    width=2.5,
    height=2)
ggplot(data = display_dat) +
  aes(x = et, y = Acre_feet, fill = Name, color = Name) +
  geom_point() + 
  geom_smooth(method = 'lm') +
  scale_color_manual(values = cpal)+
  scale_fill_manual(values = cpal) +
  theme_bw() +
  theme(legend.position = 'none') +
  xlab('Evapotranspiration (m)') + 
  ylab('Discharge (AF)') +
  theme(text = element_text(size = 18))
ggsave('~/Desktop/Thesis Writing/Conferences/Presentations/Figures/et_v_dis_all.jpg', 
       width = 4.5,
       height = 4,
       units = 'in')
dev.off()

# Precip V Discharge Plot
pdf(file='~/Desktop/Thesis Writing/Conferences/Presentations/Figures/precip_v_dis_all.pdf',
    width=2.5,
    height=2)
ggplot(data = display_dat) +
  aes(x = irrig_prcp, y = Acre_feet, fill = Name, color = Name) +
  geom_point() + 
  geom_smooth(method = 'lm') +
  scale_color_manual(values = cpal)+
  scale_fill_manual(values = cpal) +
  theme_bw() +
  theme(legend.position = 'none') +
  xlab('Irrigation Season Precip. (mm)') + 
  ylab('Discharge (AF)') +
  theme(text = element_text(size = 18))
ggsave('~/Desktop/Thesis Writing/Conferences/Presentations/Figures/prcp_v_dis_all.jpg', 
       width = 4.5,
       height = 4,
       units = 'in')
dev.off()

# Summer Temps V Discharge Plot
pdf(file='~/Desktop/Thesis Writing/Conferences/Presentations/Figures/JAtmp_v_dis_all.pdf',
    width=4.5,
    height=4)
ggplot(data = display_dat) +
  aes(x = JuneAug_temp, y = Acre_feet, fill = Name, color = Name) +
  geom_point() + 
  geom_smooth(method = 'lm') +
  scale_color_manual(values = cpal)+
  scale_fill_manual(values = cpal) +
  theme_bw() +
  theme(legend.position = 'none') +
  xlab('Avg. Max. Summer Temp. (C)') + 
  ylab('Discharge (AF)') +
  theme(text = element_text(size = 18))
ggsave('~/Desktop/Thesis Writing/Conferences/Presentations/Figures/tmp_v_dis_all.jpg', 
       width = 4.5,
       height = 4,
       units = 'in')
dev.off()

# Max Reservoir Fill V Discharge Plot
pdf(file='~/Desktop/Thesis Writing/Conferences/Presentations/Figures/maxfill_v_dis_all.pdf',
    width=4.5,
    height=4)
ggplot(data = display_dat) +
  aes(x = Max_Fill, y = Acre_feet, fill = Name, color = Name) +
  geom_point() + 
  geom_smooth(method = 'lm') +
  scale_color_manual(values = cpal)+
  scale_fill_manual(values = cpal) +
  theme_bw() +
  theme(legend.position = 'none') +
  xlab('Max. Reservoir Fill (AF)') + 
  ylab('Discharge (AF)') +
  theme(text = element_text(size = 18))
ggsave('~/Desktop/Thesis Writing/Conferences/Presentations/Figures/res_v_dis_all.jpg', 
       width = 4.5,
       height = 4,
       units = 'in')
dev.off()


## Plots for individual canals if I go that route
names <- c('Sebree Canal',
                      'Eureka No1 Canal',
                      'New Dry Creek Canal',
                      'Boise City Canal')

for (i in names){
  subdata <- subset(data, Name == i)
  outloc <- paste('~/Desktop/Thesis Writing/Conferences/Presentations/Figures/', i, sep = "")
  # pdf(file=outloc,
  #     width=4,
  #     height=4)
  print(ggplot(data = subdata) +
          aes(x = Year) +
          geom_col(aes(y=irrig_prcp)) +
          geom_point(aes(y = JuneAug_temp)) +
          scale_y_continuous('Temperature', sec.axis = sec_axis(~ .*.1, name = 'Precipitation')) +
          xlab('Year') +
          theme_bw() + 
          theme(text = element_text(size = 18))
        )
  # dev.off()
}

# Create plots of urban v discharge
for (i in names){
  subdata <- subset(data, Name == i)
  outloc <- paste('~/Desktop/Thesis Writing/Conferences/Presentations/Figures/', i, sep = "")
  pdf(file=outloc,
      width=4,
      height=4)
  print(ggplot(data = subdata) +
          aes(x = class1_urban, y = Acre_feet) +
          geom_point() +
          xlab('Percent of POU Urban (%)') +
          ylab('Diversion Discharge (AF)') + 
          theme_bw() + 
          theme(text = element_text(size = 18)) +
          geom_smooth(method = 'lm', color='black', fill = '#00798c') +
          xlim(0, 100))
  dev.off()
}


## Get Change values on real scales

change_means <- epreddraws %>%
  select(unscale.temp, .epred) %>%
  group_by(unscale.temp) %>%
  summarize(avg_pred = mean(.epred))

change_means <- epreddraws %>%
  select(unscale.precip, .epred) %>%
  group_by(unscale.precip) %>%
  summarize(avg_pred = mean(.epred))

change_means <- epreddraws %>%
  select(et, .epred) %>%
  group_by(et) %>%
  summarize(avg_pred = mean(.epred))

change_means <- epreddraws %>%
  select(unscale.urban, .epred) %>%
  group_by(unscale.urban) %>%
  summarize(avg_pred = mean(.epred))

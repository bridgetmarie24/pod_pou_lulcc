## Figures for AGU ##
## Bridget Bittmann
## Nov. 19, 2022

library(tidybayes)
library(brms)
library(bayesplot)
library(ggpubr)
library(dplyr)
detach(package:plyr)
library(paletteer)
library(tidyverse)

## Unscale function for predictor variables
unscale <- function(x, orig){
  unscaled <- (sd(orig)*2*x)+mean(orig)
  return(unscaled)
}  
## Urban vs discharge all ####

avgs <- diversions %>%
  select(Acre_feet, Name) %>%
  group_by(Name) %>%
  summarise(AF = mean(Acre_feet))

a <- ggplot(data = diversions %>%
              group_by(Name) %>%
              filter(mean(Acre_feet) > 100000)) +
  aes(x=class1_urban, y = Acre_feet, color=Name) + 
  geom_point(show.legend = FALSE) +
  geom_smooth(method = 'lm', show.legend = FALSE) +
  xlab('Urban Proportion') +
  ylab('Discharge (AF)') +
  ggtitle('Mean Annual Discharge > 200,000') +
  theme_bw() +
  theme(text = element_text(size = 16))
  
b <- ggplot(data = diversions %>%
              group_by(Name) %>%
              filter(mean(Acre_feet) < 100000 & mean(Acre_feet) > 10000)) +
  aes(x=class1_urban, y = Acre_feet, color=Name) + 
  geom_point(show.legend = FALSE) +
  geom_smooth(method = 'lm', show.legend = FALSE) +
  xlab('Urban Proportion') +
  ylab('Discharge (AF)') +
  ggtitle('200,000 > Mean Annual Discharge > 10,000') +
  theme_bw() +
  theme(text = element_text(size = 16))

c <- ggplot(data = diversions %>%
                  group_by(Name) %>%
                  filter(mean(Acre_feet) < 10000 & mean(Acre_feet) > 3000)) +
  aes(x=class1_urban, y = Acre_feet, color=Name) + 
  geom_point(show.legend = FALSE) +
  geom_smooth(method = 'lm', show.legend = FALSE) +
  xlab('Urban Proportion') +
  ylab('Discharge (AF)') +
  ggtitle('10,000 > Mean Annual Discharge > 3,000') +
  theme_bw() +
  theme(text = element_text(size = 16))

d <- ggplot(data = diversions %>%
              group_by(Name) %>%
              filter(mean(Acre_feet) < 3000)) +
  aes(x=class1_urban, y = Acre_feet, color=Name) + 
  geom_point(show.legend = FALSE) +
  geom_smooth(method = 'lm', show.legend = FALSE) +
  xlab('Urban Proportion') +
  ylab('Discharge (AF)') +
  ggtitle('Mean Annual Discharge < 3,000 ') +
  theme_bw() +
  theme(text = element_text(size = 16))

ggarrange(a,b,c,d,
          labels = c('A', 'B', 'C', 'D'),
          nrow = 2,
          ncol = 2,
          vjust = 1.5,
          hjust = 0,
          font.label=list(size = 17))
ggsave('~/Desktop/AGUPresentation/dis_v_urb_all.jpg', 
              width = 13,
              height = 8,
              units = 'in')

## Urban vs discharge####
names <- data.frame(c('Lower Center Point',
                      'Eureka No1 Canal',
                      'New Dry Creek Canal',
                      'Warm Springs Canal'))
names <- names %>%
  rename(Names = c..Lower.Center.Point....Eureka.No1.Canal....New.Dry.Creek.Canal...)
display_dat <- filter(diversions, Name %in% names$Names)

# Color scheme for plots
cpal <- c('purple4', 'red3', '#D55E00', 'goldenrod1')

ggplot(data = display_dat) +
  aes(x = class1_urban, y = Acre_feet, fill = Name, color = Name, label=Year) +
  geom_point() + 
  geom_text()+
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
  xlab('Year') + 
  ylab('Urban Proportion') +
  theme(text = element_text(size = 18))
ggsave('~/Desktop/urb_v_dis_all.jpg', 
       width = 7,
       height = 4,
       units = 'in')

diversions <- diversions %>%
  group_by(Name) %>%
  mutate(change = case_when((max(class1_urban)-min(class1_urban)) < 10 ~ '0 to 9.99%',
                            (max(class1_urban)-min(class1_urban)) >= 10 & (max(class1_urban)-min(class1_urban)) < 25 ~ '10 to 24.99%',
                            (max(class1_urban)-min(class1_urban)) >= 25 & (max(class1_urban)-min(class1_urban)) < 50 ~ '25 to 49.99%',
                            (max(class1_urban)-min(class1_urban)) >= 50 ~ '50 to 100%'))

ggplot(data = diversions) +
  aes(x = class1_urban, y = Acre_feet, fill = change, color = change) +
  geom_point() + 
  geom_smooth(method = 'lm', aes(fill = change, color=change, group = Name)) +
  scale_color_manual(name = 'Total Urban Change',
                     labels = c('0 to 9.99%',
                                '10 to 24.99%',
                                '25 to 49.99%',
                                '50 to 100%'),
                     values = cpal) +
  scale_fill_manual(name = 'Total Urban Change',
                    labels = c('0 to 9.99%',
                               '10 to 24.99%',
                               '25 to 49.99%',
                               '50 to 100%'),
                    values = cpal) +
  theme_bw() +
  theme(legend.position = 'right') +
  xlab('Urban Proportion') + 
  ylab('Discharge (AF)') +
  theme(text = element_text(size = 18)) +
  coord_cartesian(ylim = c(0, 20000))
ggsave('~/Desktop/AGUPresentation/urb_v_dis_full.jpg', 
       width = 7,
       height = 4,
       units = 'in')

ndc <- subset(diversions, Name == 'Lower Center Point')

ggplot(data = ndc) +
  aes(x = class1_urban, y = Acre_feet, fill=Name, color = Name, label = Year) +
  geom_point() + 
  geom_smooth(method = 'lm', aes(color=Name, fill=Name, group = Name)) +
  geom_text(color='black', hjust = 1, vjust = 0)+
  theme_bw() +
  theme(legend.position = 'right') +
  xlab('Urban Proportion') + 
  ylab('Discharge (AF)') +
  theme(text = element_text(size = 18)) +
  coord_cartesian(ylim = c(0, 20000))

## With all diversions on the plot ####

## Climate posterior distributions ####
mcmc_plot(AF.arma, 
          type='areas', 
          variable = c('b_scale_AF_used',
                       'b_et',
                       'b_scale_irrig_prcp',
                       'b_scale_irrig_temp'),
          prob = 0.95) +
  scale_y_discrete(labels = c('Storage Water Use',
                              'ET',
                              'Precipitation',
                              'Temperature')) +
  xlab('Relative Effect Size (log)') +
  theme(text = element_text(size=15, family = 'Arial')) 
ggsave('~/Desktop/mcmc_new.jpg', 
       width = 6,
       height = 4,
       units = 'in',
       dpi=600)

summary(AF.arma)

# ARMA MODEL ####

load('~/Desktop/diversion_models/Model_outputs/AF_arma_full.Rdata')
## URBAN PLOT ####

posterior <-as.data.frame(AF.arma)

ggplot(posterior, aes(x = b_scale_class1_urban, 
                      fill = stat(x < 0))) +
  stat_halfeye() +
  scale_fill_manual(values=c( "grey50", "#20a198"))+
  geom_vline(aes(xintercept=0), 
             color="black", size=1, linetype="dashed")+
  ylab("Probability Density") +
  xlab('Effect of Urban Area')+
  guides(fill="none") + 
  theme_bw() +
  theme(text = element_text(size = 18)) +
  geom_vline(xintercept = median(posterior$b_scale_irrig_prcp), linetype = 'dotted') +
  coord_cartesian(xlim = c(-1.0, 1.52))
ggsave('~/Desktop/AGUPresentation/urban_postmass.jpg', 
       width = 5,
       height = 4,
       units = 'in')
length(which(posterior$b_scale_class1_urban < 0))/nrow(posterior)

## PRECIP ####

new = diversions %>%
  data_grid(scale_class1_urban = mean(scale_class1_urban),
            et = mean(et),
            scale_irrig_prcp = seq_range(scale_irrig_prcp, n=200),
            scale_irrig_temp = mean(scale_irrig_temp),
            scale_AF_used = mean(scale_AF_used))
new$Name <- NA

epreddraws <-  add_epred_draws(AF.arma, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA
)

epreddraws$unscale.precip <- unscale(epreddraws$scale_irrig_prcp,
                                     diversions$irrig_prcp)
ggplot(data=epreddraws, 
       aes(x = unscale.precip, y = exp(.epred))) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("Avg. Total Precip. (mm)")  +
  theme_bw() +
  theme(text = element_text(size = 18))
ggsave('~/Desktop/AGUPresentation/prcp_marg.jpg', 
       width = 4,
       height = 4,
       units = 'in')

## TEMP ####

new = diversions %>%
  data_grid(scale_class1_urban = mean(scale_class1_urban),
            et = mean(et),
            scale_irrig_prcp = mean(scale_irrig_prcp),
            scale_irrig_temp = seq_range(scale_irrig_temp, n=200),
            scale_AF_used = mean(scale_AF_used))
new$Name <- NA

epreddraws <-  add_epred_draws(AF.arma, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA
)

epreddraws$unscale.temp <- unscale(epreddraws$scale_irrig_temp,
                                     diversions$irrig_temp)
ggplot(data=epreddraws, 
       aes(x = unscale.temp, y = exp(.epred))) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("Avg. Temp (C)")  +
  theme_bw() +
  theme(text = element_text(size = 18))
ggsave('~/Desktop/AGUPresentation/temp_marg.jpg', 
       width = 4,
       height = 4,
       units = 'in')

## ET ####

new = diversions %>%
  data_grid(scale_class1_urban = mean(scale_class1_urban),
            et = seq_range(et, n=200),
            scale_irrig_prcp = mean(scale_irrig_prcp),
            scale_irrig_temp = mean(scale_irrig_temp),
            scale_AF_used = mean(scale_AF_used))
new$Name <- NA

epreddraws <-  add_epred_draws(AF.arma, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA
)


ggplot(data=epreddraws, 
       aes(x = et, y = exp(.epred))) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("Avg. Total ET (m)")  +
  theme_bw() +
  theme(text = element_text(size = 18))
ggsave('~/Desktop/AGUPresentation/et_marg.jpg', 
       width = 4,
       height = 4,
       units = 'in')

change_et <- epreddraws%>%
  select(et, .epred) %>%
  group_by(et) %>%
  summarize(avg = median(exp(.epred)))

## Storage ####

new = diversions %>%
  data_grid(scale_class1_urban = mean(scale_class1_urban),
            et = mean(et),
            scale_irrig_prcp = mean(scale_irrig_prcp),
            scale_irrig_temp = mean(scale_irrig_temp),
            scale_AF_used = seq_range(scale_AF_used, n=200))
new$Name <- NA

epreddraws <-  add_epred_draws(AF.arma, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA
)

epreddraws$unscale.use <- unscale(epreddraws$scale_AF_used,
                                     diversions$AF_used)
ggplot(data=epreddraws, 
       aes(x = unscale.use, y = exp(.epred))) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("Storage Water Use (AF)")  +
  theme_bw() +
  theme(text = element_text(size = 18))
ggsave('~/Desktop/AGUPresentation/use_marg.jpg', 
       width = 4,
       height = 4,
       units = 'in')

## NO TIME MODEL ####
## URBAN EFFECT ####

new = diversions %>%
  data_grid(scale_class1_urban = seq_range(scale_class1_urban, n=200),
            et = mean(et),
            scale_irrig_prcp = mean(scale_irrig_prcp),
            scale_irrig_temp = mean(scale_irrig_temp),
            scale_AF_used = mean(scale_AF_used))


epreddraws <-  add_epred_draws(AF.mix.stor.data, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA
)

epreddraws$unscale.urban <- epreddraws$scale_class1_urban*100

change_urb <- epreddraws%>%
  select(unscale.urban, .epred) %>%
  group_by(unscale.urban) %>%
  summarize(avg = mean(exp(.epred)))
  
ggplot(data=epreddraws, 
       aes(x = unscale.urban, y = exp(.epred))) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("Urban Percentage of POU (%)")  +
  theme_bw() +
  theme(text = element_text(size = 18))
ggsave('~/Desktop/AGUPresentation/urb_marg_notime.jpg', 
       width = 5,
       height = 4,
       units = 'in')

## Precip EFFECT ####

new = diversions %>%
  data_grid(scale_class1_urban = mean(scale_class1_urban),
            et = mean(et),
            scale_irrig_prcp = seq_range(scale_irrig_prcp, n=200),
            scale_irrig_temp = mean(scale_irrig_temp),
            scale_AF_used = mean(scale_AF_used))


epreddraws <-  add_epred_draws(AF.mix.stor.data, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA
)

epreddraws$unscale.precip <- unscale(epreddraws$scale_irrig_prcp,
                                     diversions$irrig_prcp)

ggplot(data=epreddraws, 
       aes(x = unscale.precip, y = exp(.epred))) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("Avg. Total Precip. (mm)")  +
  theme_bw() +
  theme(text = element_text(size = 18))
ggsave('~/Desktop/AGUPresentation/prcp_marg_notime.jpg', 
       width = 4,
       height = 4,
       units = 'in')

change_prcp <- epreddraws%>%
  select(unscale.precip, .epred) %>%
  group_by(unscale.precip) %>%
  summarize(avg = mean(exp(.epred)))

## Storage Water ####
new = diversions %>%
  data_grid(scale_class1_urban = mean(scale_class1_urban),
            et = mean(et),
            scale_irrig_prcp = mean(scale_irrig_prcp),
            scale_irrig_temp = mean(scale_irrig_temp),
            scale_AF_used = seq_range(scale_AF_used, n=200))


epreddraws <-  add_epred_draws(AF.mix.stor.data, 
                               newdata=new,
                               ndraws=1000,
                               re_formula=NA
)

epreddraws$unscale.use <- unscale(epreddraws$scale_AF_used,
                                     diversions$AF_used)

ggplot(data=epreddraws, 
       aes(x = unscale.use, y = exp(.epred))) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Canal Discharge (Acre-ft/yr)") + xlab("Storage Water Use (AF)")  +
  theme_bw() +
  theme(text = element_text(size = 18))
ggsave('~/Desktop/AGUPresentation/storage_marg_notime.jpg', 
       width = 4,
       height = 4,
       units = 'in')

## Temp posterior mass ####

posterior <-as.data.frame(AF.mix.stor.data)

ggplot(posterior, aes(x = b_scale_irrig_temp, 
                      fill = stat(x < 0))) +
  stat_halfeye() +
  scale_fill_manual(values=c( "grey50", "#20a198"))+
  geom_vline(aes(xintercept=0), 
             color="black", size=1, linetype="dashed")+
  ylab("Density") +
  xlab('Effect of Avg. Temp (C)')+
  guides(fill="none") + 
  theme_bw() +
  theme(text = element_text(size = 18)) +
  geom_vline(xintercept = median(posterior$b_scale_irrig_prcp), linetype = 'dotted')
ggsave('~/Desktop/AGUPresentation/temp_postmass.jpg', 
       width = 4,
       height = 4,
       units = 'in')
length(which(posterior$b_scale_irrig_temp < 0))/nrow(posterior)

## ET Posterior Mass ####
posterior <-as.data.frame(AF.mix.stor.data)

ggplot(posterior, aes(x = b_et, 
                      fill = stat(x < 0))) +
  stat_halfeye() +
  scale_fill_manual(values=c( "grey50", "#20a198"))+
  geom_vline(aes(xintercept=0), 
             color="black", size=1, linetype="dashed")+
  ylab("Density") +
  xlab('Effect of ET')+
  guides(fill="none") + 
  theme_bw() +
  theme(text = element_text(size = 18)) +
  geom_vline(xintercept = median(posterior$b_scale_irrig_prcp), linetype = 'dotted')
ggsave('~/Desktop/AGUPresentation/et_postmass.jpg', 
       width = 4,
       height = 4,
       units = 'in')
length(which(posterior$b_et < 0))/nrow(posterior)

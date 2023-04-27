## Create Figures from Model Results ##
## --------------------------------- ##


## Load packages ##
library(brms)
library(ggplot2)
library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)
library(modelr)

## Unscale function for predictor variables
unscale <- function(x, orig){
  unscaled <- (sd(orig)*2*x)+mean(orig)
  return(unscaled)
}  

## Step 1: Create data to generate the predictions over a continuous range of values:
diversions <- data.frame(read.csv('~/Desktop/diversion_models/Data.Inputs/mixed_model_input.csv'))  
load('~/Desktop/diversion_models/Model_outputs/AF_arma_full.Rdata')
simdata <- diversions %>%
  data_grid(scale_.class1_urban = seq_range(scale_.class1_urban,
                                    n=200),
            scale_.irrig_prcp = mean(scale_.irrig_prcp),
            scale_.JuneAug_temp = mean(scale_.JuneAug_temp),
            scale_.Max_Fill = mean(scale_.Max_Fill),
            et = mean(et))

## Step 2: generate predictions from model:

epreddraws <-  add_epred_draws(AF.logn.mixJAtmp, 
                               newdata=simdata,
                               ndraws=1000,
                               re_formula=NA
)

##Step 3: Plot the data

epreddraws$unscale.urban <- epreddraws$scale_.class1_urban*100

# pdf(file='~/Desktop/diversion_models/Figures/AFurb_marg.pdf',
#     width=4,
#     height=4)
ggplot(data=epreddraws, 
       aes(x = unscale.urban, y = mean(.epred))) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Diversion Rate (Acre-ft/yr)") + xlab("Urban Proportion") +
  theme_bw() +
  theme(text = element_text(size = 18))
ggsave('~/Desktop/diversion_models/Figures/AFurb_marg.jpg', 
       width = 4,
       height = 4,
       units = 'in')
# dev.off()

## Posterior mass plot 
posterior <-as.data.frame(AF.logn.mixJAtmp)

# pdf(file='~/Desktop/diversion_models/Figures/AFurb_postmass.pdf',
#     width=4,
#     height=4)
ggplot(posterior, aes(x = b_scale_.class1_urban, 
                      fill = stat(x < 0))) +
  stat_halfeye() +
  scale_fill_manual(values=c("grey50", "#E69F00"))+
  geom_vline(aes(xintercept=0), 
             color="black", size=1, linetype="dashed")+
  ylab("Density") +
  xlab('Effect of Urban Proportion')+
  guides(fill="none") +
  theme_bw() +
  theme(text = element_text(size = 18))
ggsave('~/Desktop/diversion_models/Figures/AFurb_postmass.jpg', 
       width = 4,
       height = 4,
       units = 'in')
# dev.off()
#20a198 #greenish color
length(which(posterior$b_scale_.class1_urban < 0))/nrow(posterior)

## Precipitation

simdata <- diversions %>%
  data_grid(scale_.class1_urban = mean(scale_.class1_urban),
            scale_.irrig_prcp = seq_range(scale_.irrig_prcp, n=200),
            scale_.JuneAug_temp = mean(scale_.JuneAug_temp),
            scale_.Max_Fill = mean(scale_.Max_Fill),
            et = mean(et))
epreddraws <-  add_epred_draws(AF.logn.mixJAtmp, 
                               newdata=simdata,
                               ndraws=1000,
                               re_formula=NA
)
epreddraws$unscale.precip <- unscale(epreddraws$scale_.irrig_prcp,
                                     diversions$irrig_prcp)
# pdf(file='~/Desktop/diversion_models/Figures/AFprcp_marg.pdf',
#        width=4,
#        height=4)
ggplot(data=epreddraws, 
       aes(x = unscale.precip, y = .epred)) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Diversion Rate (Acre-ft/yr)") + xlab("Precipitation (mm)") +
  theme_bw() +
  theme(text = element_text(size = 18))
ggsave('~/Desktop/diversion_models/Figures/AFprcp_marg.jpg', 
       width = 4,
       height = 4,
       units = 'in')
# dev.off()

## Temperature
simdata <- diversions %>%
  data_grid(scale_.class1_urban = mean(scale_.class1_urban),
            scale_.irrig_prcp = mean(scale_.irrig_prcp),
            scale_.JuneAug_temp = seq_range(scale_.JuneAug_temp, n=200),
            scale_.Max_Fill = mean(scale_.Max_Fill),
            et = mean(et))
epreddraws <-  add_epred_draws(AF.logn.mixJAtmp, 
                               newdata=simdata,
                               ndraws=1000,
                               re_formula=NA
)
epreddraws$unscale.temp <- unscale(epreddraws$scale_.JuneAug_temp,
                                     diversions$JuneAug_temp)
# pdf(file='~/Desktop/diversion_models/Figures/AFJAtmp_marg.pdf',
#     width=4,
#     height=4)
ggplot(data=epreddraws, 
       aes(x = unscale.temp, y = .epred)) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Diversion Rate (Acre-ft/yr)") + xlab("Avg. Max. Summer Temp. (C)")  +
  theme_bw() +
  theme(text = element_text(size = 18))
ggsave('~/Desktop/diversion_models/Figures/AFJAtmp_marg.jpg', 
       width = 4.5,
       height = 4,
       units = 'in')
# dev.off()

## Reservoir Fill 
simdata <- diversions %>%
  data_grid(scale_.class1_urban = mean(scale_.class1_urban),
            scale_.irrig_prcp = mean(scale_.irrig_prcp),
            scale_.JuneAug_temp = mean(scale_.JuneAug_temp),
            scale_.Max_Fill = seq_range(scale_.Max_Fill, n=200),
            et = mean(et))
epreddraws <-  add_epred_draws(AF.logn.mixJAtmp, 
                               newdata=simdata,
                               ndraws=1000,
                               re_formula=NA
)
epreddraws$unscale.resfill <- unscale(epreddraws$scale_.Max_Fill,
                                     diversions$Max_Fill)
# pdf(file='~/Desktop/diversion_models/Figures/AFresfill_marg.pdf',
#     width=6,
#     height=4)
ggplot(data=epreddraws, 
       aes(x = unscale.resfill, y = .epred)) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#197bdd", 
    color="#0033CC", size=2) + 
  ylab("Diversion Rate (Acre-ft/yr)") + xlab("Maximum Reservoir Storage (AF)")+
  theme_bw()
ggsave('~/Desktop/diversion_models/Figures/AFresfill_marg.jpg', 
       width = 4,
       height = 4,
       units = 'in')
# dev.off()

## ET 
simdata <- diversions %>%
  data_grid(scale_.class1_urban = mean(scale_.class1_urban),
            scale_.irrig_prcp = mean(scale_.irrig_prcp),
            scale_.JuneAug_temp = mean(scale_.JuneAug_temp),
            scale_.Max_Fill = mean(scale_.Max_Fill),
            et = seq_range(et, n=200))
epreddraws <-  add_epred_draws(AF.logn.mixJAtmp, 
                               newdata=simdata,
                               ndraws=1000,
                               re_formula=NA
)

# pdf(file='~/Desktop/diversion_models/Figures/AFet_marg.pdf',
#     width=4,
#     height=4)
ggplot(data=epreddraws, 
       aes(x = et, y = .epred)) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#00798c", 
    color="black", size=2) + 
  ylab("Diversion Rate (Acre-ft/yr)") + xlab("Evapotranspiration (m)") +
  theme_bw() +
  theme(text = element_text(size = 18))
ggsave('~/Desktop/diversion_models/Figures/AFet_marg.jpg', 
       width = 4,
       height = 4,
       units = 'in')
# dev.off()

summary(AF.logn.mixJAtmp)
ranef(AF.logn.mixJAtmp)


## TIMING PLOTS ##
ranef(len.ST.mix.ir)
summary(len.ST.mix.ir)

simdata <- diversions %>%
  data_grid(scale_.class1_urban = seq_range(scale_.class1_urban, n=200),
            scale_.irrig_prcp = mean(scale_.irrig_prcp),
            scale_.irrig_temp = mean(scale_.irrig_temp),
            scale_.Max_Fill = mean(scale_.Max_Fill),
            et = mean(et))
epreddraws <-  add_epred_draws(len.ST.mix.ir, 
                               newdata=simdata,
                               ndraws=1000,
                               re_formula=NA
)
epreddraws$unscale.urban <- epreddraws$scale_.class1_urban *100
pdf(file='~/Desktop/diversion_models/Figures/lenurban_marg.pdf',
    width=4,
    height=4)
ggplot(data=epreddraws, 
       aes(x = unscale.urban, y = .epred)) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#197bdd", 
    color="#0033CC", size=2) + 
  ylab("Length of Irrigation Season (days)") + xlab("Urban Proportion") +
  ylim(165,205) + theme_bw()
dev.off()



## Posterior mass plot 
posterior <-as.data.frame(len.ST.mix.ir)

pdf(file='~/Desktop/diversion_models/Figures/lenurb_postmass.pdf',
    width=4,
    height=4)
ggplot(posterior, aes(x = b_scale_.class1_urban, 
                      fill = stat(x < 0))) +
  stat_halfeye() +
  scale_fill_manual(values=c( "#20a198", "grey50"))+
  geom_vline(aes(xintercept=0), 
             color="black", size=1, linetype="dashed")+
  ylab("Density") +
  xlab('Effect of Urban Proportion')+
  guides(fill="none") + 
  theme_bw()
dev.off()

length(which(posterior$b_scale_.class1_urban > 0))/nrow(posterior)

## ET 
simdata <- diversions %>%
  data_grid(scale_.class1_urban = mean(scale_.class1_urban),
            scale_.irrig_prcp = mean(scale_.irrig_prcp),
            scale_.irrig_temp = mean(scale_.irrig_temp),
            scale_.Max_Fill = mean(scale_.Max_Fill),
            et = seq_range(et, n=200))
epreddraws <-  add_epred_draws(len.ST.mix.ir, 
                               newdata=simdata,
                               ndraws=1000,
                               re_formula=NA
)
pdf(file='~/Desktop/diversion_models/Figures/lenet_marg.pdf',
    width=6,
    height=4)
ggplot(data=epreddraws, 
       aes(x = et, y = .epred)) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#197bdd", 
    color="#0033CC", size=2) + 
  ylab("Length of Irrigation Season (days)") + xlab("Evapotranspiration (m)")+
  ylim(165,205) +
  theme_bw()
dev.off()

# Precipitation
simdata <- diversions %>%
  data_grid(scale_.class1_urban = mean(scale_.class1_urban),
            scale_.irrig_prcp = seq_range(scale_.irrig_prcp, n=200),
            scale_.irrig_temp = mean(scale_.irrig_temp),
            scale_.Max_Fill = mean(scale_.Max_Fill),
            et = mean(et))
epreddraws <-  add_epred_draws(len.ST.mix.ir, 
                               newdata=simdata,
                               ndraws=1000,
                               re_formula=NA
)
epreddraws$unscale.precip <- unscale(epreddraws$scale_.irrig_prcp,
                                     diversions$irrig_prcp)
pdf(file='~/Desktop/diversion_models/Figures/lenprcp_marg.pdf',
    width=6,
    height=4)
ggplot(data=epreddraws, 
       aes(x = unscale.precip, y = .epred)) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#197bdd", 
    color="#0033CC", size=2) + 
  ylab("Length of Irrigation Season (days)") + xlab("Precipitation (mm)")+
  ylim(165,205) + 
  theme_bw()
dev.off()

# Temperature 

simdata <- diversions %>%
  data_grid(scale_.class1_urban = mean(scale_.class1_urban),
            scale_.irrig_prcp = mean(scale_.irrig_prcp),
            scale_.irrig_temp = seq_range(scale_.irrig_temp, n=200),
            scale_.Max_Fill = mean(scale_.Max_Fill),
            et = mean(et))
epreddraws <-  add_epred_draws(len.ST.mix.ir, 
                               newdata=simdata,
                               ndraws=1000,
                               re_formula=NA
)
epreddraws$unscale.temp <- unscale(epreddraws$scale_.irrig_temp,
                                     diversions$irrig_temp)
pdf(file='~/Desktop/diversion_models/Figures/lentemp_marg.pdf',
    width=6,
    height=4)
ggplot(data=epreddraws, 
       aes(x = unscale.temp, y = .epred)) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#197bdd", 
    color="#0033CC", size=2) + 
  ylab("Length of Irrigation Season (days)") + xlab("Mean Maximum Temperature (C)")+
  ylim(165,205) +
  theme_bw()
dev.off()

## Max Fill
simdata <- diversions %>%
  data_grid(scale_.class1_urban = mean(scale_.class1_urban),
            scale_.irrig_prcp = mean(scale_.irrig_prcp),
            scale_.irrig_temp = mean(scale_.irrig_temp),
            scale_.Max_Fill = seq_range(scale_.Max_Fill, n=200),
            et = mean(et))
epreddraws <-  add_epred_draws(len.ST.mix.ir, 
                               newdata=simdata,
                               ndraws=1000,
                               re_formula=NA
)
epreddraws$unscale.maxfill <- unscale(epreddraws$scale_.Max_Fill,
                                   diversions$Max_Fill)
pdf(file='~/Desktop/diversion_models/Figures/lenMF_marg.pdf',
    width=6,
    height=4)
ggplot(data=epreddraws, 
       aes(x = unscale.maxfill, y = .epred)) +
  stat_lineribbon(
    .width = c(.5, 0.95), alpha = 0.35, fill="#197bdd", 
    color="#0033CC", size=2) + 
  ylab("Length of Irrigation Season (days)") + xlab("Maximum Reservoir Storage (AF)")+
  ylim(165,205) +
  theme_bw()
dev.off()


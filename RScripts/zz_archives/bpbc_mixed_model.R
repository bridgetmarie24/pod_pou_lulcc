## Mixed Effects Models with Boise Project Data ##

# By: Bridget Bittmann
# Date created: 08/11/2022
# Date last modified: 


## Import packages
library(brms)
library(bayesplot)
library(plyr)
library(dplyr)
library(tidyverse)
library(tidybayes)
library(readr)
library(ggplot2)
library(tibble)
library(ggrepel)
library(flexmix)
library(modelr)

## Preprocess the data
data <- data.frame(read.csv('~/Desktop/diversion_models/Data.Inputs/bpbc_model_input.csv'))
data <- data[-c(1)] # drops Python index output with csv
data[is.na(data)] <- 100 #fills NA contagion values with 100
data <- na.omit(data)

# nas <- data[rowSums(is.na(data)) > 0, ]

## Standardize data 
scale2sd <- function(x){
  (x - mean(x))/(sd(x)*2)
}

col_number <- c(8, 9, 10, 11, 13, 14, 15)

for (i in col_number) {
  name <- colnames(data[i])
  new_col_name <- paste('scale_', name, sep = "")
  data[new_col_name] <- scale2sd(data[,i])
}

## Convert percentages to proportions to get between 0 and 1

col_number <- c(4:7)

for (i in col_number) {
  name <- colnames(data[i])
  new_col_name <- paste('scale_', name, sep = "")
  data[new_col_name] <- (data[,i])/100
}

## Check the distribution of the volume
pdf(file='~/Desktop/diversion_models/Figures/acreft_hist.pdf',
    width=6,
    height=4)
ggplot(data=data)+
  aes(x=Acre_feet)+
  geom_histogram(color='black')+
  ylab('Count')+
  xlab('Annual Discharge (AF/yr)')+
  theme_bw()
dev.off()

ggplot(data=data)+
  aes(x=Acre_feet)+
  geom_density(color='black')+
  ylab('Density')+
  xlab('Annual Discharge (AF/yr)')+
  theme_bw()

## Use brms for a mixed effects model

AF.logn.clim.bpbc <- brm(Acre_feet ~ (1|Name)+scale_irrig_prcp+scale_irrig_temp+et+scale_Max_Fill,
                      data=data,
                      family='lognormal',
                      iter = 8000,
                      prior = test_priors,
                      control=list(max_treedepth=20, 
                                   adapt_delta=0.999),
                      cores=getOption('mc.cores', parallel::detectCores()))
summary(AF.logn.clim.bpbc)
pp_check(AF.logn.clim.bpbc)
conditional_effects(AF.logn.clim.bpbc)

## Test priors

test_priors <- c(
  set_prior("normal(-2,2)", class = "b", coef = "scale_irrig_prcp"),
  set_prior("normal(-2,2)", class ='b', coef = 'scale_irrig_temp'),
  set_prior("normal(-2,2)", class = "b", coef = "et"),
  set_prior("normal(-2,2)", class = "b", coef = "scale_Max_Fill"),
  set_prior("student_t(3,5,2.5)", class="Intercept")
)

prior_samples <- brm(Acre_feet ~ (1|Name) + scale_irrig_prcp + scale_irrig_temp + et + scale_Max_Fill,
                     family = lognormal(),
                     data = data,  # This data won't actually be incorporated into the posterior! Just there to let the model run.
                     prior = test_priors, 
                     sample_prior = "only")
summary(prior_samples)
preds_from_prior <-  add_predicted_draws(data, prior_samples, ndraws = 10)

# visualize by plotting the predictions (based only on the prior) for different values of brightness:
ggplot(preds_from_prior,
       aes(x = scale_irrig_prcp, y = .prediction)) + 
  geom_point(alpha = 0.4) +
  ylim(0,500000)

AF.logn.urb.bpbc <- brm(Acre_feet ~ (1|Name+scale_class1_urban) + scale_class1_urban,
                      data=data,
                      family = 'lognormal',
                      iter=8000,
                      control=list(max_treedepth=20,
                                   adapt_delta=0.99),
                      save_pars=save_pars(all=TRUE),
                      cores=getOption('mc.cores', parallel::detectCores()))
save(AF.logn.urb.bpbc, file='~/Desktop/diversion_models/Model_outputs/AF_logn_urb_bpbc.Rdata')
load('~/Desktop/diversion_models/Model_outputs/AF_logn_urb_bpbc.Rdata')
AF.logn.urb.bpbc <- add_criterion(AF.logn.urb.bpbc, criterion='loo', moment_match=TRUE)
plot(AF.logn.urb.bpbc)
summary(AF.logn.urb.bpbc)
pp_check(AF.logn.urb.bpbc, ndraws=100)
conditional_effects(AF.logn.urb.bpbc)

AF.logn.mix.bpbc <- brm(Acre_feet ~ (1 | Name + scale_class1_urban) + scale_class1_urban + scale_irrig_prcp + scale_JuneAug_temp + et + scale_Max_Fill,
                    data=data,
                    family = 'lognormal',
                    iter=10000,
                    control=list(max_treedepth=20,
                                 adapt_delta=0.99),
                    save_pars=save_pars(all=TRUE),
                    cores=getOption('mc.cores', parallel::detectCores()))
AF.logn.mix.bpbc <- add_criterion(AF.logn.mix.bpbc, criterion='loo', moment_match=TRUE)
save(AF.logn.mix.bpbc, file='~/Desktop/diversion_models/Model_outputs/AF_logn_mix_bpbc.Rdata')
summary(AF.logn.mix.bpbc)
conditional_effects(AF.logn.mix.bpbc)
load('~/Desktop/diversion_models/Model_outputs/AF_logn_mix_bpbc.Rdata')

AF.logn.agurb.bpbc <- brm(Acre_feet ~ (1 | Name + scale_class2_crops) + scale_class2_crops + scale_irrig_prcp + scale_JuneAug_temp + et + scale_Max_Fill,
                          data=data,
                          family = 'lognormal',
                          iter=8000,
                          control=list(max_treedepth=20,
                                       adapt_delta=0.99),
                          cores=getOption('mc.cores', parallel::detectCores()))
save(AF.logn.agurb.bpbc, file='~/Desktop/diversion_models/Model_outputs/AF_logn_ag_bpbc.Rdata')
summary(AF.logn.agurb.bpbc)
AF.logn.agurb.bpbc <- add_criterion(AF.logn.agurb.bpbc, criterion='loo', moment_match=TRUE)

AF.logn.mix2.bpbc <- brm(Acre_feet ~ (1 | Name + scale_class1_urban) + scale_class1_urban + scale_irrig_prcp + et + scale_Max_Fill,
                         data=data,
                         family = 'lognormal',
                         iter=8000,
                         control=list(max_treedepth=20,
                                      adapt_delta=0.99),
                         save_pars=save_pars(all=TRUE),
                         cores=getOption('mc.cores', parallel::detectCores()))
summary(AF.logn.mix2.bpbc)
save(AF.logn.mix2.bpbc, file='~/Desktop/diversion_models/Model_outputs/AF_logn_mix2_bpbc.Rdata')

AF.logn.JAtmp.bpbc <- brm(Acre_feet ~ (1|Name) + scale_JuneAug_temp,
                          data = data,
                          family = 'lognormal',
                          iter = 25000,
                          control = list(max_treedepth = 20,
                                         adapt_delta = 0.999),
                          prior = test_priors,
                          save_pars = save_pars(all=TRUE),
                          cores = getOption('mc.cores', parallel::detectCores()))
summary(AF.logn.JAtmp.bpbc)
pp_check(AF.logn.JAtmp.bpbc)
test_priors <- c(
  set_prior("normal(0,1)", class="Intercept"),
  set_prior("normal(-0.1,0.3)", class="b", coef = "scale_JuneAug_temp")
)

prior_samples <- brm(Acre_feet ~ (1|Name) + scale_JuneAug_temp,
                     family = lognormal(),
                     data = data,  # This data won't actually be incorporated into the posterior! Just there to let the model run.
                     prior = test_priors, 
                     sample_prior = "only")
summary(prior_samples)
preds_from_prior <-  add_predicted_draws(data, prior_samples, ndraws = 10)

# visualize by plotting the predictions (based only on the prior) for different values of brightness:
ggplot(preds_from_prior,
       aes(x = scale_irrig_prcp, y = .prediction)) + 
  geom_point(alpha = 0.4) +
  ylim(0,500000)
max(preds_from_prior$.prediction)
mean(preds_from_prior$.prediction)
median(preds_from_prior$.prediction)
mean(data$Acre_feet)
median(data$Acre_feet)

## Test Priors 

# Filter data, so each group has more than 10 data points
filtered <- data %>%
  group_by(Name) %>%
  filter(n() > 10)

out <- data %>%
  group_by(Name) %>%
  filter(n() < 10)

# Look at urban vs discharge
names <- unique(data$Name)

for (i in names) {
  sub_data <- subset(data, Name == i)
  print(ggplot(data=sub_data) +
          aes(x=class1_urban, y=Acre_feet)+
          geom_point()+
          theme_bw()+
          ggtitle(i) +
          xlab('% Urban')+
          ylab('Discharge (AF/year'))
}

for (i in names) {
  sub_data <- subset(data, Name == i)
  print(ggplot(data=sub_data) +
          aes(x=Acre_feet)+
          geom_histogram(bins=10)+
          theme_bw()+
          ggtitle(i) +
          xlab('Discharge')+
          ylab('Density'))
}

for (i in names){
  sub_data <- subset(data, Name == i)
  print(ggplot(data=sub_data) +
          aes(x = Year, y = Acre_feet)+
          geom_point() +
          theme_bw() + 
          ggtitle(i) +
          xlab('Year') + 
          ylab('Discharge (AF/yr)'))
}

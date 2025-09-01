library(brms)
library(plm)

working_dir <- 'Desktop/temp-dir/pod_pou_lulcc/'

# Read in the model
mod.arma <- readRDS(paste(working_dir, 'models/mod-arma-stud-041123.RDS', sep = ''))

# Grab the residuals
res <- residuals(mod.arma, summary = FALSE)
meanresids <- apply(res, 2, mean)

# Read in the original data to get group and year data
data <- read.csv(paste(working_dir, 'data/arma_input_041123.csv', sep = ''))
data$res <- meanresids

resid_df <- data[, c('Name', 'Year', 'res')]
resid_pframe <- pdata.frame(resid_df, index = c('Name', 'Year'))

# test for stationarity
purtest(resid_pframe$res, data = resid_pframe, lags = 'AIC', test = 'levinlin') 


library(brms)
library(plm)

cd_models <- '~/Desktop/diversion_models/final_models/'
mod.arma <- readRDS(paste(cd_models, 'mod-arma-stud-041123.RDS', sep = ''))

res <- residuals(mod.arma)
meanres <- apply(res, 2, mean)

# test for stationarity
purtest(res, test = 'levinlin')
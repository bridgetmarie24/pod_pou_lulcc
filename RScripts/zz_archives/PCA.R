## --------------------------------------- ##
## PCA for Diversion Explanatory Variables ## 
## --------------------------------------- ## 

## By: Bridget Bittmann
## Date created: 06/09/2022
## Date modified: 

divdata <- read_csv('~/Desktop/diversion_models/Data.Inputs/mixed_model_input.csv')
cols <- c(18, 21:30)
cut <- divdata[,c(18, 21:30)]

pca <- prcomp(cut)
summary(pca)
print(pca)

pdf(file='~/Desktop/diversion_models/Figures/pca1-2.pdf',
    width=8,
    height = 8)
colnames(cut) <- c('ET','Prcp_non-irrig', 'Prcp_irrig', 'T_irrig', 'T_June-Aug', 'Q_in-LP', 'MaxFill', 'p_urban','p_ag','C','LPI')
autoplot(kmeans(cut, 3), data=cut, 
         loadings=T, 
         loadings.label=T, 
         loadings.colour='navy',
         loadings.label.colour='black')
dev.off()

screeplot(pca, npcs=9, type='lines')

pdf(file='~/Desktop/diversion_models/Figures/pca1-3.pdf',
    width=8,
    height = 8)
cut <- data[,c(18, 21:30)]
colnames(cut) <- c('ET','Prcp_non-irrig', 'Prcp_irrig', 'T_irrig', 'T_June-Aug', 'Q_in-LP', 'MaxFill', 'p_urban','p_ag','C','LPI')
autoplot(kmeans(cut, 3), x=1, y=3, data=cut, 
         loadings=T, 
         loadings.label=T, 
         loadings.colour='navy',
         loadings.label.colour='black')
dev.off()

pdf(file='~/Desktop/diversion_models/Figures/pca2-3.pdf',
    width=8,
    height = 8)
cut <- data[,c(18, 21:30)]
colnames(cut) <- c('ET','Prcp_non-irrig', 'Prcp_irrig', 'T_irrig', 'T_June-Aug', 'Q_in-LP', 'MaxFill', 'p_urban','p_ag','C','LPI')
autoplot(kmeans(cut, 3), x=2, y=3, data=cut, 
         loadings=T, 
         loadings.label=T, 
         loadings.colour='navy',
         loadings.label.colour='black')
dev.off()

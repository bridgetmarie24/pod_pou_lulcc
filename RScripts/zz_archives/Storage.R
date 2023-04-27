## STORAGE WATER PRE-ANALYSIS ##


storage <- data.frame(read_csv('~/Desktop/diversion_models/Data.Inputs/accounting.csv'))

names <- c('New York Canal',
           'Ridenbaugh Canal',
           'Settlers Canal')

for (i in names){
  sub_data <- subset(storage, Name == i)
  sub_data2 <- subset(diversions, Name == i)
  print(ggplot() + 
          geom_point(data = sub_data, aes(x = Year, y = AF_used), color = "#00798c") +
          geom_point(data = sub_data2, aes(x = Year, y = Acre_feet), color = "#E69F00")+
          ggtitle(i) + 
          theme_bw() +
          ylab('Water Used from Storage (AF)')+
          xlab('Year'))
}

simdata <- diversions %>%
  data_grid(scale_.class1_urban = seq_range(scale_.class1_urban, n=200))
epreddraws <-  add_epred_draws(start.urb, 
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
  ylab("Start Day of Irrigation Season (DOY)") + xlab("Urban Proportion") +
  theme_bw()






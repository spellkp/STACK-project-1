library(ggplot2)
library(reshape2)

MRSmeasure <- as.data.frame(read.csv("MRSmeasure"))
names(MRSmeasure) <- c("Day", "Jeffrey Energy Center", "J. S. Cooper Plant", "TransAlta Centralia \n Generating")

MRSmeasure[2] <- 100*MRSmeasure[2]
MRSmeasure[3] <- 100*MRSmeasure[3]
MRSmeasure[4] <- 100*MRSmeasure[4]

MRSmeasure <- melt(MRSmeasure ,  id.vars = 'Day', variable.name = 'series')

  ggplot(MRSmeasure, aes(Day, value)) + 
    geom_point() + 
    geom_smooth(se = FALSE) +
    ylim(-0.025, 0.025) +
    ylab("Metric Value (%)") +
    facet_grid(series ~ .) +
    theme_bw()
  
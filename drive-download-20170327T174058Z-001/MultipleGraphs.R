library(ggplot2)
library(reshape2)
library(ggmap)

MRSmeasure <- as.data.frame(read.csv("MRSmeasure"))
names(MRSmeasure) <- c("Day", "Jeffrey Energy Center", "J. S. Cooper Plant", "TransAlta Centralia Generating")

MRSmeasure[2] <- 100*MRSmeasure[2]
MRSmeasure[3] <- 100*MRSmeasure[3]
MRSmeasure[4] <- 100*MRSmeasure[4]

MRSmeasure2 <- melt(MRSmeasure ,  id.vars = 'Day', variable.name = 'series')

  ggplot(MRSmeasure2, aes(Day, value)) + 
    geom_point() + 
    geom_smooth(se = FALSE) +
    ylim(0, 0.25) +
    ylab("Metric Value (%)") +
    facet_grid(series ~ .) +
    theme_bw()

  
  
  Metric1 <- data.frame(c(1:366), Metric1)
  colnames(Metric1)[1] <- "Day"
  
  ggplot(data = Metric1, aes(x = Day, y = Metric1)) +
    geom_point() +
    geom_smooth(se = FALSE) +
    labs(title = "JEC") +
    ylim(-0.00025, 0.00025) +
    theme_bw()
  
  
  
  Metric2 <- data.frame(c(1:366), Metric2)
  colnames(Metric2)[1] <- "Day"
  
  ggplot(data = Metric2, aes(x = Day, y = Metric2)) +
    geom_point() +
    geom_smooth(se = FALSE) +
    labs(title = "JSC") +
    ylim(-0.00025, 0.00025) +
    theme_bw()
  
  
  
  Metric3 <- data.frame(c(1:366), Metric3)
  colnames(Metric3)[1] <- "Day"
  
  ggplot(data = Metric3, aes(x = Day, y = Metric3)) +
    geom_point() +
    geom_smooth(se = FALSE) +
    labs(title = "TCG") +
    ylim(-0.00025, 0.00025) +
    theme_bw()
  
  
  
  
  
  
  
  ##### Max Day for JEC #####
  
  JECModel1 <- read.csv("JEC-A-2012")[2:6]
  JECModel2 <- read.csv("JEC-E-2012")[2:6]
  
  JECeGRIDLoc <- c(39.2865, -96.1172)
  
  MaxDay1 <- which.max(MRSmeasure$`Jeffrey Energy Center`)
  
  PlotModel1 <- subset(JECModel1, Day == MaxDay1)
  PlotModel2 <- subset(JECModel2, Day == MaxDay1)
  
  al1 = get_map(location = c(lon = JECeGRIDLoc[2], lat = JECeGRIDLoc[1]), zoom = 07, maptype = 'satellite')
  al1MAP = ggmap(al1)
  al1MAP + geom_tile(data = PlotModel1, aes(x = Lon, y = Lat, fill = Conc)) +
    scale_fill_gradient(limits=c(min(min(PlotModel1$Conc), min(PlotModel2$Conc)), 
                                 max(max(PlotModel1$Conc), max(PlotModel2$Conc))), low = "yellow", high = "red") +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("Jeffrey Energy Center (2012): \n eGRID Model Dispersion Area")
  
  
  
  al1 = get_map(location = c(lon = JECeGRIDLoc[2], lat = JECeGRIDLoc[1]), zoom = 07, maptype = 'satellite')
  al1MAP = ggmap(al1)
  al1MAP + geom_tile(data = PlotModel2, aes(x = Lon, y = Lat, fill = Conc)) +
    scale_fill_gradient(limits=c(min(min(PlotModel1$Conc), min(PlotModel2$Conc)), 
                                 max(max(PlotModel1$Conc), max(PlotModel2$Conc))), low = "yellow", high = "red") +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("Jeffrey Energy Center (2012): \n Full Model Dispersion Area")
  
  
  
  ##### Max Day JSC #####
  
  JSCModel1 <- read.csv("JSC-A-2012")[2:6]
  JSCModel2 <- read.csv("JSC-E-2012")[2:6]
  
  JSCeGRIDLoc <- c(36.9981, -84.5919)
  
  MaxDay2 <- which.max(MRSmeasure$`J. S. Cooper Plant`)
  
  PlotModel1 <- subset(JSCModel1, Day == MaxDay2)
  PlotModel2 <- subset(JSCModel2, Day == MaxDay2)
  
  al1 = get_map(location = c(lon = JSCeGRIDLoc[2], lat = JSCeGRIDLoc[1]), zoom = 06, maptype = 'satellite')
  al1MAP = ggmap(al1)
  al1MAP + geom_tile(data = PlotModel1, aes(x = Lon, y = Lat, fill = Conc)) +
    scale_fill_gradient(limits=c(min(min(PlotModel1$Conc), min(PlotModel2$Conc)), 
                                 max(max(PlotModel1$Conc), max(PlotModel2$Conc))), low = "yellow", high = "red") +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("J. S. Cooper (2012): \n eGRID Model Dispersion Area")
  
  
  
  al1 = get_map(location = c(lon = JSCeGRIDLoc[2], lat = JSCeGRIDLoc[1]), zoom = 06, maptype = 'satellite')
  al1MAP = ggmap(al1)
  al1MAP + geom_tile(data = PlotModel2, aes(x = Lon, y = Lat, fill = Conc)) +
    scale_fill_gradient(limits=c(min(min(PlotModel1$Conc), min(PlotModel2$Conc)), 
                                 max(max(PlotModel1$Conc), max(PlotModel2$Conc))), low = "yellow", high = "red") +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("J. S. Cooper (2012): \n Full Model Dispersion Area")
  
  
  
  ##### Max Day for TCG #####
  
  TCGModel1 <- read.csv("TCG-A-2012")[2:6]
  TCGModel2 <- read.csv("TCG-E-2012")[2:6]
  
  TCGeGRIDLoc <- c(46.7559, -122.8598)
  
  MaxDay3 <- which.max(MRSmeasure[3])
  
  PlotModel1 <- subset(TCGModel1, Day == MaxDay3)
  PlotModel2 <- subset(TCGModel2, Day == MaxDay3)
  
  al1 = get_map(location = c(lon = TCGeGRIDLoc[2], lat = TCGeGRIDLoc[1]), zoom = 05, maptype = 'satellite')
  al1MAP = ggmap(al1)
  al1MAP + geom_tile(data = PlotModel1, aes(x = Lon, y = Lat, fill = Conc)) +
    scale_fill_gradient(limits=c(min(min(PlotModel1$Conc), min(PlotModel2$Conc)), 
                                 max(max(PlotModel1$Conc), max(PlotModel2$Conc))), low = "yellow", high = "red") +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("TransAlta Centralia Generation (2012): \n eGRID Model Dispersion Area")
  
  
  
  al1 = get_map(location = c(lon = TCGeGRIDLoc[2], lat = TCGeGRIDLoc[1]), zoom = 05, maptype = 'satellite')
  al1MAP = ggmap(al1)
  al1MAP + geom_tile(data = PlotModel2, aes(x = Lon, y = Lat, fill = Conc)) +
    scale_fill_gradient(limits=c(min(min(PlotModel1$Conc), min(PlotModel2$Conc)), 
                                 max(max(PlotModel1$Conc), max(PlotModel2$Conc))), low = "yellow", high = "red") +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("TransAlta Centralia Generation (2012): \n Full Model Dispersion Area")
    
    
    
    
    
    

  
  
  
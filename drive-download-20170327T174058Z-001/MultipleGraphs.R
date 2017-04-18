library(ggplot2)
library(reshape2)
library(ggmap)

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

  
  
  
  
  
  
  
  
  
  
  Model1 <- read.csv("JEC-A-2012")[2:6]
  Model2 <- read.csv("JEC-E-2012")[2:6]
  JECeGRIDLoc <- c(39.2865, -96.1172)
  
  MaxDay1 <- 285
  
  PlotModel1 <- subset(Model1, Day == MaxDay1)
  PlotModel2 <- subset(Model2, Day == MaxDay1)
  
  al1 = get_map(location = c(lon = JECeGRIDLoc[2], lat = JECeGRIDLoc[1]), zoom = 05, maptype = 'satellite')
  al1MAP = ggmap(al1)
  
  ggplot() +
    stat_contour(geom = "polygon", data = PlotModel1, aes(x = Lon, y = Lat, z = Conc))
    #scale_fill_gradient(limits=c(min(min(PlotModel1$Conc), min(PlotModel2$Conc)), 
                                 #max(max(PlotModel1$Conc), max(PlotModel2$Conc))), low = "yellow", high = "red") +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("Jeffrey Energy Center (2012): \n eGRID Model Dispersion Area")
  
  
    
    
    
    
    ggplot(data = PlotModel1, aes(x = Lon, y = Lat)) +
      geom_point(aes(color = Conc), shape = "") +
      stat_density2d(aes(fill = ..level..), n = 100, geom="polygon", contour = TRUE) +
      guides(fill = FALSE) +
      theme_bw()
      
      
    
    
    
    
    
    
    
    
    
    
    
  
  al1 = get_map(location = c(lon = JECeGRIDLoc[2], lat = JECeGRIDLoc[1]), zoom = 05, maptype = 'satellite')
  al1MAP = ggmap(al1)
  al1MAP + geom_tile(data = PlotModel2, aes(x = Lon, y = Lat, fill = Conc)) +
    scale_fill_gradient(limits=c(min(min(PlotModel1$Conc), min(PlotModel2$Conc)), 
                                 max(max(PlotModel1$Conc), max(PlotModel2$Conc))), low = "yellow", high = "red") +
    xlab("Longitude") +
    ylab("Latitude") +
    ggtitle("Jeffrey Energy Center (2012): \n Full Model Dispersion Area")
  
  
  
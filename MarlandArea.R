library(reshape2)
library(ggplot2)
library(ggmap)

Model1 <- read.csv("JEC-A-2012")[2:6]
Model2 <- read.csv("JEC-E-2012")[2:6]

StackLoc <- rbind(                               #Insert c(LAT,LON); more can be added.
  c(39.28684, -96.11821),
  c(39.28681, -96.11721),
  c(39.28681, -96.11618)
)

eGRIDLoc <- c(39.2865, -96.1172)

StartDay <- 2
EndDay <- 366

R <- 0.1

Metric = NULL

for (i in StartDay:EndDay) {
  
  DayModel1 <- subset(Model1, Day == i)
  DayModel2 <- subset(Model2, Day == i)
  
  x_range <- max(max(DayModel1$Lon), max(DayModel2$Lon)) - min(min(DayModel1$Lon), min(DayModel2$Lon)) + 1
  y_range <- max(max(DayModel1$Lat), max(DayModel2$Lat)) - min(min(DayModel1$Lat), min(DayModel2$Lat)) + 1
  
  x_steps <- round(x_range/R)
  y_steps <- round(y_range/R)
  
  matrix1 <- matrix(0, nrow = y_steps, ncol = x_steps)
  matrix2 <- matrix(0, nrow = y_steps, ncol = x_steps)
  
  for (j in 1:y_steps) {
    
    for (k in 1:x_steps) {
      
      val1 <- mean(DayModel1$Conc[DayModel1$Lon >= min(DayModel1$Lon) + R*(k-1) &
                                    DayModel1$Lon < min(DayModel1$Lon) + R*k &
                                    DayModel1$Lat >= min(DayModel1$Lat) + R*(j-1) &
                                    DayModel1$Lat < min(DayModel1$Lat) + R*j])
      
      matrix1[j,k] <- ifelse(is.nan(val1), 0, val1)
    }
    
  }
  
  
  for (j in 1:y_steps) {
    
    for (k in 1:x_steps) {
      
      val2 <- mean(DayModel2$Conc[DayModel2$Lon >= min(DayModel2$Lon) + R*(k-1) &
                                    DayModel2$Lon < min(DayModel2$Lon) + R*k &
                                    DayModel2$Lat >= min(DayModel2$Lat) + R*(j-1) &
                                    DayModel2$Lat < min(DayModel2$Lat) + R*j])
      
      matrix2[j,k] <- ifelse(is.nan(val2), 0, val2)
    }
    
  }
  
  Metric[i] <- 0.5*(1.2321e+12/1.2592e+10)*sum(matrix2 - matrix1)
  
}


Metric <- data.frame(c(1:366), Metric)
colnames(Metric)[1] <- "Day"

ggplot(data = Metric, aes(x = Day, y = Metric)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ylim(-0.0005, 0.0005) +
  theme_bw()

###########

MaxDay <- which.max(Metric$Metric)

PlotModel1 <- subset(Model1, Day == MaxDay)
PlotModel2 <- subset(Model2, Day == MaxDay)

al1 = get_map(location = c(lon = eGRIDLoc[2], lat = eGRIDLoc[1]), zoom = 05, maptype = 'satellite')
al1MAP = ggmap(al1)
al1MAP + geom_tile(data = PlotModel1, aes(x = Lon, y = Lat, fill = Conc)) +
  scale_fill_gradient(limits=c(min(min(PlotModel1$Conc), min(PlotModel2$Conc)), 
                               max(max(PlotModel1$Conc), max(PlotModel2$Conc))), low = "yellow", high = "red") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Jeffrey Energy Center (2012): \n eGRID Model Dispersion Area")



al1 = get_map(location = c(lon = eGRIDLoc[2], lat = eGRIDLoc[1]), zoom = 05, maptype = 'satellite')
al1MAP = ggmap(al1)
al1MAP + geom_tile(data = PlotModel2, aes(x = Lon, y = Lat, fill = Conc)) +
  scale_fill_gradient(limits=c(min(min(PlotModel1$Conc), min(PlotModel2$Conc)), 
                               max(max(PlotModel1$Conc), max(PlotModel2$Conc))), low = "yellow", high = "red") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Jeffrey Energy Center (2012): \n Full Model Dispersion Area")

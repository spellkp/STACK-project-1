#####################################
##### Initialize Parameters #########
#####################################
IfPar <- 0            #1='Yes'; 0='No'#Multicore Capabilities
FreeCores <- 1        #Number of cores available during computation

StackLoc <- rbind(                               #Insert c(LAT,LON); more can be added.
  c(39.28684, -96.11821),
  c(39.28681, -96.11721),
  c(39.28681, -96.11618)
)

eGRIDLoc <- c(39.2865, -96.1172)

Model1 <- "JEC-A-2012"    #Name of dataset for Model 1; MUST BE CSV
Model2 <- "JEC-E-2012"    #Name of dataset for Model 2; MUST BE CSV

#####################################
##### Initialize All Libraries ######
#####################################
library(ggplot2)
library(dplyr)
library(geosphere)
library(foreach)
library(doParallel)
library(ggmap)
library(akima)
library(fields)
#####################################
#####################################

#####################################
##### Initialize Parallel Cores #####
#####################################
if (IfPar == 1)
{cl <- detectCores()-FreeCores
registerDoParallel(cl)
} else {print("Serial Computation")}
#####################################

#############################################
##### Area Calculation and Comparison   #####
#############################################

##### Model 1 (Single Stack) ########

Model1 <- read.csv(Model1)[2:6]

DispArea1 = 
  foreach(i=2:366, .combine = rbind) %dopar% {
    
    tempModel1 <- subset(Model1, Model1$Day == i)
    
    s <- interp(x = tempModel1$Lon, y = tempModel1$Lat, z = tempModel1$Conc)
    new_s <- s$z*0
    new_s <- new_s+1
    sum(new_s, na.rm = TRUE)*12321
  }




Model2 <- read.csv(Model2)[2:6]

DispArea2 = 
  foreach(i=2:366, .combine = rbind) %dopar% {
    
    tempModel2 <- subset(Model2, Model2$Day == i)
    
    s <- interp(x = tempModel2$Lon, y = tempModel2$Lat, z = tempModel2$Conc)
    new_s <- s$z*0
    new_s <- new_s+1
    sum(new_s, na.rm = TRUE)*12321
  }

PercentDifference <- data.frame(c(2:366), 100*(DispArea2 - DispArea1)/mean(c(DispArea1, DispArea2)))
colnames(PercentDifference)[1] <- "Day"
colnames(PercentDifference)[2] <- "PercentDifference"

ggplot(data = PercentDifference, aes(x = Day, y = PercentDifference)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw()






##### Plot Models for Max Day #####


MaxDay <- PercentDifference$Day[PercentDifference$PercentDifference == max(PercentDifference$PercentDifference)]

PlotModel1 <- subset(Model1, Day == MaxDay-1)
PlotModel2 <- subset(Model2, Day == MaxDay-1)

al1 = get_map(location = c(lon = eGRIDLoc[2], lat = eGRIDLoc[1]), zoom = 07, maptype = 'satellite')
al1MAP = ggmap(al1)
al1MAP + geom_tile(data = PlotModel1, aes(x = Lon, y = Lat, fill = Conc)) +
  scale_fill_gradient(limits=c(min(min(PlotModel1$Conc), min(PlotModel2$Conc)), 
                               max(max(PlotModel1$Conc), max(PlotModel2$Conc))), low = "yellow", high = "red") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Jeffrey Energy Center (2012): \n eGRID Model Dispersion Area")



al1 = get_map(location = c(lon = eGRIDLoc[2], lat = eGRIDLoc[1]), zoom = 07, maptype = 'satellite')
al1MAP = ggmap(al1)
al1MAP + geom_tile(data = PlotModel2, aes(x = Lon, y = Lat, fill = Conc)) +
  scale_fill_gradient(limits=c(min(min(PlotModel1$Conc), min(PlotModel2$Conc)), 
                               max(max(PlotModel1$Conc), max(PlotModel2$Conc))), low = "yellow", high = "red") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Jeffrey Energy Center (2012): \n Full Model Dispersion Area")

PercentDifference[MaxDay-1,]

setwd('~/ResearchScripts/STACK-project')

#####################################
##### Initialize Parameters #########
#####################################
IfPar <- 0            #1='Yes'; 0='No'#Multicore Capabilities
FreeCores <- 1        #Number of cores available during computation

  StackLoc <- rbind(                               #Insert c(LAT,LON); more can be added.
    c(39.28684, -96.11821),
    c(39.28681, -96.11721),
    c(39.28681, -96.11618))
  
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
##### Analysis of Maximum Concentration #####
#############################################

Model1 <- read.csv(Model1)[2:6]
Model2 <- read.csv(Model2)[2:6]


Comp1PercentDifference = NULL
Comp2PercentDifference = NULL
Comp3PercentDifference = NULL
                                    
  for (i in 2:366) {                
    #Max Concentration Comparison
    tempModel1 <- subset(Model1, Model1$Day == i)
    tempModel2 <- subset(Model2, Model2$Day == i)
    
    Comp1PercentDifference[i] <- (max(tempModel2$Conc) - max(tempModel1$Conc))/mean(c(max(tempModel1$Conc), max(tempModel2$Conc)))
  }
   

for (i in 2:366) { 
    #Weighted Center Comparison
    tempModel1 <- subset(Model1, Model1$Day == i)
    tempModel2 <- subset(Model2, Model2$Day == i)
  
    AvgLat1 <- sum((tempModel1$Lat-mean(StackLoc[,1]))*tempModel1$Conc)/sum(tempModel1$Conc)
    AvgLon1 <- sum((tempModel1$Lon-mean(StackLoc[,2]))*tempModel1$Conc)/sum(tempModel1$Conc)
    CentModel1 <- sqrt(AvgLat1^2 + AvgLon1^2)
    
    AvgLat2 <- sum((tempModel2$Lat-mean(StackLoc[,1]))*tempModel2$Conc)/sum(tempModel2$Conc)
    AvgLon2 <- sum((tempModel2$Lon-mean(StackLoc[,2]))*tempModel2$Conc)/sum(tempModel2$Conc)
    CentModel2 <- sqrt(AvgLat2^2 + AvgLon2^2)
    
    Comp2PercentDifference[i] <- (CentModel2 - CentModel1)/mean(c(CentModel2, CentModel1))
}




##### Area Comparison #####
DispArea1 = 
  foreach(i=2:366, .combine = rbind) %dopar% {
    
    tempModel1 <- subset(Model1, Model1$Day == i)
    
    s <- interp(x = tempModel1$Lon, y = tempModel1$Lat, z = tempModel1$Conc)
    new_s <- s$z*0
    new_s <- new_s+1
    sum(new_s, na.rm = TRUE)*12321
  }

DispArea1 <- as.data.frame(DispArea1)



DispArea2 = 
  foreach(i=2:366, .combine = rbind) %dopar% {
    
    tempModel2 <- subset(Model2, Model2$Day == i)
    
    s <- interp(x = tempModel2$Lon, y = tempModel2$Lat, z = tempModel2$Conc)
    new_s <- s$z*0
    new_s <- new_s+1
    sum(new_s, na.rm = TRUE)*12321
  }

DispArea2 <- as.data.frame(DispArea2)

Comp3PercentDifference <- (DispArea2 - DispArea1)/mean(c(DispArea2$V1,DispArea1$V1))

##### Build the dataframe with all percent differences in it #####

PercentDifferenceData <- data.frame(c(2:366), 100*Comp1PercentDifference[2:366], 100*Comp2PercentDifference[2:366], 100*Comp3PercentDifference$V1,
                                    100*sqrt(Comp1PercentDifference[2:366]^2 + Comp2PercentDifference[2:366]^2 + Comp3PercentDifference$V1^2))

names(PercentDifferenceData) <- c("Day", "Metric 1", "Metric 2", "Metric 3", "Overall")



##### Plots #####
ggplot(data = PercentDifferenceData, aes(x = Day, y = Overall)) +
  geom_point() +
  xlab("Day") +
  ylab("Percent Difference (%)") +
  geom_smooth(se = FALSE) +
  theme_bw()

##########

MaxDay <- PercentDifferenceData$Day[PercentDifferenceData$Overall == max(PercentDifferenceData$Overall)]-1

PlotModel1 <- subset(Model1, Day == MaxDay)
PlotModel2 <- subset(Model2, Day == MaxDay)

al1 = get_map(location = c(lon = eGRIDLoc[2], lat = eGRIDLoc[1]), zoom = 06, maptype = 'satellite')
al1MAP = ggmap(al1)
al1MAP + geom_tile(data = PlotModel1, aes(x = Lon, y = Lat, fill = Conc)) +
  scale_fill_gradient(limits=c(min(min(PlotModel1$Conc), min(PlotModel2$Conc)), 
                               max(max(PlotModel1$Conc), max(PlotModel2$Conc))), low = "yellow", high = "red") +
  xlab("Longitude") +
  ylab("Latitude") 

##########

al1 = get_map(location = c(lon = eGRIDLoc[2], lat = eGRIDLoc[1]), zoom = 06, maptype = 'satellite')
al1MAP = ggmap(al1)
al1MAP + geom_tile(data = PlotModel2, aes(x = Lon, y = Lat, fill = Conc)) +
  scale_fill_gradient(limits=c(min(min(PlotModel1$Conc), min(PlotModel2$Conc)), 
                               max(max(PlotModel1$Conc), max(PlotModel2$Conc))), low = "yellow", high = "red") +
  xlab("Longitude") +
  ylab("Latitude") 
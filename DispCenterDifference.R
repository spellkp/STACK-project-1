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

Model1 <- "A-2012"    #Name of dataset for Model 1; MUST BE CSV
Model2 <- "E-2012"    #Name of dataset for Model 2; MUST BE CSV

#####################################
##### Initialize All Libraries ######
#####################################
library(ggplot2)
library(dplyr)
library(geosphere)
library(foreach)
library(doParallel)
library(ggmap)
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
Model1 <- read.csv(Model1)[2:6]

Mod1 =
   foreach(i=2:366, .combine = rbind) %dopar% {
   tempModel1 <- subset(Model1, Model1$Day == i)
   AvgLat <- mean(tempModel1$Lat)-eGRIDLoc[1]
   AvgLon <- mean(tempModel1$Lon)-eGRIDLoc[2]
   cbind(AvgLat, AvgLon)
}
Mod1 <- as.data.frame(Mod1)

Model2 <- read.csv(Model2)[2:6]

Mod2 = foreach(i=2:366, .combine = rbind) %dopar% {
  tempModel2 <- subset(Model2, Model2$Day == i)
  AvgLat <- mean(tempModel2$Lat)-mean(StackLoc[,1])
  AvgLon <- mean(tempModel2$Lon)-mean(StackLoc[,2])
  cbind(AvgLat, AvgLon)
}
Mod2 <- as.data.frame(Mod2)

##### Unweighted Magnitude and Angle Difference (vector) #####

MagnitudeDifference <- sqrt((Mod1$AvgLat-Mod2$AvgLat)^2+(Mod1$AvgLon-Mod2$AvgLon)^2)*111
x <- ((Mod1$AvgLat*Mod2$AvgLat) + (Mod1$AvgLon*Mod2$AvgLon)) / (sqrt((Mod1$AvgLat)^2 + (Mod1$AvgLon)^2)*sqrt((Mod2$AvgLat)^2 + (Mod2$AvgLon)^2))
AngleDifference <- (180/3.14159)*acos(x)

plot(MagnitudeDifference)
plot(AngleDifference)

mean(MagnitudeDifference)
mean(AngleDifference)

#######################################################
###### Weighted Model #################################
#######################################################

WMod1 =
  foreach(i=2:366, .combine = rbind) %dopar% {
    tempModel1 <- subset(Model1, Model1$Day == i)
    AvgLat <- sum((tempModel1$Lat-eGRIDLoc[1])*tempModel1$Conc)/sum(tempModel1$Conc)
    AvgLon <- sum((tempModel1$Lon-eGRIDLoc[2])*tempModel1$Conc)/sum(tempModel1$Conc)
    cbind(AvgLat, AvgLon)
  }
WMod1 <- as.data.frame(WMod1)


WMod2 =
  foreach(i=2:366, .combine = rbind) %dopar% {
    tempModel2 <- subset(Model2, Model2$Day == i)
    AvgLat <- sum((tempModel2$Lat-eGRIDLoc[1])*tempModel2$Conc)/sum(tempModel2$Conc)
    AvgLon <- sum((tempModel2$Lon-eGRIDLoc[2])*tempModel2$Conc)/sum(tempModel2$Conc)
    cbind(AvgLat, AvgLon)
  }
WMod2 <- as.data.frame(WMod2)

WMagnitudeDifference <- sqrt((WMod1$AvgLat-WMod2$AvgLat)^2+(WMod1$AvgLon-WMod2$AvgLon)^2)*111
x <- ((WMod1$AvgLat*WMod2$AvgLat) + (WMod1$AvgLon*WMod2$AvgLon)) / (sqrt((WMod1$AvgLat)^2 + (WMod1$AvgLon)^2)*sqrt((WMod2$AvgLat)^2 + (WMod2$AvgLon)^2))
WAngleDifference <- (180/3.14159)*acos(x)

plot(WMagnitudeDifference)
plot(WAngleDifference)

mean(WMagnitudeDifference)
mean(WAngleDifference)
